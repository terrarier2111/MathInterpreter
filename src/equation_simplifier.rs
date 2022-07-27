use crate::error::DiagnosticBuilder;
use crate::shared::{
    BinOpKind, LiteralKind, LiteralToken, Number, SignKind, Token, TokenKind, TokenStream,
    TrailingSpace,
};
use crate::span::{FixedTokenSpan, GenericSpan, Span};
use crate::{_lib, diagnostic_builder, parser, shared, ANSMode, Config, DiagnosticsConfig, Mode};
use rust_decimal::prelude::One;
use std::collections::HashMap;
use std::ops::{Neg, Range};

// const SIMPLIFICATION_PASSES: [Box<dyn SimplificationPass>; 2] = [Box::new(ConstOpSimplificationPass {}), Box::new(NoopSimplificationPass {})];

// Unfortunately we can't perform certain simplifications if we can't prove that they are mathematically correct, like for example:
// 0^x | This can't be simplified because we would need to prove that x != 0
// x^0 | This can't be simplified because we would need to prove that x != 0
// 0/x | This can't be simplified because we would need to prove that x != 0
// FIXME: take the new format of AstNodes instead of a Vec of Tokens if that helps - it might not help because of brace shenanigans, although that could be sorted out through precedence
pub fn simplify(input: String, tokens: Vec<Token>) -> Result<Vec<Token>, DiagnosticBuilder> {
    let mut stream = TokenStream::new(input, tokens);
    let simplification_passes: [Box<dyn SimplificationPass>; 5] = [
        Box::new(ConstOpSimplificationPass {}),
        Box::new(NoopSimplificationPass {}),
        Box::new(BraceSimplificationPass {}),
        Box::new(OpSequenceSimplificationPass {}),
        Box::new(AddSubSequenceSimplificationPass {}),
    ];
    for _ in 0..simplification_passes.len() {
        for s_pass in simplification_passes.iter() {
            s_pass.start_simplify(&mut stream)?;
        }
    }
    Ok(stream.to_tokens())
}

trait SimplificationPass {
    #[inline]
    fn start_simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        let result = self.simplify(token_stream);
        token_stream.reset();
        result
    }

    fn simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder>;
}

trait PrioritizedSimplificationPass: SimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder>;

    fn simplify2(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder>;

    fn simplify1(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder>;
}

impl<T: PrioritizedSimplificationPass> SimplificationPass for T {
    fn simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        self.simplify3(token_stream)?;
        token_stream.reset();
        self.simplify2(token_stream)?;
        token_stream.reset();
        self.simplify1(token_stream)
    }
}

// simplifies const ops to their outcome
// example: `2*5+20` becomes `30`
struct ConstOpSimplificationPass {}

impl PrioritizedSimplificationPass for ConstOpSimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, op_kind) = token.clone() {
                match op_kind {
                    BinOpKind::Pow => {
                        // TODO: MAYBE: Support (named) constant simplification for things like PI or E
                        let args = op_kind.resolve_num_args(token_stream);
                        if op_kind.is_valid(&args) {
                            let args = (
                                args.0.and_then(|token| shared::token_to_num(&token)),
                                args.1.and_then(|token| shared::token_to_num(&token)),
                            );
                            if op_kind.is_valid(&args) {
                                let result = op_kind.eval(args);
                                let idx = token_stream.inner_idx();
                                token_stream.inner_tokens_mut().remove(idx); // Remove Op
                                token_stream.inner_tokens_mut().remove(idx); // Remove exp
                                token_stream.inner_tokens_mut().remove(idx - 1); // Remove base
                                token_stream.inner_tokens_mut().insert(
                                    idx - 1,
                                    Token::Literal(LiteralToken {
                                        span: Span::NONE,
                                        content: result.to_string(),
                                        kind: LiteralKind::Number,
                                        trailing_space: TrailingSpace::Maybe,
                                    }),
                                );
                                token_stream.go_back();
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn simplify2(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, op_kind) = token.clone() {
                match op_kind {
                    BinOpKind::Multiply | BinOpKind::Modulo | BinOpKind::Divide => {
                        // TODO: MAYBE: Support (named) constant simplification for things like PI or E
                        let args = op_kind.resolve_num_args(token_stream);
                        if op_kind.is_valid(&args) {
                            let args = (
                                args.0.and_then(|token| shared::token_to_num(&token)),
                                args.1.and_then(|token| shared::token_to_num(&token)),
                            );
                            if op_kind.is_valid(&args) {
                                // Check for previous op to prevent incorrect simplifications
                                let prev_op_kind = prev_op_kind(token_stream)?;
                                if prev_op_kind.is_none()
                                    || (prev_op_kind.unwrap() != BinOpKind::Multiply
                                        && prev_op_kind.unwrap() != BinOpKind::Modulo
                                        && prev_op_kind.unwrap() != BinOpKind::Divide)
                                {
                                    // Apply simplification
                                    let result = op_kind.eval(args);
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // Remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // Remove right hand argument
                                    token_stream.inner_tokens_mut().remove(idx - 1); // Remove left hand argument
                                    token_stream.inner_tokens_mut().insert(
                                        idx - 1,
                                        Token::Literal(LiteralToken {
                                            span: Span::NONE,
                                            content: result.to_string(),
                                            kind: LiteralKind::Number,
                                            trailing_space: TrailingSpace::Maybe,
                                        }),
                                    );
                                    token_stream.go_back();
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn simplify1(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, op_kind) = token.clone() {
                match op_kind {
                    BinOpKind::Add | BinOpKind::Subtract => {
                        // TODO: MAYBE: Support (named) constant simplification for things like PI or E
                        let args = op_kind.resolve_num_args(token_stream);
                        if op_kind.is_valid(&args) {
                            let args = (
                                args.0.and_then(|token| shared::token_to_num(&token)),
                                args.1.and_then(|token| shared::token_to_num(&token)),
                            );
                            if op_kind.is_valid(&args) {
                                // Check for previous op to prevent incorrect simplifications
                                let prev_op_kind = prev_op_kind(token_stream)?;
                                if prev_op_kind.is_none()
                                    || (prev_op_kind.unwrap() != BinOpKind::Multiply
                                        && prev_op_kind.unwrap() != BinOpKind::Modulo
                                        && prev_op_kind.unwrap() != BinOpKind::Divide)
                                {
                                    // Apply simplification
                                    let result = op_kind.eval(args);
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // Remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // Remove right hand argument
                                    token_stream.inner_tokens_mut().remove(idx - 1); // Remove left hand argument
                                    token_stream.inner_tokens_mut().insert(
                                        idx - 1,
                                        Token::Literal(LiteralToken {
                                            span: Span::NONE,
                                            content: result.to_string(),
                                            kind: LiteralKind::Number,
                                            trailing_space: TrailingSpace::Maybe,
                                        }),
                                    );
                                    token_stream.go_back();
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }
}

// simplifies NOOPs away
// example: `3^1+0+4*1+2*0` becomes `7`
struct NoopSimplificationPass {}

impl PrioritizedSimplificationPass for NoopSimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        // TODO: Expand this logic a bit to simplify more things!
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    BinOpKind::Pow => {
                        // handle right hand argument
                        if let Some(token) = args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // remove 1
                                    token_stream.go_back();
                                    continue;
                                }
                            }
                        }
                        // handle left hand argument
                        if let Some(token) = args.0 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    remove_token_or_braced_region(
                                        token_stream.input.clone(),
                                        token_stream.inner_tokens_mut(),
                                        idx,
                                    )?; // remove exponent
                                    token_stream.go_back();
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn simplify2(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        // TODO: Expand this logic a bit to simplify more things!
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    BinOpKind::Divide => {
                        if let Some(token) = args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // remove 1
                                    token_stream.go_back();
                                    continue;
                                }
                            }
                        }
                    }
                    BinOpKind::Multiply => {
                        if let Some(token) = args.0 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx - 1); // remove 1
                                    token_stream.go_back();
                                    continue;
                                } else if num.is_zero() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    remove_token_or_braced_region(
                                        token_stream.input.clone(),
                                        token_stream.inner_tokens_mut(),
                                        idx,
                                    )?; // remove num
                                    token_stream.go_back();
                                    continue;
                                }
                            }
                        }
                        if let Some(token) = args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // remove 1
                                    token_stream.go_back();
                                    continue;
                                } else if num.is_zero() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    remove_token_or_braced_region(
                                        token_stream.input.clone(),
                                        token_stream.inner_tokens_mut(),
                                        idx - 1,
                                    )?; // remove num
                                    token_stream.go_back();
                                    continue;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn simplify1(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        // TODO: Expand this logic a bit to simplify more things!
        // TODO: Reduce code duplication!
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    BinOpKind::Add => {
                        if let Some(token) = &args.0 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_zero() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx - 1); // remove 0
                                    token_stream.go_back();
                                    continue;
                                }
                            }
                        }
                        if let Some(token) = &args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_zero() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // remove 0
                                    token_stream.go_back();
                                    continue;
                                }
                            }
                        }
                    }
                    BinOpKind::Subtract => {
                        if let Some(token) = args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_zero() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // remove 0
                                    token_stream.go_back();
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    }
}

// simplifies cases like `(TOKEN)` to `TOKEN`
// simplifies cases like `(((SOME TOKENS)))` to `(SOME TOKENS)`
struct BraceSimplificationPass {}

impl SimplificationPass for BraceSimplificationPass {
    fn simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        let mut open_braces = vec![];
        let mut finished_braces = vec![];
        while let Some(token) = token_stream.next() {
            if let Token::OpenParen(_) = token {
                open_braces.push(token_stream.inner_idx());
            } else if let Token::ClosedParen(_) = token {
                let current = token_stream.inner_idx();
                let last = open_braces.pop().unwrap();
                let diff = current - last;
                // Check if there is only a single token in between the braces and remove the braces if so
                if diff == 2 {
                    token_stream.inner_tokens_mut().remove(current);
                    token_stream.inner_tokens_mut().remove(current - 2);
                    token_stream.go_back();
                    token_stream.go_back();
                } else {
                    finished_braces.push((last, current));
                }
            }
        }
        let mut removed = vec![];
        let mut last = &(usize::MAX, usize::MAX);
        for brace in finished_braces.iter() {
            if (brace.0.max(last.0) - brace.0.min(last.0)) == 1
                && (brace.1.max(last.1) - brace.1.min(last.1)) == 1
            {
                removed.push(brace.clone());
            }
            last = brace;
        }
        while !removed.is_empty() {
            let brace = removed.remove(0);
            for prev in removed.iter_mut() {
                let mut o_diff = 0;
                let mut c_diff = 0;
                if prev.0 > brace.0 {
                    // if the `(` of the checked brace comes after the `(` of the removed one
                    o_diff += 1;
                }
                if prev.1 > brace.0 {
                    // if the `)` of the checked brace comes after the `(` of the removed one
                    c_diff += 1;
                }
                if prev.0 > brace.1 {
                    // if the `(` of the checked brace comes after the `)` of the removed one
                    o_diff += 1;
                }
                if prev.1 > brace.1 {
                    // if the `)` of the checked brace comes after the `)` of the removed one
                    c_diff += 1;
                }
                prev.0 -= o_diff;
                prev.1 -= c_diff;
            }
            token_stream.inner_tokens_mut().remove(brace.1);
            token_stream.inner_tokens_mut().remove(brace.0);
        }
        Ok(())
    }
}

// simplifies op sequences to their outcome
// example: `3*a*1*b*2*a*a*b*c*2*a*b*2*a/d/e/d/d/d/e` becomes `24*(a^5)*(b^3)*c/(d*`
struct OpSequenceSimplificationPass {}

impl SimplificationPass for OpSequenceSimplificationPass {
    fn simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        fn apply_results(
            offset: usize,
            operations: &Vec<Token>,
            op_kind: BinOpKind,
            stream: &mut TokenStream,
        ) {
            for _ in 0..(operations.len() * 2) {
                stream.remove_token(offset);
            }
            let mut num_part = Number::ONE;
            let mut var_ops = HashMap::new();
            for token in operations.iter() {
                if let Token::Literal(lit_tok) = token {
                    match lit_tok.kind {
                        LiteralKind::Number => {
                            let mut num = lit_tok.content.parse::<Number>().unwrap();
                            num_part = num_part * num;
                        }
                        LiteralKind::CharSeq => {
                            if !var_ops.contains_key(&lit_tok.content) {
                                var_ops.insert(lit_tok.content.clone(), 1_usize);
                            } else {
                                *var_ops.get_mut(&lit_tok.content).unwrap() += 1;
                            }
                        }
                    }
                }
            }
            let mut add_offset = 0;
            if !num_part.is_one() {
                stream
                    .inner_tokens_mut()
                    .insert(offset, Token::BinOp(FixedTokenSpan::none(), op_kind));
                let sign = if num_part.is_sign_negative() {
                    SignKind::Minus
                } else {
                    SignKind::Default
                };
                let mut buff = num_part.normalize().to_string();
                if sign == SignKind::Minus {
                    buff.remove(0);
                }
                stream.inner_tokens_mut().insert(
                    offset + 1,
                    Token::Literal(LiteralToken {
                        span: Span::NONE,
                        content: buff,
                        kind: LiteralKind::Number,
                        trailing_space: TrailingSpace::Maybe,
                    }),
                );
                add_offset += 2;
            }
            // order the multiplications alphabetically
            let mut ops = var_ops.into_iter().collect::<Vec<(String, usize)>>();
            ops.sort_by(|x, y| x.0.cmp(&y.0));

            // insert the results
            for op in ops {
                stream.inner_tokens_mut().insert(
                    offset + add_offset,
                    Token::BinOp(FixedTokenSpan::none(), op_kind),
                );
                if op.1 == 1 {
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 1,
                        Token::Literal(LiteralToken {
                            span: Span::NONE,
                            content: op.0,
                            kind: LiteralKind::CharSeq,
                            trailing_space: TrailingSpace::Maybe,
                        }),
                    );
                    add_offset += 2;
                } else {
                    // This adds "(offset.0 ^ offset.1)"
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 1,
                        Token::OpenParen(FixedTokenSpan::none()),
                    );
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 2,
                        Token::Literal(LiteralToken {
                            span: Span::NONE,
                            content: op.0,
                            kind: LiteralKind::CharSeq,
                            trailing_space: TrailingSpace::Maybe,
                        }),
                    );
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 3,
                        Token::BinOp(
                            FixedTokenSpan::none(),
                            match op_kind {
                                BinOpKind::Divide => BinOpKind::Multiply,
                                BinOpKind::Multiply => BinOpKind::Pow,
                                BinOpKind::Pow => BinOpKind::Multiply,
                                _ => unreachable!(),
                            },
                        ),
                    );
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 4,
                        Token::Literal(LiteralToken {
                            span: Span::NONE,
                            content: op.1.to_string(),
                            kind: LiteralKind::Number,
                            trailing_space: TrailingSpace::Maybe,
                        }),
                    );
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 5,
                        Token::ClosedParen(FixedTokenSpan::none()),
                    );
                    add_offset += 6;
                }
            }
        }

        let mut offset = 0;
        let mut last_kind = BinOpKind::Eq;
        let mut operations = vec![];
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, kind) = token {
                if kind == &last_kind {
                    if let Some(token) = token_stream.look_ahead() {
                        if TokenKind::Literal == token.kind() {
                            if operations.is_empty() {
                                offset = token_stream.inner_idx();
                            }
                            operations.push(token.clone());
                        }
                    }
                } else {
                    let kind = *kind;
                    if !operations.is_empty() {
                        apply_results(offset, &operations, last_kind, token_stream);
                        operations.clear();
                    }
                    if kind == BinOpKind::Multiply
                        || kind == BinOpKind::Divide
                        || kind == BinOpKind::Pow
                    {
                        last_kind = kind;
                        if let Some(token) = token_stream.look_ahead() {
                            if TokenKind::Literal == token.kind() {
                                if operations.is_empty() {
                                    offset = token_stream.inner_idx();
                                }
                                operations.push(token.clone());
                            }
                        }
                    }
                }
            } else if token.kind() != TokenKind::Literal {
                // FIXME: Is this correct?
                if !operations.is_empty() {
                    apply_results(offset, &operations, last_kind, token_stream);
                    operations.clear();
                }
            }
        }
        if !operations.is_empty() {
            apply_results(offset, &operations, last_kind, token_stream);
        }
        Ok(())
    }
}

// simplifies addition and subtraction sequences to their outcome
// example: `35+27+e+a+e+a+b+a+a+a+a+a+a+a+a` becomes `62+a*10+b+e*2`
struct AddSubSequenceSimplificationPass {}

impl SimplificationPass for AddSubSequenceSimplificationPass {
    fn simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        fn apply_results(
            offset: usize,
            actions: &Vec<(Token, SequentialAction)>,
            stream: &mut TokenStream,
        ) {
            for _ in 0..(actions.len() * 2) {
                stream.remove_token(offset);
            }
            let mut num_part = Number::ZERO;
            let mut var_adds = HashMap::new();
            for token in actions.iter() {
                if let Token::Literal(lit_tok) = &token.0 {
                    match lit_tok.kind {
                        LiteralKind::Number => {
                            let mut num = lit_tok.content.parse::<Number>().unwrap();
                            num_part = num_part + num;
                        }
                        LiteralKind::CharSeq => {
                            let diff = 1;
                            if !var_adds.contains_key(&lit_tok.content) {
                                var_adds.insert(lit_tok.content.clone(), diff);
                            } else {
                                *var_adds.get_mut(&lit_tok.content).unwrap() += diff;
                            }
                        }
                    }
                }
            }
            let mut add_offset = 0;
            if !num_part.is_zero() {
                if num_part.is_sign_positive() {
                    stream
                        .inner_tokens_mut()
                        .insert(offset, Token::BinOp(FixedTokenSpan::none(), BinOpKind::Add));
                } else {
                    stream.inner_tokens_mut().insert(
                        offset,
                        Token::BinOp(FixedTokenSpan::none(), BinOpKind::Subtract),
                    );
                }
                let negative = num_part.is_sign_negative();
                let mut buff = num_part.normalize().to_string();
                if negative {
                    buff.remove(0);
                }
                stream.inner_tokens_mut().insert(
                    offset + 1,
                    Token::Literal(LiteralToken {
                        span: Span::NONE,
                        content: buff,
                        kind: LiteralKind::Number,
                        trailing_space: TrailingSpace::Maybe,
                    }),
                );
                add_offset += 2;
            }
            // order the multiplications alphabetically
            let mut adds = var_adds.into_iter().collect::<Vec<(String, isize)>>();
            adds.sort_by(|x, y| x.0.cmp(&y.0));

            // insert the results
            for add in adds {
                if add.1 > 0 {
                    stream.inner_tokens_mut().insert(
                        offset + add_offset,
                        Token::BinOp(FixedTokenSpan::none(), BinOpKind::Add),
                    );
                } else {
                    stream.inner_tokens_mut().insert(
                        offset + add_offset,
                        Token::BinOp(FixedTokenSpan::none(), BinOpKind::Subtract),
                    );
                }

                if add.1 == 1 || add.1 == -1 {
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 1,
                        Token::Literal(LiteralToken {
                            span: Span::NONE,
                            content: add.0,
                            kind: LiteralKind::CharSeq,
                            trailing_space: TrailingSpace::Maybe,
                        }),
                    );
                    add_offset += 2;
                } else {
                    // This adds "(offset.0 * offset.1)"
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 1,
                        Token::Literal(LiteralToken {
                            span: Span::NONE,
                            content: add.0,
                            kind: LiteralKind::CharSeq,
                            trailing_space: TrailingSpace::Maybe,
                        }),
                    );
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 2,
                        Token::BinOp(FixedTokenSpan::none(), BinOpKind::Multiply),
                    );
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 3,
                        Token::Literal(LiteralToken {
                            span: Span::NONE,
                            content: add.1.to_string(),
                            kind: LiteralKind::Number,
                            trailing_space: TrailingSpace::Maybe,
                        }),
                    );
                    add_offset += 4;
                }
            }
        }

        let mut offset = 0;
        let mut actions = vec![];
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, kind) = token {
                if kind == &BinOpKind::Add || kind == &BinOpKind::Subtract {
                    let kind = match kind {
                        BinOpKind::Add => SequentialAction::Add,
                        BinOpKind::Subtract => SequentialAction::Subtract,
                        _ => unreachable!(),
                    };
                    if let Some(token) = token_stream.look_ahead() {
                        if TokenKind::Literal == token.kind() {
                            if actions.is_empty() {
                                offset = token_stream.inner_idx();
                            }
                            actions.push((token.clone(), kind));
                            continue;
                        }
                    }
                } else {
                    if !actions.is_empty() {
                        apply_results(offset, &actions, token_stream);
                        actions.clear();
                    }
                }
            } else if token.kind() != TokenKind::Literal {
                // FIXME: Is this correct?
                if !actions.is_empty() {
                    apply_results(offset, &actions, token_stream);
                    actions.clear();
                }
            }
        }
        if !actions.is_empty() {
            apply_results(offset, &actions, token_stream);
        }
        Ok(())
    }
}

// TODO: Simplification: Remove braces if they don't contain any operation which is lower/equal to the priority to the operations directly before and directly after the braces
struct BraceOpPrioritySimplificationPass;

impl SimplificationPass for BraceOpPrioritySimplificationPass {
    fn simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        let mut braces = vec![];
        let mut last_op = None;
        while let Some(token) = token_stream.next() {
            if let Token::BinOp(_, kind) = token {
                last_op = Some(*kind);
            } else if let Token::OpenParen(_) = token {
                braces.push((token_stream.inner_idx(), last_op));
            } else if let Token::ClosedParen(_) = token {
                let potential_next_op = token_stream.look_ahead();
            }
        }
        Ok(())
    }
}

#[repr(u8)]
enum SequentialAction {
    Add,
    Subtract,
}

pub(crate) fn remove_token_or_braced_region(
    input: String, // FIXME: Can we just pass this by reference here?
    tokens: &mut Vec<Token>,
    idx: usize,
) -> Result<(), DiagnosticBuilder> {
    let region = parse_braced_call_immediately(&input, tokens, idx);
    if let Some(region) = region {
        region?.erase(tokens);
    } else {
        tokens.remove(idx);
    }
    Ok(())
}

pub(crate) fn idx_token_or_braced_region(
    input: String, // FIXME: Can we just pass this by reference here?
    tokens: &Vec<Token>,
    idx: usize,
) -> Result<usize, DiagnosticBuilder> {
    let region = parse_braced_call_immediately(&input, tokens, idx);
    let result = if let Some(region) = region {
        region?.as_range().start
    } else {
        idx
    };
    Ok(result)
}

fn prev_op_kind(token_stream: &TokenStream) -> Result<Option<BinOpKind>, DiagnosticBuilder> {
    let offset = idx_token_or_braced_region(
        token_stream.input.clone(),
        token_stream.inner_tokens(),
        token_stream.inner_idx() - 1,
    )?;
    if offset > 0 {
        if let Token::BinOp(_, kind) = token_stream.inner_tokens().get(offset - 1).unwrap() {
            return Ok(Some(*kind));
        }
    }
    Ok(None)
}

/// Tries to parse a braced call region like `(34*63+e)`
/// it errors, when there is an unequal amount of
/// `(` and `)`
pub(crate) fn parse_braced_call_region(
    input: &String,
    tokens: &Vec<Token>,
    parse_start: usize,
) -> Result<Region, DiagnosticBuilder> {
    let mut start = usize::MAX;
    let mut end = usize::MAX;
    let mut open = 0;
    for x in tokens.iter().enumerate().skip(parse_start) {
        if let Token::OpenParen(_) = x.1 {
            if open == 0 {
                start = x.0;
            }
            open += 1;
        } else if let Token::ClosedParen(_) = x.1 {
            open -= 1;
            if open == 0 {
                end = x.0;
                break;
            }
        }
    }
    if open != 0 {
        return diagnostic_builder!(
            input.clone(),
            "The brace count during region parsing didn't match!"
        );
    }
    Ok(Region::new(start, end, tokens))
}

pub(crate) fn parse_braced_call_region_backwards(
    input: &String,
    tokens: &Vec<Token>,
    parse_start: usize,
) -> Result<Region, DiagnosticBuilder> {
    let mut start = usize::MAX;
    let mut end = usize::MAX;
    let mut closed = 0;
    for x in tokens
        .iter()
        .enumerate()
        .rev()
        .skip(tokens.len() - parse_start - 1)
    {
        if let Token::ClosedParen(_) = x.1 {
            if closed == 0 {
                start = x.0;
            }
            closed += 1;
        } else if let Token::OpenParen(_) = x.1 {
            closed -= 1;
            if closed == 0 {
                end = x.0;
                break;
            }
        }
    }
    if closed != 0 {
        return diagnostic_builder!(
            input.clone(),
            format!(
                "The brace count during region parsing didn't match! {}",
                closed
            )
        );
    }
    Ok(Region::new(end, start, tokens))
}

/// Contract: The current token has to be an OpenParen token
pub(crate) fn parse_braced_call_immediately(
    input: &String,
    tokens: &Vec<Token>,
    parse_start: usize,
) -> Option<Result<Region, DiagnosticBuilder>> {
    if let Token::OpenParen(_) = tokens.get(parse_start).unwrap() {
        Some(parse_braced_call_region(input, tokens, parse_start))
    } else if let Token::ClosedParen(_) = tokens.get(parse_start).unwrap() {
        Some(parse_braced_call_region_backwards(
            input,
            tokens,
            parse_start,
        ))
    } else {
        None
    }
}

#[derive(Debug)]
pub(crate) struct Region {
    start: usize,
    end: usize,
    inner_span: Span,
}

impl Region {
    fn new(start: usize, end: usize, tokens: &Vec<Token>) -> Self {
        let inner_start = tokens.get(start).unwrap().span().start();
        let inner_end = tokens.get(end).unwrap().span().end();
        Self {
            start,
            end,
            inner_span: Span::multi_token(inner_start, inner_end),
        }
    }

    pub(crate) fn replace_in_tokens(self, tokens: &mut Vec<Token>) {
        let mut inner_tokens = vec![];
        for _ in self.as_range() {
            inner_tokens.push(tokens.remove(self.start));
        }
        tokens.insert(self.start, self.to_token(inner_tokens));
    }

    pub(crate) fn to_token(self, tokens: Vec<Token>) -> Token {
        Token::Region(self.inner_span, tokens)
    }

    pub(crate) fn to_inner_tokens(self, tokens: &Vec<Token>) -> Vec<Token> {
        let mut inner = vec![];
        for x in self.as_range() {
            inner.push(tokens.get(x).unwrap().clone());
        }
        inner
    }

    pub(crate) fn pop_braces(&mut self, tokens: &mut Vec<Token>) {
        if let Token::OpenParen(_) = tokens.get(self.start).unwrap() {
            tokens.remove(self.start);
            self.end -= 1;
        }
        if let Token::ClosedParen(_) = tokens.get(self.end).unwrap() {
            tokens.remove(self.end);
            self.end -= 1;
        }
    }

    pub(crate) fn partition_by_comma(&mut self, tokens: &mut Vec<Token>) -> Vec<usize> {
        let braced_offset = if let Token::OpenParen(_) = tokens.get(self.start).unwrap() {
            1
        } else {
            0
        };
        let mut open = 0;
        let mut partitions = vec![];
        let mut partition_start = braced_offset;
        for x in tokens.iter().skip(self.start + braced_offset).enumerate() {
            if x.0 >= (self.end - self.start - braced_offset) {
                break;
            }
            if let Token::OpenParen(_) = x.1 {
                open += 1;
            } else if let Token::ClosedParen(_) = x.1 {
                open -= 1;
            } else if let Token::Comma(_) = x.1 {
                if open == 0 {
                    partitions.push((
                        self.start + partition_start,
                        self.start + braced_offset + x.0,
                    ));
                    partition_start = braced_offset + x.0 + 1;
                }
            }
        }
        partitions.push((self.start + partition_start, self.end));
        let mut resulting_partitions = vec![];
        let mut neg_offset = 0;
        for part in partitions {
            let mut partition = vec![];
            let partition_len = part.1 - part.0;
            let start = tokens.get(part.0 - neg_offset).unwrap().span().start();
            let end = tokens.get(part.1 - neg_offset).unwrap().span().end();
            for _ in 0..partition_len {
                partition.push(tokens.remove(part.0 - neg_offset));
            }
            tokens.insert(
                part.0 - neg_offset,
                Token::Region(Span::multi_token(start, end), partition),
            );
            self.end += 1;
            resulting_partitions.push(part.0 - neg_offset);
            neg_offset += partition_len - 1;
            self.end -= partition_len;
        }
        resulting_partitions
    }

    pub(crate) fn erase_and_provide_args(mut self, tokens: &mut Vec<Token>) -> Vec<Vec<Token>> {
        let mut result = vec![];
        let arg_indices = self.partition_by_comma(tokens);
        let args = arg_indices.len();
        for x in arg_indices.into_iter().enumerate() {
            let token = tokens.remove(x.1 - x.0);
            if let Token::Region(_, tokens) = token {
                result.push(tokens);
            } else {
                // This should never happen
                panic!("No argument found, but {:?}", token);
            }
        }
        self.end -= args;
        self.erase(tokens);
        result
    }

    pub(crate) fn erase(self, tokens: &mut Vec<Token>) {
        for _ in self.as_range() {
            tokens.remove(self.start);
        }
    }

    #[inline]
    pub(crate) fn as_range(&self) -> Range<usize> {
        self.start..(self.end + 1) // add 1 here because the token at `self.end` should be removed as well
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        (self.end + 1) - self.start
    }
}

#[test]
fn test() {
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::Never,
        Mode::Simplify,
    ));
    _lib::eval(String::from("8*4+6*0+4*3*5*0+3*0+0*3*9"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "32");
    _lib::eval(String::from("0+0*(8+3)-(8+3)*0"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "0");
    _lib::eval(String::from("x*0"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "0");
    _lib::eval(String::from("8*4+4*3*5*0+x"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "32+x");
    _lib::eval(String::from("1/4*8*x"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "2*x");
    _lib::eval(String::from("y*(4)+2"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "y*4+2");
    _lib::eval(String::from("0/(5*3+4)"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "0");
    _lib::eval(String::from("((5*3+x))"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "(15+x)");
    _lib::eval(String::from("8+((5*3+x))*2"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "8+(15+x)*2");
    _lib::eval(String::from("8+((5*3+x)*2)+(34*y)"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "8+((15+x)*2)+(34*y)");
    _lib::eval(String::from("4+(((x+4)))+((4*7+7))*3"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "4+(x+4)+105");
    _lib::eval(String::from("3*2*a*4*1*b*5*a*5*b*b*a*3*b"), &mut context).unwrap();
    assert_eq!(context.parse_ctx.get_input(), "1800*(a^3)*(b^4)");
    _lib::eval(
        String::from("35+27+e+a+e+a+b+a+a+a+a+a+a+a+a"),
        &mut context,
    );
    assert_eq!(context.parse_ctx.get_input(), "62+a*10+b+e*2");
    _lib::eval(String::from("25/a/a/b/a/b/a/b/b/4"), &mut context);
    assert_eq!(context.parse_ctx.get_input(), "6.25/(a*4)/(b*4)");
}
