use crate::error::{DiagnosticBuilder, Span};
use crate::shared::{LiteralKind, Number, OpKind, SignKind, Token, TokenKind, TokenStream};
use crate::{_lib, parser, shared, ANSMode, Config, DiagnosticsConfig, Mode};
use rust_decimal::prelude::One;
use std::collections::HashMap;
use std::ops::Neg;

// const SIMPLIFICATION_PASSES: [Box<dyn SimplificationPass>; 2] = [Box::new(ConstOpSimplificationPass {}), Box::new(NoopSimplificationPass {})];

// Unfortunately we can't perform certain simplifications if we can't prove that they are mathematically correct, like for example:
// 0^x | This can't be simplified because we would need to prove that x != 0
// x^0 | This can't be simplified because we would need to prove that x != 0
// 0/x | This can't be simplified because we would need to prove that x != 0
pub fn simplify(input: String, tokens: Vec<Token>) -> Result<Vec<Token>, DiagnosticBuilder> {
    let mut stream = TokenStream::new(input, tokens);
    let simplification_passes: [Box<dyn SimplificationPass>; 4] = [
        Box::new(ConstOpSimplificationPass {}),
        Box::new(NoopSimplificationPass {}),
        Box::new(SingleBraceSimplificationPass {}),
        Box::new(MultiplicationSequenceSimplificationPass {}),
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

struct ConstOpSimplificationPass {}

impl PrioritizedSimplificationPass for ConstOpSimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                match op_kind {
                    OpKind::Pow => {
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
                                    Token::Literal(
                                        Span::NONE,
                                        result.to_string(),
                                        SignKind::Default,
                                        LiteralKind::Number,
                                    ),
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
            if let Token::Op(_, op_kind) = token.clone() {
                match op_kind {
                    OpKind::Multiply | OpKind::Modulo | OpKind::Divide => {
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
                                    || (prev_op_kind.unwrap() != OpKind::Multiply
                                        && prev_op_kind.unwrap() != OpKind::Modulo
                                        && prev_op_kind.unwrap() != OpKind::Divide)
                                {
                                    // Apply simplification
                                    let result = op_kind.eval(args);
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // Remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // Remove right hand argument
                                    token_stream.inner_tokens_mut().remove(idx - 1); // Remove left hand argument
                                    token_stream.inner_tokens_mut().insert(
                                        idx - 1,
                                        Token::Literal(
                                            Span::NONE,
                                            result.to_string(),
                                            SignKind::Default,
                                            LiteralKind::Number,
                                        ),
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
            if let Token::Op(_, op_kind) = token.clone() {
                match op_kind {
                    OpKind::Plus | OpKind::Minus => {
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
                                    || (prev_op_kind.unwrap() != OpKind::Multiply
                                        && prev_op_kind.unwrap() != OpKind::Modulo
                                        && prev_op_kind.unwrap() != OpKind::Divide)
                                {
                                    // Apply simplification
                                    let result = op_kind.eval(args);
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // Remove Op
                                    token_stream.inner_tokens_mut().remove(idx); // Remove right hand argument
                                    token_stream.inner_tokens_mut().remove(idx - 1); // Remove left hand argument
                                    token_stream.inner_tokens_mut().insert(
                                        idx - 1,
                                        Token::Literal(
                                            Span::NONE,
                                            result.to_string(),
                                            SignKind::Default,
                                            LiteralKind::Number,
                                        ),
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

struct NoopSimplificationPass {}

impl PrioritizedSimplificationPass for NoopSimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        // TODO: Expand this logic a bit to simplify more things!
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    OpKind::Pow => {
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
            if let Token::Op(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    OpKind::Divide => {
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
                    OpKind::Multiply => {
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
            if let Token::Op(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    OpKind::Plus => {
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
                    OpKind::Minus => {
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

struct SingleBraceSimplificationPass {}

impl SimplificationPass for SingleBraceSimplificationPass {
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

struct MultiplicationSequenceSimplificationPass {}

impl SimplificationPass for MultiplicationSequenceSimplificationPass {
    fn simplify(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        fn apply_results(offset: usize, multiplications: &Vec<Token>, stream: &mut TokenStream) {
            for _ in 0..(multiplications.len() * 2) {
                stream.remove_token(offset);
            }
            let mut num_part = Number::ONE;
            let mut var_mults = HashMap::new();
            for token in multiplications.iter() {
                if let Token::Literal(_, lit, sign, kind) = token {
                    match kind {
                        LiteralKind::Number => {
                            let mut num = lit.parse::<Number>().unwrap();
                            if sign == &SignKind::Minus {
                                num = num.neg();
                            }
                            num_part = num_part * num;
                        }
                        LiteralKind::CharSeq => {
                            if sign == &SignKind::Minus {
                                num_part = num_part.neg();
                            }
                            if !var_mults.contains_key(lit) {
                                var_mults.insert(lit.clone(), 1_usize);
                            } else {
                                *var_mults.get_mut(lit).unwrap() += 1;
                            }
                        }
                    }
                }
            }
            let mut add_offset = 0;
            if !num_part.is_one() {
                stream
                    .inner_tokens_mut()
                    .insert(offset, Token::Op(usize::MAX, OpKind::Multiply));
                let sign = match num_part.to_string().chars().next().unwrap() {
                    '-' => SignKind::Minus,
                    _ => SignKind::Default,
                };
                let mut buff = num_part.normalize().to_string();
                if sign == SignKind::Minus {
                    buff.remove(0);
                }
                stream.inner_tokens_mut().insert(
                    offset + 1,
                    Token::Literal(Span::NONE, buff, sign, LiteralKind::Number),
                );
                add_offset += 2;
            }
            // order the multiplications alphabetically
            let mut mults = var_mults.into_iter().collect::<Vec<(String, usize)>>();
            mults.sort_by(|x, y| x.0.cmp(&y.0));

            // insert the results
            for mult in mults {
                if mult.1 == 1 {
                    stream
                        .inner_tokens_mut()
                        .insert(offset + add_offset, Token::Op(usize::MAX, OpKind::Multiply));
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 1,
                        Token::Literal(Span::NONE, mult.0, SignKind::Default, LiteralKind::CharSeq),
                    );
                    add_offset += 2;
                } else {
                    // This adds "*(offset.0 ^ offset.1)"
                    stream
                        .inner_tokens_mut()
                        .insert(offset + add_offset, Token::Op(usize::MAX, OpKind::Multiply));
                    stream
                        .inner_tokens_mut()
                        .insert(offset + add_offset + 1, Token::OpenParen(usize::MAX));
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 2,
                        Token::Literal(Span::NONE, mult.0, SignKind::Default, LiteralKind::CharSeq),
                    );
                    stream
                        .inner_tokens_mut()
                        .insert(offset + add_offset + 3, Token::Op(usize::MAX, OpKind::Pow));
                    stream.inner_tokens_mut().insert(
                        offset + add_offset + 4,
                        Token::Literal(
                            Span::NONE,
                            mult.1.to_string(),
                            SignKind::Default,
                            LiteralKind::Number,
                        ),
                    );
                    stream
                        .inner_tokens_mut()
                        .insert(offset + add_offset + 5, Token::ClosedParen(usize::MAX));
                    add_offset += 6;
                }
            }
        }

        let mut offset = 0;
        let mut multiplications = vec![];
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, kind) = token {
                if kind == &OpKind::Multiply {
                    if let Some(token) = token_stream.look_ahead() {
                        if TokenKind::Literal == token.kind() {
                            if multiplications.is_empty() {
                                offset = token_stream.inner_idx();
                            }
                            multiplications.push(token.clone());
                            continue;
                        }
                    }
                } else {
                    if !multiplications.is_empty() {
                        apply_results(offset, &multiplications, token_stream);
                        multiplications.clear();
                    }
                }
            } else if token.kind() != TokenKind::Literal {
                // FIXME: Is this correct?
                if !multiplications.is_empty() {
                    apply_results(offset, &multiplications, token_stream);
                    multiplications.clear();
                }
            }
        }
        if !multiplications.is_empty() {
            apply_results(offset, &multiplications, token_stream);
        }
        Ok(())
    }
}

pub(crate) fn remove_token_or_braced_region(
    input: String, // FIXME: Can we just pass this by reference here?
    tokens: &mut Vec<Token>,
    idx: usize,
) -> Result<(), DiagnosticBuilder> {
    let region = parser::parse_braced_call_immediately(&input, tokens, idx);
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
    let region = parser::parse_braced_call_immediately(&input, tokens, idx);
    let result = if let Some(region) = region {
        region?.as_range().start
    } else {
        idx
    };
    Ok(result)
}

fn prev_op_kind(token_stream: &TokenStream) -> Result<Option<OpKind>, DiagnosticBuilder> {
    let offset = idx_token_or_braced_region(
        token_stream.input.clone(),
        token_stream.inner_tokens(),
        token_stream.inner_idx() - 1,
    )?;
    if offset > 0 {
        if let Token::Op(_, kind) = token_stream.inner_tokens().get(offset - 1).unwrap() {
            return Ok(Some(*kind));
        }
    }
    Ok(None)
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
    _lib::eval(
        String::from("3*25*a*42*13*b*53*a*5*b*b*a*31*b"),
        &mut context,
    )
    .unwrap();
    assert_eq!(context.parse_ctx.get_input(), "336404250*(a^3)*(b^4)");
}
