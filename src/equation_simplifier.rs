use crate::error::Span;
use crate::shared;
use crate::shared::{LiteralKind, OpKind, SignKind, Token, TokenStream};
use rust_decimal::prelude::One;
use rust_decimal::Decimal;

// const SIMPLIFICATION_PASSES: [Box<dyn SimplificationPass>; 2] = [Box::new(ConstOpSimplificationPass {}), Box::new(NoopSimplificationPass {})];

pub fn simplify(tokens: Vec<Token>) -> Vec<Token> {
    let mut stream = TokenStream::new(tokens);
    let simplification_passes: [Box<dyn SimplificationPass>; 2] = [
        Box::new(ConstOpSimplificationPass {}),
        Box::new(NoopSimplificationPass {}),
    ];
    for s_pass in simplification_passes {
        s_pass.start_simplify(&mut stream);
    }
    stream.to_tokens()
}

trait SimplificationPass {
    #[inline]
    fn start_simplify(&self, token_stream: &mut TokenStream) {
        self.simplify(token_stream);
        token_stream.reset();
    }

    fn simplify(&self, token_stream: &mut TokenStream);
}

trait PrioritizedSimplificationPass: SimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream);

    fn simplify2(&self, token_stream: &mut TokenStream);

    fn simplify1(&self, token_stream: &mut TokenStream);
}

impl<T: PrioritizedSimplificationPass> SimplificationPass for T {
    fn simplify(&self, token_stream: &mut TokenStream) {
        self.simplify3(token_stream);
        token_stream.reset();
        self.simplify2(token_stream);
        token_stream.reset();
        self.simplify1(token_stream);
    }
}

struct ConstOpSimplificationPass {}

impl PrioritizedSimplificationPass for ConstOpSimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream) {
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                match op_kind {
                    OpKind::Plus => {}
                    OpKind::Minus => {}
                    OpKind::Divide => {}
                    OpKind::Multiply => {}
                    OpKind::Modulo => {}
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
                                token_stream.inner_tokens_mut().remove(idx);
                                if op_kind.args().has_right() {
                                    token_stream.inner_tokens_mut().remove(idx);
                                }
                                if op_kind.args().has_left() {
                                    token_stream.inner_tokens_mut().remove(idx - 1);
                                }
                                token_stream.inner_tokens_mut().insert(
                                    idx - 1,
                                    Token::Literal(
                                        Span::NONE,
                                        result.to_string(),
                                        SignKind::Plus,
                                        LiteralKind::Number,
                                    ),
                                );
                            }
                        }
                    }
                    OpKind::OpenParen => {}
                }
            }
        }
    }

    fn simplify2(&self, token_stream: &mut TokenStream) {
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                match op_kind {
                    OpKind::Plus => {}
                    OpKind::Minus => {}
                    OpKind::Divide => {}
                    OpKind::Multiply | OpKind::Modulo => {
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
                                token_stream.inner_tokens_mut().remove(idx);
                                if op_kind.args().has_right() {
                                    token_stream.inner_tokens_mut().remove(idx);
                                }
                                if op_kind.args().has_left() {
                                    token_stream.inner_tokens_mut().remove(idx - 1);
                                }
                                token_stream.inner_tokens_mut().insert(
                                    idx - 1,
                                    Token::Literal(
                                        Span::NONE,
                                        result.to_string(),
                                        SignKind::Plus,
                                        LiteralKind::Number,
                                    ),
                                );
                            }
                        }
                    }
                    OpKind::Pow => {}
                    OpKind::OpenParen => {}
                }
            }
        }
    }

    fn simplify1(&self, token_stream: &mut TokenStream) {
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
                                let result = op_kind.eval(args);
                                let idx = token_stream.inner_idx();
                                token_stream.inner_tokens_mut().remove(idx);
                                if op_kind.args().has_right() {
                                    token_stream.inner_tokens_mut().remove(idx);
                                }
                                if op_kind.args().has_left() {
                                    token_stream.inner_tokens_mut().remove(idx - 1);
                                }
                                token_stream.inner_tokens_mut().insert(
                                    idx - 1,
                                    Token::Literal(
                                        Span::NONE,
                                        result.to_string(),
                                        SignKind::Plus,
                                        LiteralKind::Number,
                                    ),
                                );
                            }
                        }
                    }
                    OpKind::Divide => {}
                    OpKind::Multiply => {}
                    OpKind::Modulo => {}
                    OpKind::Pow => {}
                    OpKind::OpenParen => {}
                }
            }
        }
    }
}

struct NoopSimplificationPass {}

impl PrioritizedSimplificationPass for NoopSimplificationPass {
    fn simplify3(&self, token_stream: &mut TokenStream) {
        // TODO: Expand this logic a bit to simplify more things!
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    OpKind::Plus => {}
                    OpKind::Minus => {}
                    OpKind::Divide => {}
                    OpKind::Multiply => {}
                    OpKind::Modulo => {}
                    OpKind::Pow => {
                        if let Some(token) = args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx);
                                    // remove 0
                                }
                            }
                        }
                    }
                    OpKind::OpenParen => {}
                }
            }
        }
    }

    fn simplify2(&self, token_stream: &mut TokenStream) {
        // TODO: Expand this logic a bit to simplify more things!
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                let args = op_kind.resolve_num_args(token_stream);
                match op_kind {
                    OpKind::Plus => {}
                    OpKind::Minus => {}
                    OpKind::Divide => {
                        if let Some(token) = args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx);
                                    // remove 0
                                }
                            }
                        }
                    }
                    OpKind::Multiply => {
                        if let Some(token) = args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_one() {
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx);
                                    // remove 0
                                }
                            }
                        }
                    }
                    OpKind::Modulo => {}
                    OpKind::Pow => {}
                    OpKind::OpenParen => {}
                }
            }
        }
    }

    fn simplify1(&self, token_stream: &mut TokenStream) {
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
                                    println!("remove 0");
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx - 1); // remove 0
                                    continue;
                                }
                            }
                        }
                        if let Some(token) = &args.1 {
                            let num = shared::token_to_num(&token);
                            if let Some(num) = num {
                                if num.is_zero() {
                                    println!("remove 0");
                                    let idx = token_stream.inner_idx();
                                    token_stream.inner_tokens_mut().remove(idx); // remove Op
                                    token_stream.inner_tokens_mut().remove(idx);
                                    // remove 0
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
                                    token_stream.inner_tokens_mut().remove(idx);
                                    // remove 0
                                }
                            }
                        }
                    }
                    OpKind::Divide => {}
                    OpKind::Multiply => {}
                    OpKind::Modulo => {}
                    OpKind::Pow => {}
                    OpKind::OpenParen => {}
                }
            }
        }
    }
}
