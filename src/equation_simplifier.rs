use crate::error::{DiagnosticBuilder, Span};
use crate::shared::{LiteralKind, OpKind, SignKind, Token, TokenStream};
use crate::{_lib, parser, shared, ANSMode, Config, DiagnosticsConfig, Mode};
use rust_decimal::prelude::One;

// const SIMPLIFICATION_PASSES: [Box<dyn SimplificationPass>; 2] = [Box::new(ConstOpSimplificationPass {}), Box::new(NoopSimplificationPass {})];

// Unfortunately we can't perform certain simplifications if we can't prove that they are mathematically correct, like for example:
// 0^x | This can't be simplified because we would need to prove that x != 0
// x^0 | This can't be simplified because we would need to prove that x != 0
// 0/x | This can't be simplified because we would need to prove that x != 0
pub fn simplify(input: String, tokens: Vec<Token>) -> Result<Vec<Token>, DiagnosticBuilder> {
    let mut stream = TokenStream::new(input, tokens);
    let simplification_passes: [Box<dyn SimplificationPass>; 3] = [
        Box::new(ConstOpSimplificationPass {}),
        Box::new(NoopSimplificationPass {}),
        Box::new(SingleBraceSimplificationPass {}),
    ];
    for s_pass in simplification_passes {
        s_pass.start_simplify(&mut stream)?;
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
                                token_stream.inner_tokens_mut().remove(idx); // Remove Op
                                token_stream.inner_tokens_mut().remove(idx); // Remove exp
                                token_stream.inner_tokens_mut().remove(idx - 1); // Remove base
                                token_stream.inner_tokens_mut().insert(
                                    idx - 1,
                                    Token::Literal(
                                        Span::NONE,
                                        result.to_string(),
                                        SignKind::Plus,
                                        LiteralKind::Number,
                                    ),
                                );
                                token_stream.go_back();
                            }
                        }
                    }
                    OpKind::OpenParen => {}
                }
            }
        }
        Ok(())
    }

    fn simplify2(&self, token_stream: &mut TokenStream) -> Result<(), DiagnosticBuilder> {
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                match op_kind {
                    OpKind::Plus => {}
                    OpKind::Minus => {}
                    OpKind::Multiply | OpKind::Modulo | OpKind::Divide => {
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
                                token_stream.inner_tokens_mut().remove(idx); // Remove right hand argument
                                token_stream.inner_tokens_mut().remove(idx - 1); // Remove left hand argument
                                token_stream.inner_tokens_mut().insert(
                                    idx - 1,
                                    Token::Literal(
                                        Span::NONE,
                                        result.to_string(),
                                        SignKind::Plus,
                                        LiteralKind::Number,
                                    ),
                                );
                                token_stream.go_back();
                            }
                        }
                    }
                    OpKind::Pow => {}
                    OpKind::OpenParen => {}
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
                                        SignKind::Plus,
                                        LiteralKind::Number,
                                    ),
                                );
                                token_stream.go_back();
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
                    OpKind::Plus => {}
                    OpKind::Minus => {}
                    OpKind::Divide => {}
                    OpKind::Multiply => {}
                    OpKind::Modulo => {}
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
                    OpKind::OpenParen => {}
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
                    OpKind::Plus => {}
                    OpKind::Minus => {}
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
                    OpKind::Modulo => {}
                    OpKind::Pow => {}
                    OpKind::OpenParen => {}
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
                    OpKind::Divide => {}
                    OpKind::Multiply => {}
                    OpKind::Modulo => {}
                    OpKind::Pow => {}
                    OpKind::OpenParen => {}
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
        while let Some(token) = token_stream.next() {
            if let Token::OpenParen(_) = token {
                open_braces.push(token_stream.inner_idx());
            } else if let Token::ClosedParen(_) = token {
                let current = token_stream.inner_idx();
                let last = open_braces.pop().unwrap();
                let diff = current - last;
                if diff == 2 {
                    token_stream.inner_tokens_mut().remove(current);
                    token_stream.inner_tokens_mut().remove(current - 2);
                    token_stream.go_back();
                    token_stream.go_back();
                }
            }
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
    // _lib::eval(String::from("0/(5*3+4)"), &mut context).unwrap(); // TODO: Fix this
    // assert_eq!(context.parse_ctx.get_input(), "0");
}
