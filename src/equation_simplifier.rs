use crate::error::Span;
use crate::shared;
use crate::shared::{LiteralKind, OpKind, SignKind, Token, TokenStream};

pub fn simplify(tokens: Vec<Token>) -> Vec<Token> {
    let mut stream = TokenStream::new(tokens);

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

struct ConstOpSimplificationPass {}

impl SimplificationPass for ConstOpSimplificationPass {
    fn simplify(&self, token_stream: &mut TokenStream) {
        while let Some(token) = token_stream.next() {
            if let Token::Op(_, op_kind) = token.clone() {
                if op_kind != OpKind::Divide {
                    // TODO: MAYBE: Support (named) constant simplification for things like PI or E
                    let args = op_kind.resolve_num_args(token_stream);
                    if op_kind.is_valid(&args) {
                        let args = (args.0.and_then(|token| shared::token_to_num(&token)),
                                    args.1.and_then(|token| shared::token_to_num(&token)));
                        let result = op_kind.eval(args);
                        let idx = token_stream.inner_idx();
                        token_stream.inner_tokens_mut().remove(idx);
                        if op_kind.args().has_right() {
                            token_stream.inner_tokens_mut().remove(idx);
                        }
                        if op_kind.args().has_left() {
                            token_stream.inner_tokens_mut().remove(idx - 1);
                        }
                        token_stream.inner_tokens_mut().insert(idx - 1, Token::Literal(Span::NONE, result.to_string(), SignKind::Plus, LiteralKind::Number));
                    }
                }
            }
        }
    }
}
