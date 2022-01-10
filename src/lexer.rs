use crate::diagnostic_builder;
use crate::error::{DiagnosticBuilder, Span};
use crate::shared::OpKind::{Divide, Minus, Modulo, Multiply, Plus, Pow};
use crate::shared::{SignKind, Token, TokenKind};

pub(crate) struct Lexer {



}

impl Lexer {

    pub fn new() -> Self {
        Self {

        }
    }

    pub fn lex(&self, input: String) -> Result<Vec<Token>, DiagnosticBuilder> {
        let mut tokens: Vec<Token> = vec![];
        let mut token_type = None;
        for c in input.chars().enumerate() {
            let x = c.1;
            if !x.is_alphabetic() && !x.is_numeric() && x != '.' {
                if let Some(token) = token_type.take() {
                    tokens.push(token);
                }
            }
            let token = match x {
                /*'.' => { // TODO: Improve dot validation!
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Dot)
                },*/
                '|' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::VertBar(c.0))
                },
                '=' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::Eq(c.0))
                },
                '(' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | /**/TokenKind::Dot) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::OpenParen(c.0))
                },
                ')' => {
                    let last = tokens.last().unwrap().kind();
                    if !tokens.is_empty() && matches!(last, TokenKind::VertBar | TokenKind::Dot | TokenKind::Eq) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    if !tokens.is_empty() && matches!(last, TokenKind::Sign) {
                        unimplemented!("Signs in front of `(` are currently unsupported!")
                    }
                    Some(Token::ClosedParen(c.0))
                },
                ' ' => Some(Token::None),
                ',' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Op) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::Comma(c.0))
                },
                '*' | '×' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::Op(c.0, Multiply))
                },
                '/' | ':' | '÷' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::Op(c.0, Divide))
                },
                '%' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::Op(c.0, Modulo))
                },
                '^' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                    }
                    Some(Token::Op(c.0, Pow))
                },
                '+' => {
                    if let Some(token) = token_type.take() {
                        tokens.push(token);
                    }
                    if !tokens.is_empty() {
                        if matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Dot | TokenKind::Sign) {
                            return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                        }
                        match &tokens.last().unwrap() {
                            Token::Op(_, _) | Token::OpenParen(_) => {
                                Some(Token::Sign(c.0, SignKind::Plus))
                            }
                            _ => {
                                Some(Token::Op(c.0, Plus))
                            }
                        }
                    } else {
                        Some(Token::Sign(c.0, SignKind::Plus))
                    }
                },
                '-' | '−' => {
                    if let Some(token) = token_type.take() {
                        tokens.push(token);
                    }
                    if !tokens.is_empty() {
                        if matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Dot | TokenKind::Sign) {
                            return diagnostic_builder!(input.clone(), format!("`{}` at wrong location.", x), c.0);
                        }
                        match &tokens.last().unwrap() {
                            Token::Op(sp, _) | Token::OpenParen(sp) => {
                                Some(Token::Sign(*sp, SignKind::Minus))
                            }
                            _ => {
                                Some(Token::Op(0, Minus))
                            }
                        }
                    } else {
                        Some(Token::Sign(c.0, SignKind::Minus))
                    }
                },
                _ => {
                    if x.is_numeric() || x == '.' {
                        match &mut token_type {
                            None => {
                                if x == '.' {
                                    return diagnostic_builder!(input.clone(), "`.` at wrong location.", c.0);
                                }
                                let mut num = String::from(x);
                                if !tokens.is_empty() {
                                    let mut has_sign = false;
                                    if let Token::Sign(_, sign) = tokens.last().unwrap() {
                                        has_sign = true;
                                        match sign {
                                            SignKind::Plus => {}
                                            SignKind::Minus => num.insert(0, '-'),
                                        }
                                    }
                                    if has_sign {
                                        tokens.pop();
                                    }
                                }
                                token_type = Some(Token::Number(Span::new(0, 0), num));
                            },
                            Some(token) => {
                                match token {
                                    Token::Number(_, buffer) => buffer.push(x),
                                    _ => {
                                        tokens.push(token_type.take().unwrap());
                                        token_type = Some(Token::Number(Span::new(0, 0), String::from(x)))
                                    },
                                }
                            },
                        }
                    } else if x.is_alphabetic() || x == '_' || x == '#' {
                        match &mut token_type {
                            None => {
                                if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar | TokenKind::Dot) {
                                    return diagnostic_builder!(input.clone(), format!("`{:?}` at wrong location.", tokens.last().unwrap().kind().to_string()), c.0);
                                }
                                let sign = if let Token::Sign(idx, kind) = tokens.last().unwrap().clone() {
                                    (idx, Some(kind))
                                } else {
                                    (c.0, None)
                                };
                                token_type = Some(Token::Literal(Span::new(sign.0, c.0), String::from(x), sign.1));
                            },
                            Some(token) => {
                                match token {
                                    Token::Literal(_, buffer, _) => buffer.push(x),
                                    _ => {
                                        tokens.push(token_type.take().unwrap());
                                        token_type = Some(Token::Literal(Span::new(0, 0), String::from(x), todo!()));
                                    },
                                }
                            },
                        }
                    } else {
                        tokens.push(Token::Other(c.0, x));
                    }
                    None
                },
            };
            if let Some(token) = token {
                match &token {
                    Token::Literal(..) => {}
                    Token::Number(..) => {}
                    Token::None => { // TODO: Remove this and make whitespace return None option and make literals/numbers return their respective token type! (how should we handle invalid/other tokens?)
                        if let Some(token) = token_type.take() {
                            tokens.push(token);
                        }
                    },
                    _ => {
                        if let Some(token) = token_type.take() {
                            tokens.push(token);
                        }
                        tokens.push(token);
                    },
                }
            }
        }
        if let Some(token) = token_type.take() {
            tokens.push(token);
        }
        Ok(tokens)
    }

}