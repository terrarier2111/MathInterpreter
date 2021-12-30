use std::error::Error;
use std::fmt::{Display, format, Formatter, Write};
use crate::shared::OpKind::{Divide, Minus, Modulo, Multiply, Plus, Pow};
use crate::shared::{SignKind, Token, TokenKind};

pub(crate) struct Lexer {



}

impl Lexer {

    pub fn new() -> Self {
        Self {

        }
    }

    pub fn lex(&self, input: String) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = vec![];
        let mut token_type = None;
        for x in input.chars() {
            if !x.is_alphabetic() && !x.is_numeric() && x != '.' {
                if let Some(token) = token_type.take() {
                    tokens.push(token);
                }
            }
            let token = match x {
                /*'.' => { // TODO: Improve dot validation!
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Dot)
                },*/
                '=' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Eq)
                },
                '(' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Dot) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::OpenParen)
                },
                ')' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::ClosedParen)
                },
                ' ' => Some(Token::None),
                ',' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Op) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Comma)
                },
                '*' | '×' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(Multiply))
                },
                '/' | ':' | '÷' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(Divide))
                },
                '%' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(Modulo))
                },
                '^' => {
                    if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(Pow))
                },
                '+' => {
                    if let Some(token) = token_type.take() {
                        tokens.push(token);
                    }
                    if !tokens.is_empty() {
                        if matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Dot | TokenKind::Sign) {
                            return Err(LexError::new(format!("`{}` at wrong location.", x)))
                        }
                        match &tokens.get(tokens.len() - 1).unwrap() {
                            Token::Op(_) => {
                                Some(Token::Sign(SignKind::Plus))
                            }
                            _ => {
                                Some(Token::Op(Plus))
                            }
                        }
                    } else {
                        Some(Token::Sign(SignKind::Plus))
                    }
                },
                '-' | '−' => {
                    if let Some(token) = token_type.take() {
                        tokens.push(token);
                    }
                    if !tokens.is_empty() {
                        if matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Dot | TokenKind::Sign) {
                            return Err(LexError::new(format!("`{}` at wrong location.", x)))
                        }
                        match &tokens.get(tokens.len() - 1).unwrap() {
                            Token::Op(_) => {
                                Some(Token::Sign(SignKind::Minus))
                            }
                            _ => {
                                Some(Token::Op(Minus))
                            }
                        }
                    } else {
                        Some(Token::Sign(SignKind::Minus))
                    }
                },
                _ => {
                    if x.is_numeric() || x == '.' {
                        match &mut token_type {
                            None => {
                                if x == '.' {
                                    return Err(LexError::new("`.` at wrong location.".to_string()))
                                }
                                let mut num = String::from(x);
                                if !tokens.is_empty() {
                                    let mut has_sign = false;
                                    if let Token::Sign(sign) = tokens.get(tokens.len() - 1).unwrap() {
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
                                token_type = Some(Token::Number(num));
                            },
                            Some(token) => {
                                match token {
                                    Token::Number(buffer) => buffer.push(x),
                                    _ => {
                                        tokens.push(token_type.take().unwrap());
                                        token_type = Some(Token::Number(String::from(x)))
                                    },
                                }
                            },
                        }
                    } else if x.is_alphabetic() {
                        match &mut token_type {
                            None => {
                                if !tokens.is_empty() && matches!(tokens.get(tokens.len() - 1).unwrap().kind(), TokenKind::Sign | TokenKind::Dot) {
                                    return Err(LexError::new(format!("`{:?}` at wrong location.", tokens.get(tokens.len() - 1).unwrap().kind().to_string())))
                                }
                                token_type = Some(Token::Literal(String::from(x)));
                            },
                            Some(token) => {
                                match token {
                                    Token::Literal(buffer) => buffer.push(x),
                                    _ => {
                                        tokens.push(token_type.take().unwrap());
                                        token_type = Some(Token::Literal(String::from(x)));
                                    },
                                }
                            },
                        }
                    } else {
                        tokens.push(Token::Other(x));
                    }
                    None
                },
            };
            if let Some(token) = token {
                match &token {
                    Token::Literal(_) => {}
                    Token::Number(_) => {}
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

#[derive(Debug)]
pub struct LexError(String);

impl LexError {

    pub fn new(msg: String) -> Self {
        Self(msg)
    }

}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
    }
}

impl Error for LexError {}