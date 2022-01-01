use std::error::Error;
use std::fmt::{Display, format, Formatter, Write};
use crate::error::Span;
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
                '=' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Eq(c.0))
                },
                '(' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Dot) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::OpenParen(c.0))
                },
                ')' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::ClosedParen(c.0))
                },
                ' ' => Some(Token::None),
                ',' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Op) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Comma(c.0))
                },
                '*' | '×' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(c.0, Multiply))
                },
                '/' | ':' | '÷' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(c.0, Divide))
                },
                '%' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(c.0, Modulo))
                },
                '^' => {
                    if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Op | TokenKind::Dot | TokenKind::Eq | TokenKind::Sign) {
                        return Err(LexError::new(format!("`{}` at wrong location.", x)))
                    }
                    Some(Token::Op(c.0, Pow))
                },
                '+' => {
                    if let Some(token) = token_type.take() {
                        tokens.push(token);
                    }
                    if !tokens.is_empty() {
                        if matches!(tokens.last().unwrap().kind(), TokenKind::Dot | TokenKind::Sign) {
                            return Err(LexError::new(format!("`{}` at wrong location.", x)))
                        }
                        match &tokens.last().unwrap() {
                            Token::Op(_, _) => {
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
                        if matches!(tokens.last().unwrap().kind(), TokenKind::Dot | TokenKind::Sign) {
                            return Err(LexError::new(format!("`{}` at wrong location.", x)))
                        }
                        match &tokens.last().unwrap() {
                            Token::Op(sp, _) => {
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
                                    return Err(LexError::new("`.` at wrong location.".to_string()))
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
                    } else if x.is_alphabetic() {
                        match &mut token_type {
                            None => {
                                if !tokens.is_empty() && matches!(tokens.last().unwrap().kind(), TokenKind::Sign | TokenKind::Dot) {
                                    return Err(LexError::new(format!("`{:?}` at wrong location.", tokens.last().unwrap().kind().to_string())))
                                }
                                token_type = Some(Token::Literal(Span::new(c.0, c.0), String::from(x)));
                            },
                            Some(token) => {
                                match token {
                                    Token::Literal(_, buffer) => buffer.push(x),
                                    _ => {
                                        tokens.push(token_type.take().unwrap());
                                        token_type = Some(Token::Literal(Span::new(0, 0), String::from(x)));
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
                    Token::Literal(_, _) => {}
                    Token::Number(_, _) => {}
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