use crate::diagnostic_builder;
use crate::error::{DiagnosticBuilder, Span};
use crate::shared::OpKind::{Divide, Minus, Modulo, Multiply, Plus, Pow};
use crate::shared::{OpKind, SignKind, Token, TokenKind};

pub(crate) struct Lexer {}

impl Lexer {
    pub fn new() -> Self {
        Self {}
    }

    // TODO: Improve dot validation!
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
                '|' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Eq | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    Some(Token::VertBar(c.0))
                }
                '=' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Eq | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    Some(Token::Eq(c.0))
                }
                '(' => {
                    if !tokens.is_empty()
                        && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar)
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    Some(Token::OpenParen(c.0))
                }
                ')' => {
                    let last = tokens.last().unwrap().kind();
                    if !tokens.is_empty() && matches!(last, TokenKind::VertBar | TokenKind::Eq) {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    if !tokens.is_empty() && matches!(last, TokenKind::Sign) {
                        unimplemented!("Signs in front of `(` are currently unsupported")
                    }
                    Some(Token::ClosedParen(c.0))
                }
                ' ' => Some(Token::None),
                ',' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Op
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    Some(Token::Comma(c.0))
                }
                '*' | '×' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Op | TokenKind::Eq | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    Some(Token::Op(c.0, Multiply))
                }
                '/' | ':' | '÷' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Op | TokenKind::Eq | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location.", x),
                            c.0
                        );
                    }
                    Some(Token::Op(c.0, Divide))
                }
                '%' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Op | TokenKind::Eq | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    Some(Token::Op(c.0, Modulo))
                }
                '^' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Op | TokenKind::Eq | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            c.0
                        );
                    }
                    Some(Token::Op(c.0, Pow))
                }
                '+' | '-' | '−' => {
                    let sign = match c.1 {
                        '-' | '−' => (SignKind::Minus, OpKind::Minus),
                        _ => (SignKind::Plus, OpKind::Plus),
                    };
                    if let Some(token) = token_type.take() {
                        tokens.push(token);
                    }
                    if !tokens.is_empty() {
                        if matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::Sign
                        ) {
                            return diagnostic_builder!(
                                input.clone(),
                                format!("`{}` at wrong location", x),
                                c.0
                            );
                        }
                        match &tokens.last().unwrap() {
                            Token::Op(_, _) | Token::OpenParen(_) | Token::Eq(_) => {
                                Some(Token::Sign(c.0, sign.0))
                            }
                            _ => Some(Token::Op(c.0, sign.1)),
                        }
                    } else {
                        Some(Token::Sign(c.0, sign.0))
                    }
                }
                _ => {
                    if x.is_numeric() || x == '.' {
                        match &mut token_type {
                            None => {
                                // This handles the case in which the last token **is not** part of the current token's literal
                                if x == '.' {
                                    return diagnostic_builder!(
                                        input.clone(),
                                        "`.` at wrong location",
                                        c.0
                                    );
                                }
                                let mut num = String::from(x);
                                let mut span_start = c.0;
                                if !tokens.is_empty() {
                                    if let Token::Sign(idx, sign) = tokens.last().unwrap() {
                                        let sign = *sign;
                                        span_start = *idx;
                                        tokens.pop();
                                        match sign {
                                            SignKind::Plus => {}
                                            SignKind::Minus => num.insert(0, '-'),
                                        }
                                    }
                                }
                                token_type = Some(Token::Number(Span::from_idx(span_start), num));
                            }
                            Some(token) => {
                                // This handles the case in which the last token **is** part of the current token's literal
                                match token {
                                    Token::Number(span, buffer) => {
                                        buffer.push(x);
                                        span.expand_hi();
                                    }
                                    _ => {
                                        tokens.push(token_type.take().unwrap());
                                        token_type = Some(Token::Number(
                                            Span::from_idx(c.0),
                                            String::from(x),
                                        ))
                                    }
                                }
                            }
                        }
                    } else if x.is_alphabetic() || x == '_' || x == '#' {
                        match &mut token_type {
                            None => {
                                // This handles the case in which the last token **is not** part of the current token's literal
                                let sign = if !tokens.is_empty() {
                                    if let Token::Sign(idx, kind) = tokens.last().unwrap().clone() {
                                        tokens.pop();
                                        (idx, Some(kind))
                                    } else {
                                        (c.0, None)
                                    }
                                } else {
                                    (c.0, None)
                                };

                                token_type = Some(Token::Literal(
                                    Span::from_idx(sign.0),
                                    String::from(x),
                                    sign.1,
                                ));
                            }
                            Some(token) => {
                                // This handles the case in which the last token **is** part of the current token's literal
                                match token {
                                    Token::Literal(span, buffer, _) => {
                                        buffer.push(x);
                                        span.expand_hi();
                                    }
                                    _ => {
                                        tokens.push(token_type.take().unwrap());
                                        token_type = Some(Token::Literal(
                                            Span::from_idx(c.0),
                                            String::from(x),
                                            None,
                                        ));
                                    }
                                }
                            }
                        }
                    } else {
                        tokens.push(Token::Other(c.0, x));
                    }
                    None
                }
            };
            if let Some(token) = token {
                match &token {
                    Token::Literal(..) => {}
                    Token::Number(..) => {}
                    Token::None => {
                        // TODO: Remove this and make whitespace return None option and make literals/numbers return their respective token type! (how should we handle invalid/other tokens?)
                        if let Some(token) = token_type.take() {
                            tokens.push(token);
                        }
                    }
                    _ => {
                        if let Some(token) = token_type.take() {
                            tokens.push(token);
                        }
                        tokens.push(token);
                    }
                }
            }
        }
        if let Some(token) = token_type.take() {
            tokens.push(token);
        }
        Ok(tokens)
    }
}
