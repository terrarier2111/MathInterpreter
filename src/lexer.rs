use crate::diagnostic_builder;
use crate::error::DiagnosticBuilder;
use crate::shared::BinOpKind::{Divide, Modulo, Multiply, Pow};
use crate::shared::Token::EOF;
use crate::shared::{
    BinOpKind, LiteralKind, LiteralToken, SignKind, Token, TokenKind, TrailingSpace,
};
use crate::span::{GenericSpan, Span};

pub(crate) struct Lexer();

impl Lexer {
    #[inline]
    pub fn new() -> Self {
        Self()
    }

    pub fn lex(&self, input: String) -> Result<Vec<Token>, DiagnosticBuilder> {
        let mut tokens: Vec<Token> = vec![];
        let mut token_type = None;
        let chars = input.chars().collect::<Vec<_>>();
        for i in 0..chars.len() {
            // SAFETY: This is safe because we only to to char's length - 1
            // and chars can't be updated after we got it's length
            let x = *unsafe { chars.get_unchecked(i) };
            if !x.is_alphabetic() && !x.is_numeric() && x != '.' {
                if let Some(token) = token_type.take() {
                    // Check if the literal ends with a dot
                    if let Token::Literal(lit_tok) = &token {
                        if lit_tok.kind == LiteralKind::Number && lit_tok.content.ends_with('.') {
                            return diagnostic_builder!(
                                input.clone(),
                                "`.` at wrong location",
                                lit_tok.span.end() - 1
                            );
                        }
                    }
                    tokens.push(token);
                }
            }
            let token = match x {
                '|' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap(),
                            Token::VertBar(..) | Token::BinOp(_, BinOpKind::Eq) | Token::Sign(..)
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    Some(Token::VertBar(i))
                }
                '=' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap(),
                            Token::VertBar(..) | Token::BinOp(_, BinOpKind::Eq) | Token::Sign(..)
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    Some(Token::BinOp(i, BinOpKind::Eq))
                }
                '(' => {
                    if !tokens.is_empty()
                        && matches!(tokens.last().unwrap().kind(), TokenKind::VertBar)
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    Some(Token::OpenParen(i))
                }
                ')' => {
                    let last = tokens.last().unwrap();
                    if !tokens.is_empty()
                        && matches!(last, Token::VertBar(..) | Token::BinOp(_, BinOpKind::Eq))
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    if !tokens.is_empty() && matches!(last, Token::Sign(..)) {
                        unimplemented!("Signs in front of `(` are currently unsupported")
                    }
                    Some(Token::ClosedParen(i))
                }
                ' ' => Some(Token::None),
                ',' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::BinOp
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    Some(Token::Comma(i))
                }
                '*' | '×' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::BinOp | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    Some(Token::BinOp(i, Multiply))
                }
                '/' | ':' | '÷' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::BinOp | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location.", x),
                            i
                        );
                    }
                    Some(Token::BinOp(i, Divide))
                }
                '%' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::BinOp | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    Some(Token::BinOp(i, Modulo))
                }
                '^' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::VertBar | TokenKind::BinOp | TokenKind::Sign
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", x),
                            i
                        );
                    }
                    Some(Token::BinOp(i, Pow))
                }
                '+' | '-' | '−' => {
                    let sign = match x {
                        '-' | '−' => (SignKind::Minus, BinOpKind::Subtract),
                        _ => (SignKind::Plus, BinOpKind::Add),
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
                                i
                            );
                        }
                        match &tokens.last().unwrap() {
                            Token::BinOp(_, _) | Token::OpenParen(_) => {
                                Some(Token::Sign(i, sign.0))
                            }
                            _ => Some(Token::BinOp(i, sign.1)),
                        }
                    } else {
                        // This has to be an Op instead of a Sign because of abs modes.
                        Some(Token::BinOp(i, sign.1))
                    }
                }
                _ => {
                    if x.is_numeric() || x == '.' || x.is_alphabetic() || x == '_' || x == '#' {
                        let alphabetic = x.is_alphabetic() || x == '_' || x == '#';
                        match &mut token_type {
                            None => {
                                // This handles the case in which the last token **is not** part of the current token's literal
                                if x == '.' {
                                    return diagnostic_builder!(
                                        input.clone(),
                                        "`.` at wrong location",
                                        i
                                    );
                                }
                                let sign = if !tokens.is_empty() {
                                    if let Token::Sign(idx, kind) = tokens.last().unwrap().clone() {
                                        tokens.pop();
                                        (idx, kind)
                                    } else {
                                        (i, SignKind::Default)
                                    }
                                } else {
                                    (i, SignKind::Default)
                                };
                                token_type = Some(Token::Literal(LiteralToken {
                                    span: Span::single_token(sign.0),
                                    content: String::from(x),
                                    sign: sign.1,
                                    kind: if alphabetic {
                                        LiteralKind::CharSeq
                                    } else {
                                        LiteralKind::Number
                                    },
                                    trailing_space: TrailingSpace::from(
                                        chars.get(i + 1).map_or(false, |next| *next == ' '),
                                    ),
                                }));
                            }
                            Some(token) => {
                                // This handles the case in which the last token **is** part of the current token's literal
                                match token {
                                    Token::Literal(lit_tok) => {
                                        if alphabetic && lit_tok.kind != LiteralKind::CharSeq {
                                            lit_tok.kind = LiteralKind::CharSeq;
                                        }
                                        // Check for previous dots
                                        if lit_tok.kind == LiteralKind::Number
                                            && x == '.'
                                            && lit_tok.content.contains('.')
                                        {
                                            return diagnostic_builder!(
                                                input.clone(),
                                                "`.` at wrong location",
                                                i
                                            );
                                        }
                                        lit_tok.content.push(x);
                                        lit_tok.span.expand_hi();
                                    }
                                    _ => unreachable!(),
                                }
                            }
                        }
                    } else {
                        tokens.push(Token::Other(i, x));
                    }
                    None
                }
            };
            if let Some(token) = token {
                match &token {
                    Token::Literal(..) => {}
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
        tokens.push(EOF(chars.len()));
        Ok(tokens)
    }
}
