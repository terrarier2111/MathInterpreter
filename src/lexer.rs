use crate::diagnostic_builder;
use crate::error::DiagnosticBuilder;
use crate::shared::BinOpKind::{Divide, Modulo, Multiply, Pow};
use crate::shared::Token::EOF;
use crate::shared::{
    BinOpKind, LiteralKind, LiteralToken, SignKind, Token, TokenKind, TrailingSpace, UnaryOpKind,
};
use crate::span::{FixedTokenSpan, GenericSpan, Span};

pub(crate) struct Lexer();

impl Lexer {
    #[inline]
    pub fn new() -> Self {
        Self()
    }

    pub fn lex(&self, input: String) -> Result<Vec<Token>, DiagnosticBuilder> {
        let mut tokens: Vec<Token> = vec![];
        let mut cursor = 0_usize;
        let mut diagnostics_builder = DiagnosticBuilder::new(input.clone());
        let chars = input.chars().collect::<Vec<_>>();
        ///
        ///
        /// args:
        ///
        /// cursor: the last processed token index
        ///
        fn read_into_buffer<F: Fn(char) -> bool>(
            input: &[char],
            cursor: usize,
            do_continue: F,
        ) -> (String, usize) {
            let mut buffer = String::new();
            let mut new_cursor = cursor/* + 1*/;
            while input.len() > new_cursor && do_continue(input[new_cursor]) {
                buffer.push(input[new_cursor]);
                new_cursor += 1;
            }
            (buffer, new_cursor)
        }
        while chars.len() > cursor {
            let mut curr_token = None;
            let curr = chars[cursor];
            match curr {
                '0'..='9' => {
                    // FIXME: add checks like we do them everywhere else!
                    let (buffer, new_cursor) =
                        read_into_buffer(&chars, cursor, |x| matches!(x, '.' | ('0'..='9')));
                    let span = Span::multi_token(cursor, new_cursor);
                    cursor = new_cursor - 1;
                    curr_token = Some(Token::Literal(LiteralToken {
                        span,
                        content: buffer,
                        kind: LiteralKind::Number,
                        trailing_space: TrailingSpace::from(
                            chars.get(cursor + 1).map_or(false, |x| *x == ' '),
                        ),
                    }));
                }
                (('a'..='z') | ('A'..='Z') | '_' | '#') => {
                    // FIXME: add checks like we do them everywhere else!
                    let (buffer, new_cursor) = read_into_buffer(&chars, cursor, |x| {
                        matches!(x, ('a'..='z') | ('A'..='Z') | ('0'..='9') | '_' | '#')
                    });
                    let span = Span::multi_token(cursor, new_cursor);
                    cursor = new_cursor - 1;
                    curr_token = Some(Token::Literal(LiteralToken {
                        span,
                        content: buffer,
                        kind: LiteralKind::CharSeq,
                        trailing_space: TrailingSpace::from(
                            chars.get(cursor + 1).map_or(false, |x| *x == ' '),
                        ),
                    }));
                }
                '!' => {
                    // FIXME: add checks like we do them everywhere else!
                    curr_token = Some(Token::UnaryOp(
                        FixedTokenSpan::new(cursor),
                        UnaryOpKind::Factorial,
                    ));
                }
                '@' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap(),
                            Token::At(..)
                                | Token::BinOp(_, BinOpKind::Eq)
                                | Token::UnaryOp(..)
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::At(FixedTokenSpan::new(cursor)));
                }
                '=' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap(),
                            Token::At(..)
                                | Token::BinOp(_, BinOpKind::Eq)
                                | Token::UnaryOp(..)
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), BinOpKind::Eq));
                }
                '(' => {
                    if !tokens.is_empty()
                        && matches!(tokens.last().unwrap().kind(), TokenKind::At)
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::OpenParen(FixedTokenSpan::new(cursor)));
                }
                ')' => {
                    let last = tokens.last().unwrap();
                    if !tokens.is_empty()
                        && matches!(last, Token::At(..) | Token::BinOp(_, BinOpKind::Eq))
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::ClosedParen(FixedTokenSpan::new(cursor)));
                }
                ' ' => {}
                ',' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::At | TokenKind::BinOp
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::Comma(FixedTokenSpan::new(cursor)));
                }
                '*' | '×' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::At | TokenKind::BinOp | TokenKind::UnaryOp
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Multiply));
                }
                '/' | ':' | '÷' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::At | TokenKind::BinOp | TokenKind::UnaryOp
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location.", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Divide));
                }
                '%' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::At | TokenKind::BinOp | TokenKind::UnaryOp
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Modulo));
                }
                '^' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::At | TokenKind::BinOp | TokenKind::UnaryOp
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Pow));
                }
                '+' => {
                    let is_unary = if let Some(tty) = tokens.last() {
                        tty.kind() == TokenKind::BinOp
                    } else {
                        true
                    };
                    if is_unary {
                        curr_token =
                            Some(Token::UnaryOp(FixedTokenSpan::new(cursor), UnaryOpKind::Pos));
                    } else {
                        curr_token =
                            Some(Token::BinOp(FixedTokenSpan::new(cursor), BinOpKind::Add));
                    }
                }
                '-' | '−' => {
                    if !tokens.is_empty()
                        && matches!(
                            tokens.last().unwrap().kind(),
                            TokenKind::At | TokenKind::UnaryOp
                        )
                    {
                        return diagnostic_builder!(
                            input.clone(),
                            format!("`{}` at wrong location", curr),
                            cursor
                        );
                    }
                    let is_unary = if let Some(tty) = tokens.last() {
                        tty.kind() == TokenKind::BinOp || tty.kind() == TokenKind::OpenParen
                    } else {
                        true
                    };
                    if is_unary {
                        curr_token = Some(Token::UnaryOp(
                            FixedTokenSpan::new(cursor),
                            UnaryOpKind::Neg,
                        ));
                    } else {
                        curr_token = Some(Token::BinOp(
                            FixedTokenSpan::new(cursor),
                            BinOpKind::Subtract,
                        ));
                    }
                }
                '¹' => {
                    curr_token = Some(Token::UnaryOp(
                        FixedTokenSpan::new(cursor),
                        UnaryOpKind::Exp(1),
                    ));
                }
                '²' => {
                    curr_token = Some(Token::UnaryOp(
                        FixedTokenSpan::new(cursor),
                        UnaryOpKind::Exp(2),
                    ));
                }
                '³' => {
                    curr_token = Some(Token::UnaryOp(
                        FixedTokenSpan::new(cursor),
                        UnaryOpKind::Exp(3),
                    ));
                }
                '⁰' | '⁴' | '⁵' | '⁶' | '⁷' | '⁸' | '⁹' => {
                    curr_token = Some(Token::UnaryOp(
                        FixedTokenSpan::new(cursor),
                        UnaryOpKind::Exp(curr as usize - '⁰' as usize),
                    ));
                }
                _ => {
                    curr_token = Some(Token::Other(FixedTokenSpan::new(cursor), curr));
                }
            }

            if let Some(token) = curr_token.take() {
                tokens.push(token);
            }
            cursor += 1;
        }
        tokens.push(EOF(FixedTokenSpan::new(chars.len())));
        Ok(tokens)
    }
}
