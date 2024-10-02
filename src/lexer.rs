use crate::error::DiagnosticBuilder;
use crate::parser::PResult;
use crate::shared::BinOpKind::{Divide, Modulo, Multiply, Pow};
use crate::shared::Token::EOF;
use crate::shared::{
    ArgPosition, BinOpKind, LiteralKind, LiteralToken, Token, TokenKind, TrailingSpace, UnaryOpKind,
};
use crate::span::{FixedTokenSpan, Span};
use crate::{diagnostic_builder, diagnostic_builder_spanned};

pub(crate) struct Lexer();

impl Lexer {
    #[inline]
    pub fn new() -> Self {
        Self()
    }

    pub fn lex(&self, input: String) -> PResult<Vec<Token>> {
        let mut tokens: Vec<Token> = vec![];
        let mut cursor = 0_usize;
        let diagnostics_builder = DiagnosticBuilder::new();
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
                ('a'..='z') | ('A'..='Z') | '_' | '#' => {
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
                    curr_token = Some(Token::UnaryOp(
                        FixedTokenSpan::new(cursor),
                        UnaryOpKind::Factorial,
                    ));
                }
                '@' => {
                    curr_token = Some(Token::At(FixedTokenSpan::new(cursor)));
                }
                '=' => {
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), BinOpKind::Eq));
                }
                '(' => {
                    curr_token = Some(Token::OpenParen(FixedTokenSpan::new(cursor)));
                }
                ')' => {
                    curr_token = Some(Token::ClosedParen(FixedTokenSpan::new(cursor)));
                }
                ' ' => {}
                ',' => {
                    curr_token = Some(Token::Comma(FixedTokenSpan::new(cursor)));
                }
                '.' => {
                    if !tokens.is_empty() {
                        if let Token::Literal(LiteralToken {
                            content,
                            trailing_space,
                            span,
                            ..
                        }) = tokens.last().unwrap()
                        {
                            if ('0'..'9').contains(&content.chars().last().unwrap())
                                && *trailing_space != TrailingSpace::Yes
                            {
                                return diagnostic_builder_spanned!(
                                    format!("`{}{}` is ambiguous", content, curr),
                                    {
                                        let new_span = *span;
                                        new_span.expand_hi()
                                    }
                                );
                            }
                        }
                    }
                    return diagnostic_builder!(format!("`{}` at wrong location", curr), cursor);
                }
                '*' | '×' => {
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Multiply));
                }
                '/' | ':' | '÷' => {
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Divide));
                }
                '%' => {
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Modulo));
                }
                '^' => {
                    curr_token = Some(Token::BinOp(FixedTokenSpan::new(cursor), Pow));
                }
                '+' => {
                    let is_unary = if let Some(tty) = tokens.last() {
                        tty.kind() == TokenKind::BinOp
                    } else {
                        true
                    };
                    if is_unary {
                        curr_token = Some(Token::UnaryOp(
                            FixedTokenSpan::new(cursor),
                            UnaryOpKind::Pos,
                        ));
                    } else {
                        curr_token =
                            Some(Token::BinOp(FixedTokenSpan::new(cursor), BinOpKind::Add));
                    }
                }
                '-' | '−' => {
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
                    return diagnostic_builder!(format!("`{}` is not a known token", curr), cursor);
                }
            }

            if let Some(token) = curr_token.take() {
                check_prev_token(
                    &tokens,
                    token.kind(),
                    curr,
                    cursor,
                    if let Token::UnaryOp(_, kind) = &token {
                        kind.arg_position() == ArgPosition::LHS
                    } else {
                        false
                    },
                )?;
                tokens.push(token);
            }
            cursor += 1;
        }
        tokens.push(EOF(FixedTokenSpan::new(chars.len())));
        Ok(tokens)
    }
}

fn check_prev_token(
    tokens: &Vec<Token>,
    curr: TokenKind,
    chr: char,
    cursor: usize,
    own_unary_arg_side_left: bool,
) -> PResult<()> {
    let needs_token = match curr {
        TokenKind::OpenParen => false,
        TokenKind::ClosedParen => true,
        TokenKind::At => todo!(),
        TokenKind::Comma => true,
        TokenKind::BinOp => false,
        TokenKind::UnaryOp => own_unary_arg_side_left,
        TokenKind::Literal => false,
        TokenKind::Region => false,
        TokenKind::EOF => false,
    };
    let ok = tokens
        .last()
        .map(|last| check_token(last, curr, own_unary_arg_side_left))
        .unwrap_or(!needs_token);

    if !ok {
        diagnostic_builder!(format!("`{}` at wrong location", chr), cursor)
    } else {
        Ok(())
    }
}

fn check_token(prev: &Token, curr: TokenKind, own_unary_arg_side_left: bool) -> bool {
    match curr {
        TokenKind::OpenParen => {
            let prev_kind = prev.kind();
            prev_kind != TokenKind::At && prev_kind != TokenKind::Comma
        },
        TokenKind::ClosedParen => {
            let prev_kind = prev.kind();
            if let Token::UnaryOp(_, kind) = prev {
                return kind.arg_position() == ArgPosition::LHS;
            }
            prev_kind != TokenKind::At && prev_kind != TokenKind::Comma && prev_kind != TokenKind::BinOp
        },
        TokenKind::At => todo!()/*{
            let prev_kind = prev.kind();
            if let Token::BinOp(_, kind) = prev {
                return kind != BinOpKind::Eq;
            }
            prev_kind != TokenKind::At
        }*/,
        TokenKind::Comma => {
            let prev_kind = prev.kind();
            if let Token::UnaryOp(_, kind) = prev {
                return kind.arg_position() == ArgPosition::LHS;
            }
            prev_kind != TokenKind::At && prev_kind != TokenKind::Comma && prev_kind != TokenKind::BinOp
        },
        TokenKind::BinOp => {
            let prev_kind = prev.kind();
            if let Token::UnaryOp(_, kind) = prev {
                return kind.arg_position() == ArgPosition::LHS;
            }
            prev_kind != TokenKind::At && prev_kind != TokenKind::Comma && prev_kind != TokenKind::BinOp
        },
        TokenKind::UnaryOp => {
            let prev_kind = prev.kind();
            if own_unary_arg_side_left {
                if let Token::UnaryOp(_, kind) = prev {
                    return kind.arg_position() == ArgPosition::LHS;
                }
                if prev_kind == TokenKind::Comma || prev_kind == TokenKind::OpenParen || prev_kind == TokenKind::BinOp {
                    return false;
                }
            }
            prev_kind != TokenKind::At && prev_kind != TokenKind::Comma
        },
        TokenKind::Literal => true,
        TokenKind::Region => true,
        TokenKind::EOF => {
            let prev_kind = prev.kind();
            if let Token::UnaryOp(_, kind) = prev {
                return kind.arg_position() == ArgPosition::LHS;
            }
            prev_kind != TokenKind::At && prev_kind != TokenKind::Comma && prev_kind != TokenKind::BinOp
        },
    }
}
