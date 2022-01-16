use crate::error::Span;
use rust_decimal::Decimal;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Token {
    OpenParen(usize),
    ClosedParen(usize),
    Eq(usize),
    VertBar(usize),
    Comma(usize),
    Op(usize, OpKind),
    Literal(Span, String, SignKind),
    Number(Span, String),
    Sign(usize, SignKind),
    Other(usize, char),
    None,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Token::OpenParen(..) => TokenKind::OpenParen,
            Token::ClosedParen(..) => TokenKind::ClosedParen,
            Token::Eq(..) => TokenKind::Eq,
            Token::VertBar(..) => TokenKind::VertBar,
            Token::Comma(..) => TokenKind::Comma,
            Token::Op(..) => TokenKind::Op,
            Token::Literal(..) => TokenKind::Literal,
            Token::Number(..) => TokenKind::Number,
            Token::Sign(..) => TokenKind::Sign,
            Token::Other(..) => TokenKind::Other,
            Token::None => unreachable!(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Token::OpenParen(idx) => Span::from_idx(*idx),
            Token::ClosedParen(idx) => Span::from_idx(*idx),
            Token::Eq(idx) => Span::from_idx(*idx),
            Token::VertBar(idx) => Span::from_idx(*idx),
            Token::Comma(idx) => Span::from_idx(*idx),
            Token::Op(idx, _) => Span::from_idx(*idx),
            Token::Literal(sp, ..) => *sp,
            Token::Number(sp, _) => *sp,
            Token::Sign(idx, _) => Span::from_idx(*idx),
            Token::Other(idx, _) => Span::from_idx(*idx),
            Token::None => unreachable!(),
        }
    }

    pub fn implicitly_multiply_left(&self) -> ImplicitlyMultiply {
        match self {
            Token::OpenParen(_) => ImplicitlyMultiply::Right,
            Token::ClosedParen(_) => ImplicitlyMultiply::Left,
            Token::Eq(_) => ImplicitlyMultiply::Never,
            Token::VertBar(_) => ImplicitlyMultiply::Never,
            Token::Comma(_) => ImplicitlyMultiply::Never,
            Token::Op(_, _) => ImplicitlyMultiply::Never,
            Token::Literal(_, _, _) => ImplicitlyMultiply::Always,
            Token::Number(_, _) => ImplicitlyMultiply::Always,
            Token::Sign(_, _) => unreachable!(),
            Token::Other(_, _) => unreachable!(),
            Token::None => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    OpenParen,
    ClosedParen,
    Eq,
    VertBar,
    Comma,
    Op,
    Literal,
    Number,
    Sign,
    Other,
}

impl TokenKind {
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ImplicitlyMultiply {
    Always,
    Left,
    Right,
    Never,
}

impl ImplicitlyMultiply {
    pub fn can_multiply_with_left(&self, left: ImplicitlyMultiply) -> bool {
        match self {
            ImplicitlyMultiply::Always => {
                matches!(left, ImplicitlyMultiply::Left | ImplicitlyMultiply::Always)
            }
            ImplicitlyMultiply::Left => false,
            ImplicitlyMultiply::Right => {
                matches!(left, ImplicitlyMultiply::Left | ImplicitlyMultiply::Always)
            }
            ImplicitlyMultiply::Never => false,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpKind {
    Plus,
    Minus,
    Divide,
    Multiply,
    Modulo,
    Pow, // FIXME: Is this a good name for "^"?

    OpenParen, // This is only needed for shunting yard evaluation.
}

impl OpKind {
    pub fn precedence(&self) -> u8 {
        match self {
            OpKind::Plus => 2,
            OpKind::Minus => 2,
            OpKind::Divide => 3,
            OpKind::Multiply => 3,
            OpKind::Modulo => 3, // TODO: Check this!
            OpKind::Pow => 4,
            OpKind::OpenParen => 0,
        }
    }

    pub fn associativity(&self) -> Associativity {
        match self {
            OpKind::Plus => Associativity::Left,
            OpKind::Minus => Associativity::Left,
            OpKind::Divide => Associativity::Left,
            OpKind::Multiply => Associativity::Left,
            OpKind::Modulo => Associativity::Left, // TODO: Check this!
            OpKind::Pow => Associativity::Right,
            OpKind::OpenParen => Associativity::Right, // This shouldn't be relevant!
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum SignKind {
    Plus,
    Minus,
}

impl SignKind {
    pub fn to_raw(&self) -> char {
        match self {
            SignKind::Plus => '+',
            SignKind::Minus => '-',
        }
    }
}

pub type Number = Decimal;

#[derive(Copy, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}
