use crate::error::Span;
use rust_decimal::Decimal;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Token {
    OpenParen(usize),
    ClosedParen(usize),
    Eq(usize),
    VertBar(usize),
    Comma(usize),
    Op(usize, OpKind),
    Literal(Span, String, SignKind, LiteralKind), // span, content, sign, kind
    Sign(usize, SignKind),
    Region(Span, Vec<Token>),
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
            Token::Sign(..) => TokenKind::Sign,
            Token::Region(..) => TokenKind::Region,
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
            Token::Sign(idx, _) => Span::from_idx(*idx),
            Token::Region(sp, _) => *sp,
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
            Token::Literal(..) => ImplicitlyMultiply::Always,
            Token::Region(..) => ImplicitlyMultiply::Always,
            Token::Sign(_, _) => unreachable!(),
            Token::Other(_, _) => unreachable!(),
            Token::None => unreachable!(),
        }
    }

    pub fn to_raw(&self) -> String {
        match self {
            Token::OpenParen(_) => String::from("("),
            Token::ClosedParen(_) => String::from(")"),
            Token::Eq(_) => String::from("="),
            Token::VertBar(_) => String::from("|"),
            Token::Comma(_) => String::from(","),
            Token::Op(_, op) => match op {
                OpKind::Plus => String::from("+"),
                OpKind::Minus => String::from("-"),
                OpKind::Divide => String::from("/"),
                OpKind::Multiply => String::from("*"),
                OpKind::Modulo => String::from("%"),
                OpKind::Pow => String::from("^"),
                OpKind::OpenParen => panic!("Are you sure this is correct?"), // TODO: Check this!
            },
            Token::Literal(_, buf, sign, _) => {
                let mut result = buf.clone();
                if sign == &SignKind::Minus {
                    result.insert(0, sign.to_raw()); // TODO: Should this to_raw call be replaced by a '-'?
                }
                result
            }
            Token::Sign(_, sign) => String::from(sign.to_raw()),
            Token::Region(_, _) => todo!(),
            Token::Other(_, raw) => String::from(*raw),
            Token::None => unreachable!(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_raw().as_str())
    }
}

/*
impl Display for Vec<Token> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for _ in self.iter() {
            f.write_str(self.to_raw().as_str())?;
        }
        std::fmt::Result::Ok(())
    }
}*/

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    OpenParen,
    ClosedParen,
    Eq,
    VertBar,
    Comma,
    Op,
    Literal,
    Sign,
    Region,
    Other,
}

impl TokenKind {
    pub fn to_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum LiteralKind {
    Number,
    CharSeq,
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
