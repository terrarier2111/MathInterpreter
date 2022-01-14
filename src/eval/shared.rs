use std::rc::Rc;
use rust_decimal::Decimal;
use crate::eval::error::Span;

#[derive(Clone, Debug)]
pub enum Token {

    OpenParen(usize),
    ClosedParen(usize),
    Eq(usize),
    VertBar(usize),
    Comma(usize),
    Op(usize, OpKind),
    Literal(Span, String, Option<SignKind>),
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

pub type InternedToken<'ctx> = Rc<Interned<'ctx, Token>>;

pub struct Interned<'ctx, T: Sized> {
    interned: &'ctx mut T,
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

#[derive(Debug, Copy, Clone)]
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