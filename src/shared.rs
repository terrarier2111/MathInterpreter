use std::marker::PhantomData;
use std::rc::Rc;
use rust_decimal::Decimal;
use crate::error::Span;

#[derive(Clone, Debug)]
pub enum Token {

    OpenParen(usize),
    ClosedParen(usize),
    Eq(usize),
    VertBar(usize),
    Comma(usize),
    Dot(usize),
    Op(usize, OpKind),
    Literal(Span, String),
    Number(Span, String),
    Sign(usize, SignKind),
    Other(usize, char),
    None,

}

impl Token {

    pub fn kind(&self) -> TokenKind {
        match self {
            Token::OpenParen(_) => TokenKind::OpenParen,
            Token::ClosedParen(_) => TokenKind::ClosedParen,
            Token::Eq(_) => TokenKind::Eq,
            Token::VertBar(_) => TokenKind::VertBar,
            Token::Comma(_) => TokenKind::Comma,
            Token::Dot(_) => TokenKind::Dot,
            Token::Op(_, _) => TokenKind::Op,
            Token::Literal(_, _) => TokenKind::Literal,
            Token::Number(_, _) => TokenKind::Number,
            Token::Sign(_, _) => TokenKind::Sign,
            Token::Other(_, _) => TokenKind::Other,
            Token::None => unreachable!(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Token::OpenParen(idx) => Span::new(*idx, idx + 1),
            Token::ClosedParen(idx) => Span::new(*idx, idx + 1),
            Token::Eq(idx) => Span::new(*idx, idx + 1),
            Token::VertBar(idx) => Span::new(*idx, idx + 1),
            Token::Comma(idx) => Span::new(*idx, idx + 1),
            Token::Dot(idx) => Span::new(*idx, idx + 1),
            Token::Op(idx, _) => Span::new(*idx, idx + 1),
            Token::Literal(sp, _) => *sp,
            Token::Number(sp, _) => *sp,
            Token::Sign(idx, _) => Span::new(*idx, idx + 1),
            Token::Other(idx, _) => Span::new(*idx, idx + 1),
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
    Dot,
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