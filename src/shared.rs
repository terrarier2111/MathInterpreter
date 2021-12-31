use rust_decimal::Decimal;

#[derive(Clone, Debug)]
pub enum Token {

    OpenParen,
    ClosedParen,
    Eq,
    Comma,
    Dot,
    Op(OpKind),
    Literal(String),
    Number(String),
    Sign(SignKind),
    Other(char),
    None,

}

impl Token {

    pub fn kind(&self) -> TokenKind {
        match self {
            Token::OpenParen => TokenKind::OpenParen,
            Token::ClosedParen => TokenKind::ClosedParen,
            Token::Eq => TokenKind::Eq,
            Token::Comma => TokenKind::Comma,
            Token::Dot => TokenKind::Dot,
            Token::Op(_) => TokenKind::Op,
            Token::Literal(_) => TokenKind::Literal,
            Token::Number(_) => TokenKind::Number,
            Token::Sign(_) => TokenKind::Sign,
            Token::Other(_) => TokenKind::Other,
            Token::None => unreachable!(),
        }
    }

}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {

    OpenParen,
    ClosedParen,
    Eq,
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