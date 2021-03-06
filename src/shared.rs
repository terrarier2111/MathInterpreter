use crate::error::Span;
use rust_decimal::{Decimal, MathematicalOps};
use std::fmt::{Display, Formatter};
use std::ops::Neg;

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
            Token::OpenParen(_) => String::from('('),
            Token::ClosedParen(_) => String::from(')'),
            Token::Eq(_) => String::from('='),
            Token::VertBar(_) => String::from('|'),
            Token::Comma(_) => String::from(','),
            Token::Op(_, op) => String::from(op.to_char()),
            Token::Literal(_, buf, sign, _) => {
                let mut result = buf.clone();
                if sign == &SignKind::Minus {
                    result.insert(0, '-');
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
    Add,
    Subtract,
    Divide,
    Multiply,
    Modulo,
    Pow, // FIXME: Is this a good name for "^"?

    OpenParen, // This is only needed for shunting yard evaluation.
}

impl OpKind {
    pub fn to_char(&self) -> char {
        match self {
            OpKind::Add => '+',
            OpKind::Subtract => '-',
            OpKind::Divide => '/',
            OpKind::Multiply => '*',
            OpKind::Modulo => '%',
            OpKind::Pow => '^',
            OpKind::OpenParen => unimplemented!(),
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            OpKind::Add => 2,
            OpKind::Subtract => 2,
            OpKind::Divide => 3,
            OpKind::Multiply => 3,
            OpKind::Modulo => 3, // TODO: Check this!
            OpKind::Pow => 4,
            OpKind::OpenParen => 0,
        }
    }

    pub fn associativity(&self) -> Associativity {
        match self {
            OpKind::Add => Associativity::Left,
            OpKind::Subtract => Associativity::Left,
            OpKind::Divide => Associativity::Left,
            OpKind::Multiply => Associativity::Left,
            OpKind::Modulo => Associativity::Left, // TODO: Check this!
            OpKind::Pow => Associativity::Right,
            OpKind::OpenParen => Associativity::Right, // This shouldn't be relevant!
        }
    }

    pub fn args(&self) -> ArgsKind {
        match self {
            OpKind::Add => ArgsKind::Both,
            OpKind::Subtract => ArgsKind::Both,
            OpKind::Divide => ArgsKind::Both,
            OpKind::Multiply => ArgsKind::Both,
            OpKind::Modulo => ArgsKind::Both,
            OpKind::Pow => ArgsKind::Both,
            OpKind::OpenParen => ArgsKind::None,
        }
    }

    pub fn eval(&self, args: (Option<Number>, Option<Number>)) -> Number {
        match self {
            OpKind::Add => args.0.unwrap() + args.1.unwrap(),
            OpKind::Subtract => args.0.unwrap() - args.1.unwrap(),
            OpKind::Divide => args.0.unwrap() / args.1.unwrap(),
            OpKind::Multiply => args.0.unwrap() * args.1.unwrap(),
            OpKind::Modulo => args.0.unwrap() % args.1.unwrap(),
            OpKind::Pow => args.0.unwrap().powd(args.1.unwrap()),
            OpKind::OpenParen => unreachable!(),
        }
        .normalize()
    }

    pub fn resolve_num_args(&self, tokens: &TokenStream) -> (Option<Token>, Option<Token>) {
        let right = if self.args().has_right() {
            tokens
                .inner_tokens()
                .get(tokens.inner_idx() + 1)
                .map(|token| token.clone())
        } else {
            None
        };
        let left = if self.args().has_left() {
            tokens
                .inner_tokens()
                .get(tokens.inner_idx() - 1)
                .map(|token| token.clone())
        } else {
            None
        };
        (left, right)
    }

    pub fn is_valid<T>(&self, args: &(Option<T>, Option<T>)) -> bool {
        (self.args().has_left() == args.0.is_some())
            && (self.args().has_right() == args.1.is_some())
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArgsKind {
    None,
    Left,
    Right,
    Both,
}

impl ArgsKind {
    pub fn has_left(&self) -> bool {
        match self {
            ArgsKind::None => false,
            ArgsKind::Left => true,
            ArgsKind::Right => false,
            ArgsKind::Both => true,
        }
    }

    pub fn has_right(&self) -> bool {
        match self {
            ArgsKind::None => false,
            ArgsKind::Left => false,
            ArgsKind::Right => true,
            ArgsKind::Both => true,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(u8)]
pub enum SignKind {
    Default,
    Plus,
    Minus,
}

impl SignKind {
    pub fn to_raw(&self) -> char {
        match self {
            SignKind::Plus | SignKind::Default => '+',
            SignKind::Minus => '-',
        }
    }
}

pub type Number = Decimal;

pub(crate) fn num_to_num_and_sign(num: Number) -> (Number, SignKind) {
    if num.is_sign_negative() {
        (num.neg(), SignKind::Minus)
    } else {
        (num, SignKind::Default)
    }
}

#[derive(Copy, Clone, PartialEq)]
pub enum Associativity {
    Left,
    Right,
}

pub struct TokenStream {
    pub(crate) input: String,
    tokens: Vec<Token>,
    idx: usize,
}

impl TokenStream {
    pub const fn new(input: String, tokens: Vec<Token>) -> Self {
        Self {
            input,
            tokens,
            idx: 0,
        }
    }

    pub fn next(&mut self) -> Option<&Token> {
        let ret = self.tokens.get(self.idx);
        self.idx += 1;
        ret
    }

    pub fn next_mut(&mut self) -> Option<&mut Token> {
        let ret = self.tokens.get_mut(self.idx);
        self.idx += 1;
        ret
    }

    pub fn pop_token(&mut self) -> Token {
        self.tokens.remove(self.idx - 1)
    }

    pub fn remove_token(&mut self, idx: usize) -> Token {
        self.tokens.remove(idx)
    }

    #[inline]
    pub fn look_back(&self) -> Option<&Token> {
        self.look_back_by(1)
    }

    pub fn look_back_by(&self, by: usize) -> Option<&Token> {
        self.tokens.get(self.idx - by + 1)
    }

    #[inline]
    pub fn look_ahead(&self) -> Option<&Token> {
        self.look_ahead_by(1)
    }

    pub fn look_ahead_by(&self, by: usize) -> Option<&Token> {
        self.tokens.get(self.idx + by - 1)
    }

    #[inline]
    pub fn look_back_mut(&mut self) -> Option<&mut Token> {
        self.look_back_by_mut(1)
    }

    pub fn look_back_by_mut(&mut self, by: usize) -> Option<&mut Token> {
        self.tokens.get_mut(self.idx - by)
    }

    #[inline]
    pub fn look_ahead_mut(&mut self) -> Option<&mut Token> {
        self.look_ahead_by_mut(1)
    }

    pub fn look_ahead_by_mut(&mut self, by: usize) -> Option<&mut Token> {
        self.tokens.get_mut(self.idx + by)
    }

    #[inline]
    pub fn go_back(&mut self) {
        self.idx -= 1;
    }

    #[inline]
    pub fn reset(&mut self) {
        self.idx = 0;
    }

    #[inline]
    pub fn inner_tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    #[inline]
    pub fn inner_tokens_mut(&mut self) -> &mut Vec<Token> {
        &mut self.tokens
    }

    #[inline]
    pub fn inner_idx(&self) -> usize {
        self.idx - 1
    }

    #[inline]
    pub fn to_tokens(self) -> Vec<Token> {
        self.tokens
    }
}

pub fn token_to_num(token: &Token) -> Option<Number> {
    if let Token::Literal(_, lit, sign, kind) = token {
        if kind == &LiteralKind::Number {
            let mut ret = lit.parse::<Number>().unwrap();
            if sign == &SignKind::Minus {
                ret = ret.neg();
            }
            return Some(ret);
        }
    }
    None
}
