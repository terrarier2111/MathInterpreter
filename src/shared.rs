use crate::span::Span;
use rust_decimal::{Decimal, MathematicalOps};
use std::fmt::{Display, Formatter};
use std::mem::transmute;
use std::ops::Neg;

#[derive(Clone, Debug)]
pub enum Token {
    OpenParen(usize),
    ClosedParen(usize),
    VertBar(usize),
    Comma(usize),
    UnaryOp(usize, UnaryOpKind),
    BinOp(usize, BinOpKind),
    Literal(LiteralToken),
    Sign(usize, SignKind),
    Region(Span, Vec<Token>),
    Other(usize, char),
    None,
    EOF(usize), // end of file token
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Token::OpenParen(..) => TokenKind::OpenParen,
            Token::ClosedParen(..) => TokenKind::ClosedParen,
            Token::VertBar(..) => TokenKind::VertBar,
            Token::Comma(..) => TokenKind::Comma,
            Token::BinOp(..) => TokenKind::BinOp,
            Token::UnaryOp(..) => TokenKind::UnaryOp,
            Token::Literal(..) => TokenKind::Literal,
            Token::Sign(..) => TokenKind::Sign,
            Token::Region(..) => TokenKind::Region,
            Token::Other(..) => TokenKind::Other,
            Token::EOF(..) => TokenKind::EOF,
            Token::None => unreachable!(),
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Token::OpenParen(idx) => Span::single_token(*idx),
            Token::ClosedParen(idx) => Span::single_token(*idx),
            Token::VertBar(idx) => Span::single_token(*idx),
            Token::Comma(idx) => Span::single_token(*idx),
            Token::BinOp(idx, _) => Span::single_token(*idx),
            Token::UnaryOp(idx, _) => Span::single_token(*idx),
            Token::Literal(lit_tok) => lit_tok.span,
            Token::Sign(idx, _) => Span::single_token(*idx),
            Token::Region(sp, _) => *sp,
            Token::Other(idx, _) => Span::single_token(*idx),
            Token::EOF(idx) => Span::single_token(*idx),
            Token::None => unreachable!(),
        }
    }

    pub fn implicitly_multiply_left(&self) -> ImplicitlyMultiply {
        match self {
            Token::OpenParen(..) => ImplicitlyMultiply::Right,
            Token::ClosedParen(..) => ImplicitlyMultiply::Left,
            Token::VertBar(..) => ImplicitlyMultiply::Never,
            Token::Comma(..) => ImplicitlyMultiply::Never,
            Token::BinOp(..) => ImplicitlyMultiply::Never,
            Token::UnaryOp(..) => ImplicitlyMultiply::Left,
            Token::Literal(..) => ImplicitlyMultiply::Always,
            Token::Region(..) => ImplicitlyMultiply::Always,
            Token::EOF(..) => ImplicitlyMultiply::Never,
            Token::Sign(..) => unreachable!(),
            Token::Other(..) => unreachable!(),
            Token::None => unreachable!(),
        }
    }

    pub fn to_raw(&self) -> String {
        match self {
            Token::OpenParen(_) => String::from('('),
            Token::ClosedParen(_) => String::from(')'),
            Token::VertBar(_) => String::from('|'),
            Token::Comma(_) => String::from(','),
            Token::BinOp(_, op) => String::from(op.to_char()),
            Token::UnaryOp(_, op) => String::from(op.to_char()),
            Token::Literal(lit_tok) => {
                let mut result = lit_tok.content.clone();
                if lit_tok.sign == SignKind::Minus {
                    result.insert(0, '-');
                }
                if lit_tok.trailing_space == TrailingSpace::Yes {
                    // FIXME: is this correct?
                    result.push(' ');
                }
                result
            }
            Token::Sign(_, sign) => String::from(sign.to_raw()),
            Token::Region(_, _) => todo!(),
            Token::Other(_, raw) => String::from(*raw),
            Token::EOF(_) => String::new(),
            Token::None => unreachable!(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.to_raw().as_str())
    }
}

#[derive(Clone, Debug)]
pub struct LiteralToken {
    pub(crate) span: Span,
    pub(crate) content: String,
    pub(crate) sign: SignKind,
    pub(crate) kind: LiteralKind,
    pub(crate) trailing_space: TrailingSpace,
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum TrailingSpace {
    No,
    Yes,
    Maybe,
}

impl From<bool> for TrailingSpace {
    fn from(val: bool) -> Self {
        // This is safe as `false` is represented as 0
        // and `true` is represented as 1 in a u8 value
        // and thus we can "cast" this value into our
        // enum and convert it to the No/Yes variants
        unsafe { transmute::<bool, TrailingSpace>(val) }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum TokenKind {
    OpenParen,
    ClosedParen,
    VertBar,
    Comma,
    BinOp,
    UnaryOp,
    Literal,
    Sign,
    Region,
    Other,
    EOF, // end of file token
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
pub enum UnaryOpKind {
    Neg,       // `-`
    Factorial, // `!`
}

impl UnaryOpKind {
    pub fn to_char(&self) -> char {
        match self {
            UnaryOpKind::Neg => '-',
            UnaryOpKind::Factorial => '!',
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum BinOpKind {
    Add,      // `+`
    Subtract, // `-`
    Divide,   // `/`
    Multiply, // `*`
    Modulo,   // `%`
    Pow,      // FIXME: Is this a good name for "^"?
    Eq,       // `=`

              // OpenParen, // This is only needed for shunting yard evaluation.
}

impl BinOpKind {
    pub fn to_char(&self) -> char {
        match self {
            BinOpKind::Add => '+',
            BinOpKind::Subtract => '-',
            BinOpKind::Divide => '/',
            BinOpKind::Multiply => '*',
            BinOpKind::Modulo => '%',
            BinOpKind::Pow => '^',
            // OpKind::OpenParen => unimplemented!(),
            BinOpKind::Eq => '=',
        }
    }

    pub fn precedence(&self) -> u8 {
        match self {
            BinOpKind::Add => 2,
            BinOpKind::Subtract => 2,
            BinOpKind::Divide => 3,
            BinOpKind::Multiply => 3,
            BinOpKind::Modulo => 3, // TODO: Check this!
            BinOpKind::Pow => 4,
            // OpKind::OpenParen => 0,
            BinOpKind::Eq => 1,
        }
    }

    pub fn associativity(&self) -> Associativity {
        match self {
            BinOpKind::Add => Associativity::Left,
            BinOpKind::Subtract => Associativity::Left,
            BinOpKind::Divide => Associativity::Left,
            BinOpKind::Multiply => Associativity::Left,
            BinOpKind::Modulo => Associativity::Left, // TODO: Check this!
            BinOpKind::Pow => Associativity::Right,
            // OpKind::OpenParen => Associativity::Right, // This shouldn't be relevant!
            BinOpKind::Eq => unreachable!(),
        }
    }

    // FIXME: this should go!
    pub fn args(&self) -> ArgsKind {
        match self {
            BinOpKind::Add => ArgsKind::Both,
            BinOpKind::Subtract => ArgsKind::Both,
            BinOpKind::Divide => ArgsKind::Both,
            BinOpKind::Multiply => ArgsKind::Both,
            BinOpKind::Modulo => ArgsKind::Both,
            BinOpKind::Pow => ArgsKind::Both,
            // OpKind::OpenParen => ArgsKind::None,
            BinOpKind::Eq => ArgsKind::Both,
        }
    }

    // FIXME: we should probably replace this!
    pub fn eval(&self, args: (Option<Number>, Option<Number>)) -> Number {
        match self {
            BinOpKind::Add => args.0.unwrap() + args.1.unwrap(),
            BinOpKind::Subtract => args.0.unwrap() - args.1.unwrap(),
            BinOpKind::Divide => args.0.unwrap() / args.1.unwrap(),
            BinOpKind::Multiply => args.0.unwrap() * args.1.unwrap(),
            BinOpKind::Modulo => args.0.unwrap() % args.1.unwrap(),
            BinOpKind::Pow => args.0.unwrap().powd(args.1.unwrap()),
            // OpKind::OpenParen => unreachable!(),
            BinOpKind::Eq => unreachable!(), // this has some special impl
        }
        .normalize()
    }

    // FIXME: this should go!
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
pub enum SignKind {
    Default, // FIXME: replace this with the #[default] attribute on the Plus variant!
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
    if let Token::Literal(lit_tok) = token {
        if lit_tok.kind == LiteralKind::Number {
            let mut ret = lit_tok.content.parse::<Number>().unwrap();
            if lit_tok.sign == SignKind::Minus {
                ret = ret.neg();
            }
            return Some(ret);
        }
    }
    None
}
