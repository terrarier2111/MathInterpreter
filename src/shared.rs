use crate::diagnostic_builder;
use crate::error::DiagnosticBuilder;
use crate::parser::{PResult, ParseContext};
use crate::shared::ArgPosition::{LHS, RHS};
use crate::span::{FixedTokenSpan, Span};
use arpfloat::{Float, FP256};
use statrs::function::gamma::gamma;
use std::fmt::{Display, Formatter};
use std::mem::transmute;
use std::ops::Neg;

#[derive(Clone, Debug)]
pub enum Token {
    OpenParen(FixedTokenSpan),
    ClosedParen(FixedTokenSpan),
    At(FixedTokenSpan),
    Comma(FixedTokenSpan),
    UnaryOp(FixedTokenSpan, UnaryOpKind),
    BinOp(FixedTokenSpan, BinOpKind),
    Literal(LiteralToken),
    Region(Span, Vec<Token>),
    EOF(FixedTokenSpan), // end of file token
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Token::OpenParen(..) => TokenKind::OpenParen,
            Token::ClosedParen(..) => TokenKind::ClosedParen,
            Token::At(..) => TokenKind::At,
            Token::Comma(..) => TokenKind::Comma,
            Token::BinOp(..) => TokenKind::BinOp,
            Token::UnaryOp(..) => TokenKind::UnaryOp,
            Token::Literal(..) => TokenKind::Literal,
            Token::Region(..) => TokenKind::Region,
            Token::EOF(..) => TokenKind::EOF,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Token::OpenParen(span) => span.to_unfixed_span(),
            Token::ClosedParen(span) => span.to_unfixed_span(),
            Token::At(span) => span.to_unfixed_span(),
            Token::Comma(span) => span.to_unfixed_span(),
            Token::BinOp(span, _) => span.to_unfixed_span(),
            Token::UnaryOp(span, _) => span.to_unfixed_span(),
            Token::Literal(lit_tok) => lit_tok.span,
            Token::Region(sp, _) => *sp,
            Token::EOF(span) => span.to_unfixed_span(),
        }
    }

    pub fn implicitly_multiply_left(&self) -> ImplicitlyMultiply {
        match self {
            Token::OpenParen(..) => ImplicitlyMultiply::Right,
            Token::ClosedParen(..) => ImplicitlyMultiply::Left,
            Token::At(..) => ImplicitlyMultiply::Never,
            Token::Comma(..) => ImplicitlyMultiply::Never,
            Token::BinOp(..) => ImplicitlyMultiply::Never,
            Token::UnaryOp(_, op) => {
                if op.arg_position() == RHS {
                    ImplicitlyMultiply::Right
                } else {
                    ImplicitlyMultiply::Left
                }
            }
            Token::Literal(..) => ImplicitlyMultiply::Always,
            Token::Region(..) => ImplicitlyMultiply::Always,
            Token::EOF(..) => ImplicitlyMultiply::Never,
        }
    }

    pub fn to_raw(&self) -> String {
        match self {
            Token::OpenParen(_) => String::from('('),
            Token::ClosedParen(_) => String::from(')'),
            Token::At(_) => String::from('|'),
            Token::Comma(_) => String::from(','),
            Token::BinOp(_, op) => String::from(op.to_char()),
            Token::UnaryOp(_, op) => String::from(op.to_char()),
            Token::Literal(lit_tok) => {
                let mut result = lit_tok.content.clone();
                if lit_tok.trailing_space == TrailingSpace::Yes {
                    // FIXME: is this correct?
                    result.push(' ');
                }
                result
            }
            Token::Region(_, _) => todo!(),
            Token::EOF(_) => String::new(),
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
    pub(crate) kind: LiteralKind,
    pub(crate) trailing_space: TrailingSpace,
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[repr(u8)]
pub enum TrailingSpace {
    No = 0,
    Yes = 1,
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
    At,
    Comma,
    BinOp,
    UnaryOp,
    Literal,
    Region,
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
    Pos,       // `+`
    Neg,       // `-`
    Factorial, // `!`
    Exp(usize),
}

const EXPONENTS: [char; 10] = ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'];

impl UnaryOpKind {
    pub fn to_char(&self) -> char {
        match self {
            UnaryOpKind::Pos => '+',
            UnaryOpKind::Neg => '-',
            UnaryOpKind::Factorial => '!',
            UnaryOpKind::Exp(exp) => EXPONENTS[*exp],
        }
    }

    pub fn arg_position(&self) -> ArgPosition {
        match self {
            UnaryOpKind::Pos => RHS,
            UnaryOpKind::Neg => RHS,
            UnaryOpKind::Factorial => LHS,
            UnaryOpKind::Exp(_) => LHS,
        }
    }

    pub(crate) fn eval(&self, arg: Number, parse_ctx: &ParseContext) -> PResult<Number> {
        match self {
            UnaryOpKind::Pos => Ok(arg),
            UnaryOpKind::Neg => Ok(arg.neg()),
            UnaryOpKind::Factorial => {
                // FIXME: emmit warning because of lost precision
                Ok(num_from_f64(gamma(arg.as_f64() + 1.0)))
            }
            UnaryOpKind::Exp(exp) => Ok(arg.powi(*exp as u64)),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum ArgPosition {
    LHS,
    RHS,
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
            BinOpKind::Modulo => todo!(),
            BinOpKind::Pow => args.0.unwrap().pow(&args.1.unwrap()),
            BinOpKind::Eq => unreachable!(), // this has some special impl
        }
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

pub type Number = Float;

pub fn zero() -> Number {
    Number::zero(FP256, false)
}

pub fn one() -> Number {
    Number::one(FP256, false)
}

pub fn num_from_f64(num: f64) -> Number {
    Number::from_f64(num).cast(FP256)
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
            return Some(num_from_f64(lit_tok.content.parse::<f64>().unwrap()));
        }
    }
    None
}
