use std::collections::HashMap;
use rust_decimal::Decimal;
use crate::shared::{Associativity, OpKind, SignKind, Token, TokenKind};

pub(crate) struct Parser {

    tokens: Vec<Token>,
    action: Action,

}

impl Parser {

    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            action: Action::Eval,
        }
    }

    pub fn parse(&mut self) {
        let mut eq_location = usize::MAX;
        let mut brace_start = usize::MAX;
        let mut brace_end = usize::MAX;
        let mut arguments_list = vec![];
        for token in self.tokens.iter().enumerate() {
            match token.1 {
                Token::OpenParen => brace_start = token.0,
                Token::ClosedParen => brace_end = token.0,
                Token::Eq => {
                    eq_location = token.0; // TODO: Detect second Eq and panic!
                    break;
                },
                Token::Comma => arguments_list.push(token.0),
                Token::Dot => {}
                Token::Op(_) => {}
                Token::Literal(_) => {}
                Token::Number(_) => {}
                Token::Sign(_) => {}
                Token::Other(_) => unreachable!(),
                Token::Invalid(_) => unreachable!(),
                Token::None => unreachable!(),
            }
        }
        let mut action = Action::Eval;
        if eq_location != usize::MAX {
            if brace_start != usize::MAX {
                if brace_end == usize::MAX {
                    panic!("Invalid braces!")
                }
                let mut func_name = if let Token::Literal(x) = self.tokens.pop().unwrap() {
                    x
                } else {
                    panic!("No function name was given!")
                };
                let mut arguments = vec![];
                // TODO: populate arguments!
                action = Action::DefineFunc(func_name, arguments);
            } else if brace_end != usize::MAX {
                panic!("Invalid braces!")
            } else {
                let mut var_name = if let Token::Literal(x) = self.tokens.pop().unwrap() {
                    x
                } else {
                    panic!("No function name was given!")
                };
                if eq_location != 1 {
                    panic!("Equal at wrong location!");
                }
                action = Action::DefineVar(var_name);
            }
        }
        self.action = action;
    }

    /*pub fn expect(&self, token: &Token) -> bool {
        matches!(self.tokens.get(self.cursor).unwrap(), token)
    }

    pub fn bump(&mut self) -> Option<&Token> {
        self.cursor += 1;
        self.tokens.get(self.cursor)
    }

    pub fn look_back(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.cursor - offset)
    }

    pub fn look_back_mut(&mut self, offset: usize) -> Option<&mut Token> {
        self.tokens.get_mut(self.cursor - offset)
    }

    pub fn look_ahead(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.cursor + offset)
    }

    pub fn look_ahead_mut(&mut self, offset: usize) -> Option<&mut Token> {
        self.tokens.get_mut(self.cursor + offset)
    }*/

}

pub(crate) struct ParseContext {

    variables: HashMap<String, Decimal>,

}

impl ParseContext {

    pub fn new() -> Self {
        Self {
            variables: Default::default(),
        }
    }

}

pub fn shunting_yard(input: Vec<Token>) -> Vec<Token> {
    let mut output = vec![];
    let mut operator_stack: Vec<OpKind> = vec![];
    for token in input {
        match token {
            Token::OpenParen => operator_stack.push(OpKind::OpenParen),
            Token::ClosedParen => {
                loop {
                    if let Some(op) = operator_stack.pop() {
                        if op == OpKind::OpenParen {
                            break;
                        }
                        output.push(Token::Op(op));
                    } else {
                        // TODO: Return error!
                        panic!("Invalid parens!")
                    }
                }
            },
            Token::Eq => {},
            Token::Comma => {},
            Token::Dot => {},
            Token::Op(op) => {
                while !operator_stack.is_empty() &&
                    (op.precedence() < operator_stack.last().unwrap().precedence() ||
                        (op.precedence() == operator_stack.last().unwrap().precedence() && op.associativity() == Associativity::Left)) {
                    output.push(Token::Op(operator_stack.pop().unwrap()));
                }
                operator_stack.push(op);
            },
            Token::Literal(_) => {}, // TODO: Support variables!
            Token::Number(_) => output.push(token),
            Token::Sign(_) => {},
            Token::Other(_) => {},
            Token::Invalid(_) => {},
            Token::None => unreachable!(),
        }
    }
    for operator in operator_stack.into_iter().rev() {
        output.push(Token::Op(operator));
    }
    output
}

pub fn eval_rpn(input: Vec<Token>) -> f64 {
    let mut num_stack: Vec<f64> = vec![];
    for token in input {
        match token {
            Token::OpenParen => unreachable!(),
            Token::ClosedParen => unreachable!(),
            Token::Eq => {}
            Token::Comma => {}
            Token::Dot => unreachable!(),
            Token::Op(op) => {
                match op {
                    OpKind::Plus => {
                        let num_2 = num_stack.pop().unwrap();
                        let num_1 = num_stack.pop().unwrap();
                        num_stack.push(num_1 + num_2)
                    },
                    OpKind::Minus => {
                        let num_2 = num_stack.pop().unwrap();
                        let num_1 = num_stack.pop().unwrap();
                        num_stack.push(num_1 - num_2)
                    },
                    OpKind::Divide => {
                        let num_2 = num_stack.pop().unwrap();
                        let num_1 = num_stack.pop().unwrap();
                        num_stack.push(num_1 / num_2)
                    },
                    OpKind::Multiply => {
                        let num_2 = num_stack.pop().unwrap();
                        let num_1 = num_stack.pop().unwrap();
                        num_stack.push(num_1 * num_2)
                    },
                    OpKind::Modulo => {
                        let num_2 = num_stack.pop().unwrap();
                        let num_1 = num_stack.pop().unwrap();
                        num_stack.push(num_1 % num_2)
                    },
                    OpKind::Pow => {
                        let num_2 = num_stack.pop().unwrap();
                        let num_1 = num_stack.pop().unwrap();
                        num_stack.push(num_1.powf(num_2))
                    },
                    OpKind::OpenParen => unreachable!(),
                }
            },
            Token::Literal(_) => {}
            Token::Number(num) => num_stack.push(num.parse::<f64>().unwrap()), // TODO: Improve this!
            Token::Sign(_) => unreachable!(),
            Token::Other(_) => unreachable!(),
            Token::Invalid(_) => unreachable!(),
            Token::None => unreachable!(),
        }
    }
    println!("numbers: {}", num_stack.len());
    for x in num_stack.iter() {
        println!("num: {}", x);
    }
    num_stack.pop().unwrap()
}

#[derive(Debug)]
pub enum Action {

    DefineVar(String), // var name
    DefineFunc(String, Vec<String>), // function name, function arguments
    Eval,

}

impl Action {

    pub fn kind(&self) -> ActionKind {
        match self {
            Action::DefineVar(_) => ActionKind::DefineVar,
            Action::DefineFunc(_, _) => ActionKind::DefineFunc,
            Action::Eval => ActionKind::Eval,
        }
    }

}

#[derive(Debug, Copy, Clone)]
pub enum ActionKind {

    DefineVar,
    DefineFunc,
    Eval,

}