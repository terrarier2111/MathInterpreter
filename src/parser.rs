use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
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

    pub fn parse(&mut self, parse_context: &mut ParseContext) -> Option<f64> {
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
                let mut func_name = if let Token::Literal(x) = self.tokens.remove(0) {
                    x
                } else {
                    panic!("No function name was given!")
                };
                let mut arguments = vec![];
                for x in 0..(eq_location - 1) {
                    let token = self.tokens.remove(0);
                    if x % 2 == 1 {
                        if let Token::Literal(var) = token {
                            arguments.push(var);
                        }
                    }
                }
                println!("ceql: {} | {}", ((1 + 1 + ((2 * (arguments.len() as isize)) - 1).max(0) + 1) as usize), eq_location);
                if eq_location != ((1 + 1 + ((2 * (arguments.len() as isize)) - 1).max(0) + 1) as usize) { // function name, `(`, function arguments with `,` but last argument has no `,` so - 1, `)`
                    panic!("Equal at wrong location!");
                }
                action = Action::DefineFunc(func_name, arguments);
            } else if brace_end != usize::MAX {
                panic!("Invalid braces!")
            } else {
                let mut var_name = if let Token::Literal(x) = self.tokens.remove(0) {
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
        // TODO: Replace variables with values and replace function calls!
        match &self.action {
            Action::DefineVar(name) => {
                let mut replaced_tokens = vec![];
                println!("replacing!");
                for token in self.tokens.iter().enumerate() {
                    println!("tok!");
                    if let Token::Literal(content) = token.1 {
                        println!("before existance check! {}", content);
                        if parse_context.exists_var(content) {
                            println!("do stuff!");
                            replaced_tokens.push((token.0, parse_context.lookup_var(content)))
                        }
                    }
                }
                for token in replaced_tokens {
                    self.tokens[token.0] = Token::Number(token.1.to_string());
                }
                let mut rpn = shunting_yard(self.tokens.clone());
                for token in rpn.iter() {
                    println!("{:?}", token);
                }
                let result = eval_rpn(rpn);
                parse_context.register_var(name, result);
                Some(result)
            }
            Action::DefineFunc(name, args) => {
                parse_context.register_func(Function::new(name.clone(), args.clone(), self.tokens.clone()));
                None
            },
            Action::Eval => {
                let mut replaced_vars = vec![];
                for token in self.tokens.iter().enumerate() {
                    if let Token::Literal(content) = token.1 {
                        if parse_context.exists_var(content) {
                            replaced_vars.push((token.0, parse_context.lookup_var(content)))
                        }
                    }
                }
                for token in replaced_vars {
                    self.tokens[token.0] = Token::Number(token.1.to_string());
                }
                let mut rpn = shunting_yard(self.tokens.clone());
                for token in rpn.iter() {
                    println!("{:?}", token);
                }
                let result = eval_rpn(rpn);
                Some(result)
            },
        }
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

    vars: HashMap<String, (bool, f64)>,
    funcs: HashMap<String, (bool, Function)>,

}

impl ParseContext {

    pub fn new() -> Self {
        let mut ret = Self {
            vars: Default::default(),
            funcs: Default::default(),
        };
        let pi = String::from("pi");
        let e = String::from("e");
        ret.register_constant(&pi, std::f64::consts::PI);
        ret.register_constant(&e, std::f64::consts::E);
        ret
    }

    pub fn exists_var(&self, var_name: &String) -> bool {
        let var_name = var_name.to_lowercase();
        self.vars.contains_key(&*var_name)
    }

    pub fn lookup_var(&self, var_name: &String) -> f64 {
        let var_name = var_name.to_lowercase();
        (*self.vars.get(var_name.as_str()).unwrap()).1
    }

    pub fn register_var(&mut self, var_name: &String, value: f64) -> Result<bool, RegisterError> {
        let var_name = var_name.to_lowercase();
        if self.exists_fn(&var_name) {
            return Err(RegisterError("There is already a function with this name.".to_string()));
        }
        if let Some(x) = self.vars.get(var_name.as_str()) {
            if x.0 {
                self.vars.insert(var_name, (true, value));
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            self.vars.insert(var_name, (true, value));
            Ok(true)
        }
    }

    /// For internal use only!
    pub fn register_constant(&mut self, var_name: &String, value: f64) -> bool {
        let var_name = var_name.to_lowercase();
        if let Some(x) = self.vars.get(var_name.as_str()) {
            if x.0 {
                self.vars.insert(var_name, (false, value));
                true
            } else {
                false
            }
        } else {
            self.vars.insert(var_name, (false, value));
            true
        }
    }

    pub fn exists_fn(&self, func_name: &String) -> bool {
        let func_name = func_name.to_lowercase();
        self.funcs.contains_key(&*func_name)
    }

    pub fn call_func(&self, name: &String, args: Vec<usize>) -> Vec<Token> {
        let name = name.to_lowercase();
        let func = self.funcs.get(&*name).unwrap();
        func.1.build_tokens(args)
    }

    pub fn register_func(&mut self, func: Function) -> bool {
        let func_name = func.name.to_lowercase();
        if let Some(x) = self.funcs.get(func_name.as_str()) {
            if x.0 {
                self.funcs.insert(func_name, (false, func));
                true
            } else {
                false
            }
        } else {
            self.funcs.insert(func_name, (false, func));
            true
        }
    }

    pub fn register_builtin_func(&mut self, func: Function) -> bool {
        let func_name = func.name.to_lowercase();
        if let Some(x) = self.funcs.get(func_name.as_str()) {
            if x.0 {
                self.funcs.insert(func_name, (false, func));
                true
            } else {
                false
            }
        } else {
            self.funcs.insert(func_name, (false, func));
            true
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
            Token::Sign(_) => panic!("There was no sign expected!"),
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
    if num_stack.len() != 1 {
        panic!("There is more than 1 number left after finishing evaluation!")
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

pub struct Function {

    name: String,
    args: Vec<FunctionArg>,
    tokens: Vec<Token>,

}

impl Function {

    pub fn new(name: String, arg_names: Vec<String>, tokens: Vec<Token>) -> Self {
        let mut finished_args = vec![];
        for arg in arg_names.iter().enumerate() {
            let pos = arg.0;
            let mut arg_positions = vec![];
            for token in tokens.iter().enumerate() {
                if let Token::Literal(str) = token.1 {
                    if str == arg.1 {
                        arg_positions.push(token.0);
                    }
                }
            }
            finished_args.push(FunctionArg::new(arg.1.clone(), pos, arg_positions));
        }
        Self {
            name,
            args: finished_args,
            tokens,
        }
    }

    pub fn build_tokens(&self, arg_values: Vec<usize>) -> Vec<Token> {
        let mut ret = self.tokens.clone();
        for arg in self.args.iter().enumerate() {
            arg.1.build(arg_values[arg.0], &mut ret);
        }
        ret
    }

}

pub(crate) struct FunctionArg {

    pub(crate) name: String,
    pub(crate) in_func_pos: usize,
    pub(crate) token_positions: Vec<usize>,

}

impl FunctionArg {

    pub(crate) fn new(name: String, pos: usize, token_positions: Vec<usize>) -> Self {
        Self {
            name,
            in_func_pos: pos,
            token_positions
        }
    }

    pub(crate) fn build(&self, value: usize, tokens: &mut Vec<Token>) {
        let str = value.to_string();
        let token = Token::Number(str);
        for pos in self.token_positions.iter() {
            tokens[*pos] = token.clone();
        }
    }

}

#[derive(Debug)]
pub struct RegisterError(String);

impl Display for RegisterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&*self.0)
    }
}

impl Error for RegisterError {}