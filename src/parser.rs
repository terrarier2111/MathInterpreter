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

    fn handle_vars_and_fn_calls(&mut self, parse_context: &mut ParseContext) {
        let mut replaced_tokens = vec![];
        let mut possibly_replaced_funcs =  vec![];
        for token in self.tokens.iter().enumerate() {
            if let Token::Literal(content) = token.1 {
                if parse_context.exists_var(content) {
                    replaced_tokens.push((token.0, parse_context.lookup_var(content)))
                } else if parse_context.exists_fn(content) {
                    possibly_replaced_funcs.push((token.0, token.1.clone()));
                }
            }
        }
        for token in replaced_tokens {
            self.tokens[token.0] = Token::Number(token.1.to_string());
        }
        for pr in possibly_replaced_funcs {
            let start = pr.0;
            let mut args: Vec<(usize, usize)> = vec![];
            if self.tokens[pr.0 + 1].kind() == TokenKind::OpenParen {
                let mut offset = 1;
                let mut arg_start = pr.0 + 1 + 1;
                let mut opened_parens = 1;
                loop {
                    offset += 1;
                    let token = &self.tokens[start + offset];
                    match token {
                        Token::OpenParen => opened_parens += 1,
                        Token::ClosedParen => {
                            opened_parens -= 1;
                            if opened_parens == 0 {
                                args.push((arg_start, start + offset)); // TODO: Is this -1 correct?
                                break;
                            }
                        },
                        Token::Eq => panic!("Eq at wrong location!"),
                        Token::Comma => {
                            if opened_parens == 1 {
                                args.push((arg_start, start + offset));
                                arg_start = start + offset + 1;
                            }
                        },
                        Token::Dot => panic!("Dot at wrong location!"),
                        Token::Op(_) => {},
                        Token::Literal(_) => {},
                        Token::Number(_) => {},
                        Token::Sign(_) => {},
                        Token::Other(_) => {},
                        Token::None => unreachable!(),
                    }
                }
            } else {
                panic!("You have to put `(` arguments `)` behind the function name to perform a proper function call!")
            }
            let mut finished_args = vec![];
            let end = args.last().unwrap().1 + 1;
            for arg in args {
                let mut result = vec![];
                for x in (arg.0)..(arg.1) {
                    result.push(self.tokens[x].clone());
                }
                finished_args.push(result);
            }
            if let Token::Literal(func) = pr.1 {
                for _ in 0..(end - start) {
                    self.tokens.remove(start);
                }
                let result = parse_context.call_func(&func, finished_args);
                self.tokens.insert(pr.0, Token::OpenParen);
                let len = result.len();
                for token in result.into_iter().enumerate() {
                    self.tokens.insert(pr.0 + 1 + token.0, token.1);
                }
                self.tokens.insert(pr.0 + 1 + len, Token::ClosedParen);
            }
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
                for x in 0..eq_location {
                    let token = self.tokens.remove(0);
                    if x % 2 == 1 {
                        if let Token::Literal(var) = token {
                            arguments.push(var);
                        }
                    }
                }
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
        match self.action.clone() {
            Action::DefineVar(name) => {
                // Perform variable and function lookup and evaluation
                self.handle_vars_and_fn_calls(parse_context);

                // Perform the evaluation of the variable definition
                let mut rpn = shunting_yard(self.tokens.clone());
                let result = eval_rpn(rpn);
                parse_context.register_var(&name, result);
                Some(result)
            }
            Action::DefineFunc(name, args) => {
                parse_context.register_func(Function::new(name.clone(), args.clone(), self.tokens.clone(), parse_context));
                None
            },
            Action::Eval => {
                // Perform variable and function lookup and evaluation
                self.handle_vars_and_fn_calls(parse_context);

                // Perform the evaluation of the input statement
                let mut rpn = shunting_yard(self.tokens.clone());
                let result = eval_rpn(rpn);
                Some(result)
            },
        }
    }

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

    pub fn exists_const(&self, const_name: &String) -> bool {
        let const_name = const_name.to_lowercase();
        let con = self.vars.get(&*const_name);
        if let Some(con) = con {
            return !con.0;
        }
        false
    }

    pub fn lookup_const(&self, const_name: &String) -> f64 {
        // TODO: Improve error handling!
        let const_name = const_name.to_lowercase();
        let tmp = (*self.vars.get(const_name.as_str()).unwrap());
        if !tmp.0 {
            return tmp.1;
        }
        panic!("Not a const!")
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

    pub fn call_func(&self, name: &String, args: Vec<Vec<Token>>) -> Vec<Token> {
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
            Token::Literal(_) => panic!("Failed to evaluate literal correctly!"),
            Token::Number(_) => output.push(token),
            Token::Sign(_) => panic!("There was no sign expected!"),
            Token::Other(_) => panic!("Other is not allowed here!"),
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
            Token::Eq => panic!("Eq at wrong location!"),
            Token::Comma => panic!("Comma at wrong location!"),
            Token::Dot => panic!("Dot at wrong location!"),
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
            Token::Literal(_) => panic!("Failed to evaluate literal correctly!"),
            Token::Number(num) => num_stack.push(num.parse::<f64>().unwrap()), // TODO: Improve this!
            Token::Sign(_) => unreachable!(),
            Token::Other(_) => unreachable!(),
            Token::None => unreachable!(),
        }
    }
    if num_stack.len() != 1 {
        panic!("There is more than 1 number left after finishing evaluation!")
    }
    num_stack.pop().unwrap()
}

#[derive(Debug, Clone)]
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

pub(crate) struct Function {

    name: String,
    args: Vec<FunctionArg>,
    tokens: Vec<Token>,

}

impl Function {

    pub fn new(name: String, arg_names: Vec<String>, mut tokens: Vec<Token>, parse_context: &ParseContext) -> Self {
        // Detect arguments
        let mut finished_args = vec![];
        for arg in arg_names.iter() {
            let mut arg_positions = vec![];
            for token in tokens.iter().enumerate() {
                if let Token::Literal(str) = token.1 {
                    if str == arg {
                        arg_positions.push(token.0);
                    }
                }
            }
            finished_args.push(FunctionArg::new(arg_positions));
        }
        // Perform constant replacement
        let mut replace_consts = vec![];
        for token in tokens.iter().enumerate() {
            if let Token::Literal(str) = token.1 {
                if !arg_names.contains(str) {
                    if parse_context.exists_const(str) {
                        replace_consts.push((token.0, parse_context.lookup_const(str)));
                    } else {
                        panic!("Not an argument or const!")
                    }
                }
            }
        }
        for x in replace_consts {
            tokens[x.0] = Token::Number(x.1.to_string());
        }

        Self {
            name,
            args: finished_args,
            tokens,
        }
    }

    pub fn build_tokens(&self, arg_values: Vec<Vec<Token>>) -> Vec<Token> {
        let mut ret = self.tokens.clone();
        let mut offset = 0;
        for arg in self.args.iter().enumerate() {
            offset += arg.1.build(&arg_values[arg.0], &mut ret, offset);
        }
        ret
    }

}

pub(crate) struct FunctionArg {

    pub(crate) token_positions: Vec<usize>,

}

impl FunctionArg {

    pub(crate) fn new(token_positions: Vec<usize>) -> Self {
        Self {
            token_positions
        }
    }

    pub(crate) fn build(&self, value: &Vec<Token>, tokens: &mut Vec<Token>, offset: usize) -> usize {
        let added_tokens = value.len() + 1;
        for pos in self.token_positions.iter() {
            let pos = pos + offset;
            tokens[pos] = Token::OpenParen;
            let end = pos + 1 + value.len().clone();
            for token in value.into_iter().enumerate() {
                tokens.insert(pos + 1 + token.0, token.1.clone());
            }
            tokens.insert(end, Token::ClosedParen);
        }
        added_tokens
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