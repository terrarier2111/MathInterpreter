use crate::error::{DiagnosticBuilder, Span};
use crate::shared::{
    Associativity, ImplicitlyMultiply, LiteralKind, Number, OpKind, SignKind, Token, TokenKind,
};
use rust_decimal::prelude::ToPrimitive;
use rust_decimal::MathematicalOps;
use std::collections::HashMap;
use std::ops::Neg;
#[macro_use]
use crate::parser::macros as mac;
use crate::{
    diagnostic_builder, diagnostic_builder_spanned, register_builtin_func, register_const,
};

const NONE: usize = usize::MAX;

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

    fn handle_vars_and_fn_calls(&mut self, parse_context: &mut ParseContext) -> PResult<()> {
        println!("var_or_fn_calls!");
        let mut replaced_tokens = vec![];
        let mut possibly_replaced_funcs = vec![];
        for token in self.tokens.iter().enumerate() {
            if let Token::Literal(_, content, sign, kind) = token.1 {
                if *kind == LiteralKind::CharSeq {
                    if parse_context.exists_var(content) {
                        let mut var = parse_context.lookup_var(content);
                        if sign == &SignKind::Minus {
                            var = var.neg();
                        }
                        replaced_tokens.push((token.0, var))
                    } else if parse_context.exists_fn(content) {
                        possibly_replaced_funcs.push((token.0, token.1.clone()));
                    }
                }
            }
        }
        for token in replaced_tokens {
            self.tokens[token.0] = Token::Literal(
                Span::NONE,
                token.1.to_string(),
                SignKind::Plus,
                LiteralKind::Number,
            );
            // TODO: Detect the correct sign!
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
                        Token::OpenParen(_) => opened_parens += 1,
                        Token::ClosedParen(_) => {
                            opened_parens -= 1;
                            if opened_parens == 0 {
                                args.push((arg_start, start + offset));
                                break;
                            }
                        }
                        Token::Eq(sp) => {
                            return diagnostic_builder!(
                                parse_context.input.clone(),
                                "`=` at wrong location",
                                *sp
                            );
                        }
                        Token::VertBar(_) => {
                            todo!()
                        }
                        Token::Comma(_) => {
                            if opened_parens == 1 {
                                args.push((arg_start, start + offset));
                                arg_start = start + offset + 1;
                            }
                        }
                        Token::Op(_, _) => {}
                        Token::Literal(..) => {}
                        Token::Sign(_, _) => {}
                        Token::Other(_, _) => {}
                        Token::Region(_, _) => panic!(),
                        Token::None => unreachable!(),
                    }
                }
            } else {
                return diagnostic_builder!(parse_context.input.clone(), "You have to put `(` arguments `)` behind the function name to perform a proper function call");
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
            if let Token::Literal(_span, func, _, kind) = pr.1 {
                if kind == LiteralKind::CharSeq {
                    for _ in 0..(end - start) {
                        self.tokens.remove(start);
                    }
                    let result = parse_context.call_func(&func, finished_args)?;
                    self.tokens.insert(pr.0, Token::OpenParen(pr.0));
                    let len = result.len();
                    for token in result.into_iter().enumerate() {
                        self.tokens.insert(pr.0 + 1 + token.0, token.1);
                    }
                    self.tokens.insert(pr.0 + 1 + len, Token::ClosedParen(pr.0));
                }
            }
        }
        Ok(())
    }

    pub fn parse(&mut self, parse_context: &mut ParseContext) -> PResult<Option<Number>> {
        let mut eq_location = NONE;
        let mut brace_start = NONE;
        let mut brace_end = NONE;
        let mut arguments_list = vec![];

        let mut last_token_mult = ImplicitlyMultiply::Never;
        let mut multiplications = vec![];
        for token in self.tokens.iter().enumerate() {
            if eq_location == NONE {
                match token.1 {
                    Token::OpenParen(_) => brace_start = token.0,
                    Token::ClosedParen(_) => brace_end = token.0,
                    Token::Eq(_) => {
                        multiplications.clear();
                        eq_location = token.0;
                    } // TODO: Detect second Eq and error!
                    Token::Comma(_) => arguments_list.push(token.0),
                    Token::Op(_, _) => {}
                    Token::Literal(..) => {}
                    Token::Sign(_, _) => {}
                    Token::Other(sp, raw) => {
                        return diagnostic_builder!(
                            parse_context.input.clone(),
                            format!("Unexpected token `{}`", raw),
                            *sp
                        )
                    }
                    Token::VertBar(_) => {}
                    Token::Region(_, _) => panic!(),
                    Token::None => unreachable!(),
                }
            }
            let curr_token_mult = if let Token::Literal(_, buff, _, kind) = token.1 {
                if *kind == LiteralKind::CharSeq
                    && (parse_context.exists_fn(buff) || parse_context.exists_builtin_func(buff))
                {
                    ImplicitlyMultiply::Never
                } else {
                    ImplicitlyMultiply::Always
                }
            } else {
                token.1.implicitly_multiply_left()
            };
            if curr_token_mult.can_multiply_with_left(last_token_mult) {
                multiplications.push(token.0);
            }
            last_token_mult = curr_token_mult;
        }
        for x in multiplications.iter().enumerate() {
            self.tokens
                .insert(x.0 + *x.1, Token::Op(x.0, OpKind::Multiply));
        }
        let mut bar = NONE;
        let mut rec_eq = NONE;
        if eq_location != NONE {
            for token in self.tokens.iter().enumerate() {
                match token.1 {
                    Token::OpenParen(_) => brace_start = token.0,
                    Token::ClosedParen(_) => brace_end = token.0,
                    Token::Eq(sp) => {
                        if token.0 != eq_location {
                            if bar == NONE {
                                return diagnostic_builder!(
                                    parse_context.input.clone(),
                                    "Found second `=` before `|`",
                                    *sp
                                );
                            }
                            rec_eq = token.0;
                            break;
                        }
                    }
                    Token::VertBar(sp) => {
                        if bar != NONE {
                            return diagnostic_builder!(
                                parse_context.input.clone(),
                                "Found second `|`",
                                *sp
                            )
                            .map_err(|mut x| {
                                x.note("only one `|` is allowed".to_string());
                                x
                            });
                        }
                        bar = token.0;
                    }
                    Token::Comma(_) => arguments_list.push(token.0),
                    Token::Op(_, _) => {}
                    Token::Literal(..) => {}
                    Token::Sign(_, _) => {}
                    Token::Other(sp, raw) => {
                        return diagnostic_builder!(
                            parse_context.input.clone(),
                            format!("Unexpected token `{}`", raw),
                            *sp
                        )
                    }
                    Token::Region(_, _) => panic!(),
                    Token::None => unreachable!(),
                }
            }
        }
        let mut action = Action::Eval;
        if eq_location != NONE {
            if brace_start != NONE {
                if brace_end == NONE {
                    return diagnostic_builder_spanned!(
                        parse_context.input.clone(),
                        "`(` at wrong location",
                        self.tokens.get(brace_start).unwrap().span()
                    );
                }
                let func_name = if let Token::Literal(_, x, _, kind) = self.tokens.remove(0) {
                    if kind == LiteralKind::CharSeq {
                        x
                    } else {
                        return diagnostic_builder!(
                            parse_context.input.clone(),
                            "No function name was given"
                        );
                    }
                } else {
                    return diagnostic_builder!(
                        parse_context.input.clone(),
                        "No function name was given"
                    );
                };
                let mut arguments = vec![];
                for x in 0..eq_location {
                    let token = self.tokens.remove(0);
                    if x % 2 == 1 {
                        if let Token::Literal(_, var, _, kind) = token {
                            if kind == LiteralKind::CharSeq {
                                arguments.push(var);
                            }
                        }
                    }
                }
                if eq_location
                    != ((1 + 1 + ((2 * (arguments.len() as isize)) - 1).max(0) + 1) as usize)
                {
                    // function name, `(`, function arguments with `,` but last argument has no `,` so - 1, `)`
                    // FIXME: Is this check actually required?
                    return diagnostic_builder!(
                        parse_context.input.clone(),
                        "`=` at wrong location"
                    );
                }
                action = Action::DefineFunc(func_name, arguments);
            } else if brace_end != NONE {
                return diagnostic_builder!(parse_context.input.clone(), "`)` at wrong location");
            } else {
                let var_name = if let Token::Literal(_, x, _, kind) = self.tokens.remove(0) {
                    if kind == LiteralKind::CharSeq {
                        x
                    } else {
                        return diagnostic_builder!(
                            parse_context.input.clone(),
                            "No function name was given"
                        );
                    }
                } else {
                    return diagnostic_builder!(
                        parse_context.input.clone(),
                        "No function name was given"
                    );
                };
                if eq_location != 1 {
                    return diagnostic_builder!(
                        parse_context.input.clone(),
                        "`=` at wrong location"
                    );
                }
                action = Action::DefineVar(var_name);
            }
        }
        self.action = action;
        match self.action.clone() {
            Action::DefineVar(name) => {
                // Perform variable and function lookup and evaluation
                self.handle_vars_and_fn_calls(parse_context)?;

                // Perform the evaluation of the variable definition
                let rpn = shunting_yard(self.tokens.clone(), parse_context)?;
                let result = eval_rpn(rpn, parse_context)?;
                parse_context.register_var(&name, result)?;
                Ok(Some(result))
            }
            Action::DefineFunc(name, args) => {
                let func = Function::new(
                    name.clone(),
                    args.clone(),
                    self.tokens.clone(),
                    parse_context,
                )?;
                parse_context.register_func(func);
                Ok(None)
            }
            Action::Eval => {
                // Perform variable and function lookup and evaluation
                self.handle_vars_and_fn_calls(parse_context)?;

                // Perform the evaluation of the input statement
                let rpn = shunting_yard(self.tokens.clone(), parse_context)?;
                let result = eval_rpn(rpn, parse_context)?;
                Ok(Some(result))
            }
            Action::DefineRecFunc(_, _, _) => {
                todo!()
            }
        }
    }
}

pub(crate) struct ParseContext {
    input: String,
    vars: HashMap<String, (bool, Number)>,
    funcs: HashMap<String, Function>,
    builtin_funcs: HashMap<String, BuiltInFunction>,
}

impl ParseContext {
    pub fn new() -> Self {
        let mut ret = Self {
            input: String::new(),
            vars: Default::default(),
            funcs: Default::default(),
            builtin_funcs: Default::default(),
        };
        register_const!(ret, "pi", Number::PI);
        register_const!(ret, "e", Number::E);
        register_builtin_func!(ret, "abs", 1, |nums| nums[0].abs());
        register_builtin_func!(ret, "sin", 1, |nums| nums[0].sin());
        register_builtin_func!(ret, "cos", 1, |nums| nums[0].cos());
        register_builtin_func!(ret, "tan", 1, |nums| nums[0].tan());
        register_builtin_func!(ret, "ln", 1, |nums| nums[0].ln());
        register_builtin_func!(ret, "round", 1, |nums| nums[0].round()); // FIXME: Should we even keep this one?
        register_builtin_func!(ret, "floor", 1, |nums| nums[0].floor());
        register_builtin_func!(ret, "ceil", 1, |nums| nums[0].ceil());
        register_builtin_func!(ret, "sqrt", 1, |nums| nums[0].sqrt().unwrap());
        register_builtin_func!(ret, "max", 2, |nums| nums[0].max(nums[1]));
        register_builtin_func!(ret, "min", 2, |nums| nums[0].min(nums[1]));
        ret
    }

    #[inline]
    pub fn set_input(&mut self, input: String) {
        self.input = input;
    }

    pub fn exists_const(&self, name: &String) -> bool {
        let name = name.to_lowercase();
        let con = self.vars.get(&*name);
        if let Some(con) = con {
            return !con.0;
        }
        false
    }

    pub fn lookup_const(&self, name: &String, parse_ctx: &ParseContext) -> PResult<Number> {
        let name = name.to_lowercase();
        let tmp = *self.vars.get(name.as_str()).unwrap();
        if !tmp.0 {
            return Ok(tmp.1);
        }
        diagnostic_builder!(
            parse_ctx.input.clone(),
            format!("`{}` is not a const", name)
        )
    }

    pub fn exists_var(&self, name: &String) -> bool {
        let name = name.to_lowercase();
        self.vars.contains_key(&*name)
    }

    pub fn lookup_var(&self, name: &String) -> Number {
        let name = name.to_lowercase();
        (*self.vars.get(name.as_str()).unwrap()).1
    }

    pub fn register_var(
        &mut self,
        name: &String,
        value: Number,
    ) -> Result<bool, DiagnosticBuilder> {
        let name = name.to_lowercase();
        if self.exists_fn(&name) {
            return diagnostic_builder!(
                self.input.clone(),
                format!("There is already a variable named `{}`", name)
            );
        }
        if let Some(x) = self.vars.get(name.as_str()) {
            if x.0 {
                self.vars.insert(name, (true, value));
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            self.vars.insert(name, (true, value));
            Ok(true)
        }
    }

    /// For internal use only!
    pub(crate) fn register_const(&mut self, name: &String, value: Number) -> bool {
        let name = name.to_lowercase();
        if let Some(x) = self.vars.get(name.as_str()) {
            if x.0 {
                self.vars.insert(name, (false, value));
                true
            } else {
                false
            }
        } else {
            self.vars.insert(name, (false, value));
            true
        }
    }

    pub fn exists_fn(&self, name: &String) -> bool {
        let name = name.to_lowercase();
        self.funcs.contains_key(&*name) || self.builtin_funcs.contains_key(&*name)
    }

    pub fn call_func(&self, name: &String, args: Vec<Vec<Token>>) -> PResult<Vec<Token>> {
        let name = name.to_lowercase();
        let func = self.funcs.get(&*name);
        match func {
            None => self.call_builtin_func(&name, args),
            Some(func) => func.build_tokens(args, self),
        }
    }

    pub fn register_func(&mut self, func: Function) {
        let name = func.name.to_lowercase();
        self.funcs.insert(name, func);
    }

    fn exists_builtin_func(&self, name: &String) -> bool {
        let name = name.to_lowercase();
        self.builtin_funcs.contains_key(&*name)
    }

    fn call_builtin_func(&self, name: &String, args: Vec<Vec<Token>>) -> PResult<Vec<Token>> {
        let func_name = name.to_lowercase();
        match self.builtin_funcs.get(&*func_name) {
            None => diagnostic_builder!(
                self.input.clone(),
                format!("There is no function such as `{}`.", name)
            ),
            Some(func) => func.build_tokens(args, self),
        }
    }

    fn register_builtin_func(&mut self, func: BuiltInFunction) {
        let name = func.name.to_lowercase();
        self.builtin_funcs.insert(name, func);
    }
}

pub(crate) fn shunting_yard(input: Vec<Token>, parse_ctx: &ParseContext) -> PResult<Vec<Token>> {
    let mut output = vec![];
    let mut operator_stack: Vec<OpKind> = vec![];
    for token in input {
        match token {
            Token::OpenParen(_) => operator_stack.push(OpKind::OpenParen),
            Token::ClosedParen(_) => loop {
                if let Some(op) = operator_stack.pop() {
                    if op == OpKind::OpenParen {
                        break;
                    }
                    output.push(Token::Op(usize::MAX, op));
                } else {
                    return diagnostic_builder_spanned!(
                        parse_ctx.input.clone(),
                        "Invalid parens!",
                        token.span()
                    );
                }
            },
            Token::Eq(_) => {}
            Token::VertBar(_) => {}
            Token::Comma(_) => {}
            Token::Op(_, op) => {
                while !operator_stack.is_empty()
                    && (op.precedence() < operator_stack.last().unwrap().precedence()
                        || (op.precedence() == operator_stack.last().unwrap().precedence()
                            && op.associativity() == Associativity::Left))
                {
                    output.push(Token::Op(usize::MAX, operator_stack.pop().unwrap()));
                }
                operator_stack.push(op);
            }
            Token::Literal(sp, _, _, kind) => {
                if kind == LiteralKind::CharSeq {
                    return diagnostic_builder_spanned!(
                        parse_ctx.input.clone(),
                        "Failed to evaluate literal correctly",
                        sp
                    );
                } else {
                    output.push(token);
                }
            }
            Token::Sign(sp, _) => {
                return diagnostic_builder!(
                    parse_ctx.input.clone(),
                    "There was no sign expected",
                    sp
                );
            }
            Token::Other(sp, _) => {
                return diagnostic_builder!(
                    parse_ctx.input.clone(),
                    "Other is not allowed here",
                    sp
                );
            }
            Token::Region(_, _) => panic!(),
            Token::None => unreachable!(),
        }
    }
    for operator in operator_stack.into_iter().rev() {
        output.push(Token::Op(usize::MAX, operator));
    }
    Ok(output)
}

pub(crate) fn eval_rpn(input: Vec<Token>, parse_ctx: &ParseContext) -> PResult<Number> {
    let mut num_stack: Vec<Number> = vec![];
    for token in input {
        match token {
            Token::OpenParen(_) => unreachable!(),
            Token::ClosedParen(_) => unreachable!(),
            Token::Eq(sp) => {
                return diagnostic_builder!(parse_ctx.input.clone(), "`=` at wrong location", sp);
            }
            Token::VertBar(_) => {}
            Token::Comma(sp) => {
                return diagnostic_builder!(parse_ctx.input.clone(), "`,` at wrong location", sp);
            }
            Token::Op(_, op) => match op {
                OpKind::Plus => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 + num_2)
                }
                OpKind::Minus => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 - num_2)
                }
                OpKind::Divide => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 / num_2)
                }
                OpKind::Multiply => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 * num_2)
                }
                OpKind::Modulo => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 % num_2)
                }
                OpKind::Pow => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1.powd(num_2))
                }
                OpKind::OpenParen => unreachable!(),
            },
            Token::Literal(sp, lit, _, kind) => {
                if kind == LiteralKind::CharSeq {
                    return diagnostic_builder_spanned!(
                        parse_ctx.input.clone(),
                        format!("Failed to evaluate literal `{}` correctly", lit),
                        sp
                    );
                } else {
                    num_stack.push(lit.parse::<Number>().unwrap());
                }
            }
            Token::Region(_, _) => panic!(),
            Token::Sign(_, _) => unreachable!(),
            Token::Other(_, _) => unreachable!(),
            Token::None => unreachable!(),
        }
    }
    if num_stack.len() != 1 {
        return diagnostic_builder!(
            parse_ctx.input.clone(),
            "There is more than 1 number left after finishing evaluation"
        );
    }
    Ok(num_stack.pop().unwrap())
}

#[derive(Debug, Clone)]
pub enum Action {
    DefineVar(String),                                  // var name
    DefineFunc(String, Vec<String>),                    // function name, function arguments
    DefineRecFunc(String, Vec<String>, (usize, usize)), // function name, function arguments, (recursive min idx, recursive min val)
    Eval,
}

impl Action {
    pub fn kind(&self) -> ActionKind {
        match self {
            Action::DefineVar(_) => ActionKind::DefineVar,
            Action::DefineFunc(_, _) => ActionKind::DefineFunc,
            Action::Eval => ActionKind::Eval,
            Action::DefineRecFunc(_, _, _) => ActionKind::DefineRecFunc,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ActionKind {
    DefineVar,
    DefineFunc,
    DefineRecFunc,
    Eval,
}

pub(crate) struct Function {
    name: String,
    args: Vec<FunctionArg>,
    virtual_calls: Vec<(usize, String, Vec<Vec<Token>>)>, // FIXME: Use these for calls to builtin funcs (and fix them somehow)!
    tokens: Vec<Token>,
}

impl Function {
    pub fn new(
        name: String,
        arg_names: Vec<String>,
        mut tokens: Vec<Token>,
        parse_ctx: &ParseContext,
    ) -> PResult<Self> {
        // Perform constant replacement
        let mut replace_consts = vec![];
        let mut replace_fns = vec![];
        for token in tokens.iter().enumerate().rev() {
            if let Token::Literal(span, str, _, kind) = token.1 {
                if *kind == LiteralKind::CharSeq && !arg_names.contains(str) {
                    if parse_ctx.exists_const(str) {
                        replace_consts.push((token.0, parse_ctx.lookup_const(str, parse_ctx)));
                    } else if parse_ctx.exists_fn(str) && str != &name { // FIXME: Better detection for cyclic calls, support builtin calls!
                        replace_fns.push(token.0);
                    } else {
                        return diagnostic_builder_spanned!(
                            parse_ctx.input.clone(),
                            "Not an argument or const",
                            *span
                        );
                    }
                }
            }
        }
        for x in replace_consts {
            tokens[x.0] = Token::Literal(
                Span::NONE,
                x.1?.to_string(),
                SignKind::Plus,
                LiteralKind::Number,
            );
            // TODO: Detect sign correctly!
        }
        let mut built_in = vec![];
        for repl in replace_fns {
            if let Token::OpenParen(_) = tokens.get(repl + 1).unwrap() {
                tokens.remove(repl + 1);
                let mut open = 1;
                let mut args = vec![vec![]];
                while open > 0 {
                    let token = tokens.remove(repl + 1);
                    match token {
                        Token::OpenParen(_) => open += 1,
                        Token::ClosedParen(_) => open -= 1,
                        Token::Eq(_) => panic!(),
                        Token::VertBar(_) => panic!(),
                        Token::Comma(_) => {
                            if open == 1 {
                                args.push(vec![]);
                            } else {
                                panic!()
                            }
                            continue;
                        }
                        Token::Op(_, _) => {}, // NOOP
                        Token::Literal(_, _, _, _) => {}, // NOOP
                        Token::Region(_, _) => panic!(),
                        Token::Sign(_, _) => panic!(), // FIXME: Is panicking here correct?
                        Token::Other(_, _) => panic!(),
                        Token::None => unreachable!(),
                    }
                    if open != 0 {
                        args.last_mut().unwrap().push(token.clone());
                    }
                }
                let lit = if let Token::Literal(_, lit, ..) = tokens.get(repl).unwrap() {
                    lit
                } else {
                    panic!()
                };
                if !parse_ctx.exists_builtin_func(lit) {
                    let mut result = parse_ctx.call_func(lit, args);
                    tokens.remove(repl);
                    for token in result.unwrap().into_iter().enumerate() {
                        tokens.insert(repl + token.0, token.1);
                    }
                } else {
                    built_in.push((tokens.len() - repl, lit.clone(), args));
                }
            } else {
                panic!()
            }
        }
        // Detect arguments
        let mut finished_args = vec![];
        for arg in arg_names.iter() {
            let mut arg_positions = vec![];
            for token in tokens.iter().enumerate() {
                if let Token::Literal(_, str, _, kind) = token.1 {
                    if *kind == LiteralKind::CharSeq && str == arg {
                        arg_positions.push(token.0);
                    }
                }
            }
            finished_args.push(FunctionArg::new(arg_positions));
        }

        Ok(Self {
            name,
            args: finished_args,
            virtual_calls: built_in,
            tokens,
        })
    }

    pub fn build_tokens(
        &self,
        arg_values: Vec<Vec<Token>>,
        parse_ctx: &ParseContext,
    ) -> PResult<Vec<Token>> {
        if self.args.len() != arg_values.len() {
            let args_txt = if self.args.len() == 1 {
                "argument"
            } else {
                "arguments"
            };
            return diagnostic_builder!(
                parse_ctx.input.clone(),
                format!(
                    "Expected {} {}, got {}",
                    self.args.len(),
                    args_txt,
                    arg_values.len()
                )
            );
        }
        let mut ret = self.tokens.clone();
        let mut offset = 0;
        for arg in self.args.iter().enumerate() {
            offset += arg.1.build(&arg_values[arg.0], &mut ret, offset);
        }
        Ok(ret)
    }
}

pub(crate) struct BuiltInFunction {
    name: String,
    arg_count: usize,
    inner: Box<dyn Fn(Vec<Number>) -> Number>,
}

impl BuiltInFunction {
    pub fn new(name: String, arg_count: usize, inner: Box<dyn Fn(Vec<Number>) -> Number>) -> Self {
        Self {
            name,
            arg_count,
            inner,
        }
    }

    pub fn build_tokens(
        &self,
        arg_values: Vec<Vec<Token>>,
        parse_ctx: &ParseContext,
    ) -> PResult<Vec<Token>> {
        if self.arg_count != arg_values.len() {
            let args_txt = if self.arg_count == 1 {
                "argument"
            } else {
                "arguments"
            };
            return diagnostic_builder!(
                parse_ctx.input.clone(),
                format!(
                    "Expected {} {}, got {}",
                    self.arg_count,
                    args_txt,
                    arg_values.len()
                )
            );
        }
        let mut args = vec![];
        for arg in arg_values.iter() {
            let rpn = shunting_yard(arg.clone(), parse_ctx)?;
            let result = eval_rpn(rpn, parse_ctx)?;
            args.push(result);
        }
        Ok(vec![Token::Literal(
            Span::NONE,
            (self.inner)(args).to_string(),
            SignKind::Plus, // TODO: Detect sign correctly!
            LiteralKind::Number,
        )])
    }
}

pub(crate) struct FunctionArg {
    pub(crate) token_positions: Vec<usize>,
}

impl FunctionArg {
    pub(crate) fn new(token_positions: Vec<usize>) -> Self {
        Self { token_positions }
    }

    pub(crate) fn build(
        &self,
        value: &Vec<Token>,
        tokens: &mut Vec<Token>,
        offset: usize,
    ) -> usize {
        let added_tokens = value.len() + 1;
        for pos in self.token_positions.iter() {
            let pos = pos + offset;
            tokens[pos] = Token::OpenParen(usize::MAX);
            let end = pos + 1 + value.len().clone();
            for token in value.into_iter().enumerate() {
                tokens.insert(pos + 1 + token.0, token.1.clone());
            }
            tokens.insert(end, Token::ClosedParen(usize::MAX));
        }
        added_tokens
    }
}

fn parse_braced_call_region(tokens: &Vec<Token>, parse_start: usize) -> Result<Region, DiagnosticBuilder> {
    let mut start = usize::MAX;
    let mut end = usize::MAX;
    let mut open = 0;
    for x in tokens.iter().enumerate().skip(parse_start) {
        if let Token::OpenParen(_) = x.1 {
            start = x.0;
            open += 1;
        } else if let Token::ClosedParen(_) = x.1 {
            open -= 1;
            if open == 0 {
                end = x.0;
                break;
            }
        }
    }
    if open != 0 {
        panic!()
    }
    Ok(Region::new(start, end, tokens))
}

struct Region {
    start: usize,
    end: usize,
    inner_span: Span,
}

impl Region {
    fn new(start: usize, end: usize, tokens: &Vec<Token>) -> Self {
        let inner_start = tokens.get(start).unwrap().span().start();
        let inner_end = tokens.get(end).unwrap().span().end();
        Self {
            start,
            end,
            inner_span: Span::new(inner_start, inner_end),
        }
    }

    fn replace_in_tokens(self, tokens: &mut Vec<Token>) {
        let mut inner_tokens = vec![];
        for _ in self.start..self.end {
            inner_tokens.push(tokens.remove(self.start));
        }
        tokens.insert(self.start, self.to_token(inner_tokens));
    }

    fn to_token(self, tokens: Vec<Token>) -> Token {
        Token::Region(self.inner_span, tokens)
    }

    fn pop_braces(&mut self, tokens: &mut Vec<Token>) {
        if let Token::OpenParen(_) = tokens.get(self.start).unwrap() {
            tokens.remove(self.start);
            self.end -= 1;
        }
        if let Token::ClosedParen(_) = tokens.get(self.end).unwrap() {
            tokens.remove(self.end);
            self.end -= 1;
        }
    }

    fn partition_by_comma(&mut self, tokens: &mut Vec<Token>) {
        let braced_start = if let Token::OpenParen(_) = tokens.get(0).unwrap() {
            1
        } else {
            0
        };
        let mut open = 0;
        let mut partitions = vec![];
        let mut partition_start = 0;
        for x in tokens.iter().skip(self.start).enumerate() {
            if x.0 >= (self.end - self.start) {
                break;
            }
            if let Token::OpenParen(_) = x.1 {
                open += 1;
            } else if let Token::ClosedParen(_) = x.1 {
                open -= 1;
            } else if let Token::Comma(_) = x.1 {
                if open == braced_start {
                    partitions.push((partition_start, x.0 - 1));
                    partition_start = x.0 + 1;
                }
            }
        }
        let mut neg_offset = 0;
        for part in partitions {
            let mut partition = vec![];
            let start = tokens.get(part.0 - neg_offset).unwrap().span().start();
            let end = tokens.get(part.1 - neg_offset).unwrap().span().end();
            for _ in 0..(part.1 - part.0) {
                partition.push(tokens.remove(part.0 - neg_offset));
            }
            tokens.insert(part.0 - neg_offset, Token::Region(Span::new(start, end), partition));
            neg_offset += part.1 - part.0;
        }
    }
}

pub type PResult<T> = Result<T, DiagnosticBuilder>;

mod macros {
    #[macro_export]
    macro_rules! register_builtin_func {
        ($ctx:ident, $name:literal, $arg_count:literal, $func:expr/*tt*/) => {
            $ctx.register_builtin_func(BuiltInFunction::new(
                $name.to_string(),
                $arg_count,
                Box::new($func),
            ));
        };
    }

    #[macro_export]
    macro_rules! register_const {
        ($ctx:ident, $name:literal, $value:expr) => {
            let tmp = String::from($name);
            $ctx.register_const(&tmp, $value);
        };
    }
}
