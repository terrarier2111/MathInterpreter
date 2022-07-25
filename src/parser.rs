use crate::ast::{AstNode, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, UnaryOpNode};
use crate::ast_walker::{AstWalker, AstWalkerMut, LitWalker, LitWalkerMut};
use crate::equation_evaluator::EvalWalker;
use crate::equation_simplifier::simplify;
use crate::error::DiagnosticBuilder;
use crate::shared::{
    num_to_num_and_sign, Associativity, BinOpKind, ImplicitlyMultiply, LiteralKind, LiteralToken,
    Number, SignKind, Token, TokenKind, TrailingSpace,
};
use crate::span::Span;
use crate::token_stream::TokenStream;
use crate::{
    _lib, diagnostic_builder, diagnostic_builder_spanned, equation_evaluator, pluralize,
    register_builtin_func, register_const, ANSMode, Config, DiagnosticsConfig, Mode,
};
use rust_decimal::MathematicalOps;
use std::collections::HashMap;
use std::ops::Neg;
use std::rc::Rc;

const NONE: usize = usize::MAX;

pub(crate) struct Parser<'a> {
    token_stream: TokenStream,
    parse_ctx: &'a mut ParseContext,
    ans_mode: ANSMode,
    curr: Token,
    action: Action,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, parse_ctx: &'a mut ParseContext, ans_mode: ANSMode) -> Self {
        let mut token_stream = TokenStream::new(tokens);
        let curr = token_stream.get_next_and_advance().unwrap().clone();
        Self {
            token_stream,
            parse_ctx,
            ans_mode,
            curr,
            action: Action::Eval,
        }
    }

    pub(crate) fn parse(&mut self, mode: Mode) -> ParseResult<Option<Number>> {
        fn tokens_to_string(tokens: &Box<[Token]>) -> String {
            let mut result = String::new();
            for token in tokens.iter() {
                result.push_str(token.to_raw().as_str());
            }
            result
        }
        // Make implicit multiplications explicit!
        println!("parse 0");
        let mut last_token_mult = ImplicitlyMultiply::Never;
        let mut multiplications = vec![];
        let mut idx = 0;
        let eq_idx = {
            let mut idx: Option<usize> = None;
            while !self.check_binop(BinOpKind::Eq) {
                if self.check(TokenKind::EOF) {
                    idx = None;
                    break;
                }
                self.advance();
                match idx {
                    None => {
                        idx = Some(1);
                    }
                    Some(mut idx) => {
                        idx += 1;
                    }
                }
            }
            idx
        };
        self.reset();
        if let Some(eq_idx) = eq_idx {
            idx = eq_idx + 1;
            self.token_stream.skip(eq_idx);
            self.advance();
        }

        println!("check eof");
        while !self.eat(TokenKind::EOF) {
            let curr_token_mult = if let Some(lit) = self.parse_lit() {
                if self.parse_ctx.exists_fn(&lit.content)
                    || self.parse_ctx.exists_builtin_func(&lit.content)
                {
                    ImplicitlyMultiply::Never
                } else {
                    ImplicitlyMultiply::Always
                }
            } else {
                let ret = self.curr.implicitly_multiply_left();
                self.advance();
                ret
            };
            if curr_token_mult.can_multiply_with_left(last_token_mult) {
                multiplications.push(idx);
            }
            last_token_mult = curr_token_mult;
            idx += 1;
        }
        let mut tokens = self.token_stream.internal_tokens().into_vec();
        for x in multiplications.iter().enumerate() {
            tokens.insert(x.0 + *x.1, Token::BinOp(x.0, BinOpKind::Multiply));
        }
        self.token_stream
            .replace_internal_tokens(tokens.into_boxed_slice());
        self.reset();

        println!(
            "tokens: {}",
            tokens_to_string(&self.token_stream.internal_tokens())
        );

        println!("getting head!");
        let head_expr = match self.parse_expr() {
            Ok(val) => val,
            Err(err) => {
                return ParseResult::new_err(err);
            }
        };
        println!("do stuff!");

        match mode {
            Mode::Eval => {
                println!("evaluating!");
                let val = equation_evaluator::eval(&mut self.parse_ctx, head_expr);
                match val {
                    Ok(val) => ParseResult::new_ok(val),
                    Err(err) => ParseResult::new_err(err),
                }
            }
            Mode::Simplify => ParseResult(self.parse_simplify().0.map(|val| (None, val.1))),
            Mode::Solve => unimplemented!(),
        }
    }

    fn parse_simplify(&mut self) -> ParseResult<()> {
        // Simplify the statement
        let simplified = match simplify(
            self.parse_ctx.input.clone(),
            self.token_stream.internal_tokens().to_vec(),
        ) {
            Ok(val) => val,
            Err(err) => return ParseResult::new_err(err),
        };

        /// Used for debugging, transforms a list of tokens
        /// to their string representation
        fn tokens_to_string(tokens: &Vec<Token>) -> String {
            let mut result = String::new();
            for token in tokens.iter() {
                result.push_str(token.to_raw().as_str());
            }
            result
        }

        let result = tokens_to_string(&simplified);
        println!("{}", result);
        self.parse_ctx.input = result;
        ParseResult(Ok(((), None)))
    }

    fn try_parse_function(&mut self) -> PResult<Option<AstNode>> {
        if let Some(lit) = self.parse_lit() {
            if lit.trailing_space != TrailingSpace::Yes && self.eat(TokenKind::OpenParen) {
                let mut params = vec![];
                while !self.check(TokenKind::ClosedParen) {
                    let param = self.parse_expr()?;
                    params.push(param);

                    if !self.eat(TokenKind::Comma) {
                        // FIXME: should we error if there is a trailing comma?
                        break;
                    }
                }

                if !self.eat(TokenKind::ClosedParen) {
                    return diagnostic_builder_spanned!(
                        self.parse_ctx.input.clone(),
                        "expected `)`",
                        Span::single_token(self.curr.span().end) // FIXME: should we add 1 to the passed value?
                    );
                }

                if params.len() > 1 {
                    Ok(Some(AstNode::FuncCallOrFuncDef(FuncCallOrFuncDefNode {
                        name: lit.content,
                        params: params.into_boxed_slice(),
                    })))
                } else {
                    Ok(Some(AstNode::MaybeFunc(MaybeFuncNode {
                        name: lit.content,
                        param: params.pop().map(|x| Box::new(x)),
                    })))
                }
            } else {
                self.go_back();
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn parse_expr(&mut self) -> PResult<AstNode> {
        /*match &self.curr {
            Token::OpenParen(_) => self.parse_paren_expr(),
            // Token::ClosedParen(_) => {}
            // Token::VertBar(_) => {}
            // Token::Comma(_) => {}
            // Token::Op(_, _) => {}
            Token::Literal(_) => {
                println!("curr: {}", self.curr);
                if let Some(func) = self.try_parse_function()? {
                    Ok(func)
                } else if self
                    .token_stream
                    .look_ahead(1, |x| x.kind() == TokenKind::BinOp)
                {
                    println!("curr: {}", self.curr);
                    self.parse_binop()
                } else {
                    Ok(AstNode::Lit(self.parse_lit().unwrap()))
                }
            }
            // Token::Region(_, _) => {}
            _ => diagnostic_builder_spanned!(
                self.parse_ctx.input.clone(),
                format!("expected expression, found {}", self.curr.to_raw()),
                self.curr.span()
            ),
        }*/
        self.parse_binop()
    }

    fn parse_binop(&mut self) -> PResult<AstNode> {
        println!("parsing binop!");
        let lhs = self.parse_primary()?;
        println!("doing other binop stuff!");
        self.parse_bin_op_rhs(0, lhs)
    }

    fn parse_bin_op_rhs(&mut self, prec: u8, mut lhs: AstNode) -> PResult<AstNode> {
        loop {
            let bin_op = if let Token::BinOp(_, bin_op) = &self.curr {
                Some(*bin_op)
            } else {
                None
            };

            // If this is a binop that binds at least as tightly as the current binop,
            // consume it, otherwise we are done.
            if let Some(bin_op) = bin_op {
                println!("checking stuff!");
                if bin_op.precedence() < prec {
                    return Ok(lhs);
                }
            } else {
                return Ok(lhs);
            }
            let bin_op = bin_op.unwrap();
            self.eat(TokenKind::BinOp);

            println!("parsing rhs..!");
            let mut rhs = Some(self.parse_primary()?);
            println!("rhs: {:?}", rhs);
            // let last_mult = self.token_stream.look_back(1, |token| token.implicitly_multiply_left());

            // If BinOp binds less tightly with RHS than the operator after RHS, let
            // the pending operator take RHS as its LHS.
            let /*mut */next_bin_op = if let Token::BinOp(_, bin_op) = &self.curr {
                Some(*bin_op)
            } else {
                None
            };

            /*if next_bin_op.is_none() {
                if self.ans_mode == ANSMode::Always || (self.ans_mode == ANSMode::WhenImplicit &&
                    last_mult.map_or(false, |last_mult| self.curr.implicitly_multiply_left().can_multiply_with_left(last_mult))) {
                    // FIXME: all these conversions from/to boxed slices seem pretty inefficient, should we go back to using only a simple vec?
                    let mut curr_toks = self.token_stream.internal_tokens().into_vec();
                    curr_toks.insert(self.token_stream.cursor(), Token::BinOp(NONE, BinOpKind::Multiply));
                    self.token_stream.replace_internal_tokens(curr_toks.into_boxed_slice());
                    // we now can pretend like we found a multiply token all along
                    next_bin_op = Some(BinOpKind::Multiply);
                }
            }*/

            match next_bin_op {
                None => {
                    return Ok(AstNode::BinOp(BinOpNode {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs.take().unwrap()),
                        op: bin_op,
                    }));
                }
                Some(next_bin_op) => {
                    if bin_op.precedence() < next_bin_op.precedence() {
                        rhs = rhs.map(|rhs| {
                            self.parse_bin_op_rhs(bin_op.precedence() + 1, rhs).unwrap()
                        });
                    }
                    lhs = AstNode::BinOp(BinOpNode {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs.take().unwrap()),
                        op: bin_op,
                    });
                }
            }
        }
    }

    fn parse_paren_expr(&mut self) -> PResult<AstNode> {
        if !self.eat(TokenKind::OpenParen) {
            return diagnostic_builder_spanned!(
                self.parse_ctx.input.clone(),
                "expected `{`",
                self.curr.span()
            );
        }
        let expr = self.parse_expr()?;

        if !self.eat(TokenKind::ClosedParen) {
            return diagnostic_builder_spanned!(
                self.parse_ctx.input.clone(),
                "expected `}`",
                self.curr.span()
            );
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> PResult<AstNode> {
        println!("parsing primary!");
        match &self.curr {
            Token::OpenParen(_) => self.parse_paren_expr(),
            // Token::VertBar(_) => {} // FIXME: how are we supposed to handle this?
            // Token::Literal(_) => self.parse_binop(),
            Token::Literal(_) => {
                // FIXME: check if the literal code is okay
                println!("curr: {}", self.curr);
                if let Some(func) = self.try_parse_function()? {
                    Ok(func)
                } else {
                    Ok(AstNode::Lit(self.parse_lit().unwrap()))
                }
            }
            // Token::Region(_, _) => {} // FIXME: how are we supposed to handle this?
            _ => diagnostic_builder_spanned!(
                self.parse_ctx.input.clone(),
                format!(
                    "expected primary, found {} of type {:?}",
                    self.curr.to_raw(),
                    self.curr.kind()
                ),
                self.curr.span()
            ),
        }
    }

    /*
    fn parse_primary(&mut self) -> Result<AstNode, ()> {
        println!("curr: {:?}", self.curr);
        match &self.curr {
            Token::Ident(_, content) => {
                if self
                    .token_stream
                    .look_ahead(1, |token| token.to_type() == TokenType::OpenParen)
                {
                    self.parse_call() // FIXME: handle errors properly!
                                      /*} else if self.token_stream.look_ahead(1, |token| token.to_type() == TokenType::Dot) {
                                      // FIXME: parse field access/struct method call
                                       */
                } else if self
                    .token_stream
                    .look_ahead(1, |token| token.to_type() == TokenType::OpenCurly)
                {
                    self.parse_struct_constructor()
                } else {
                    // FIXME: handle the rest!
                    let content = content.clone();
                    self.advance();
                    Ok(AstNode::Ident(content))
                }
            }
            //#!Token::Keyword(_, _) => {}
            // Token::StrLit(_, _) => {}
            Token::NumLit(_, _) => self.parse_number_expr(),
            Token::OpenParen(_) => self.parse_paren_expr(),
            Token::OpenBracket(_) => self.parse_array_constructor(),
            //#!Token::OpenCurly(_) => {}
            // Token::OpenBracket(_) => {}
            // Token::Eq(_) => {}
            // Token::Colon(_) => {}
            //#!Token::Semi(_) => {}
            // Token::Apostrophe(_) => {}
            // Token::OpenAngle(_) => {}
            // Token::Star(_) => {}
            // Token::Question(_) => {}
            // Token::Underscore(_) => {}
            // Token::Comment(_, _) => Ok(None), // FIXME: this is currently filtered in the tokenstream
            Token::EOF(_) => Err(()),
            _ => Err(()),
        }
    }

    fn parse_bin_op(&mut self) -> Result<AstNode, ()> {
        let lhs = self.parse_primary()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    fn parse_bin_op_rhs(&mut self, prec: usize, mut lhs: AstNode) -> Result<AstNode, ()> {
        // If this is a binop, find its precedence.
        loop {
            let bin_op = if let Token::BinOp(_, bin_op) = &self.curr {
                Some(*bin_op)
            } else {
                None
            };

            // If this is a binop that binds at least as tightly as the current binop,
            // consume it, otherwise we are done.
            if let Some(bin_op) = bin_op {
                if bin_op.precedence() < prec {
                    return Ok(lhs);
                }
            } else {
                return Ok(lhs);
            }
            let bin_op = bin_op.unwrap();
            self.eat(TokenType::BinOp);

            let mut rhs = Some(self.parse_primary()?);

            if rhs.is_some() {
                // If BinOp binds less tightly with RHS than the operator after RHS, let
                // the pending operator take RHS as its LHS.
                let next_bin_op = if let Token::BinOp(_, bin_op) = &self.curr {
                    Some(*bin_op)
                } else {
                    None
                };

                if let Some(next_bin_op) = next_bin_op {
                    if bin_op.precedence() < next_bin_op.precedence() {
                        rhs = rhs.map(|rhs| {
                            self.parse_bin_op_rhs(bin_op.precedence() + 1, rhs).unwrap()
                        });
                        if rhs.is_none() {
                            return Err(()); // FIXME: is this correct?
                        }
                    }
                } else {
                    return Ok(AstNode::BinaryExpr(Box::new(BinaryExprNode {
                        lhs,
                        rhs: rhs.take().unwrap(),
                        op: bin_op,
                    })));
                }

                lhs = AstNode::BinaryExpr(Box::new(BinaryExprNode {
                    lhs,
                    rhs: rhs.take().unwrap(),
                    op: bin_op,
                }))
            }
        }
    }
    */

    fn parse_lit(&mut self) -> Option<LiteralToken> {
        if let Token::Literal(lit) = &self.curr {
            let ret = Some(lit.clone());
            self.advance();
            ret
        } else {
            None
        }
    }

    fn check(&self, token: TokenKind) -> bool {
        self.curr.kind() == token
    }

    fn eat(&mut self, token: TokenKind) -> bool {
        if self.curr.kind() == token {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check_binop(&self, binop: BinOpKind) -> bool {
        if let Token::BinOp(_, kind) = &self.curr {
            *kind == binop
        } else {
            false
        }
    }

    fn eat_binop(&mut self, binop: BinOpKind) -> bool {
        if let Token::BinOp(_, kind) = &self.curr {
            if *kind == binop {
                self.advance();
                return true;
            }
        }
        false
    }

    fn advance(&mut self) {
        if let Some(next) = self.token_stream.get_next() {
            self.curr = next.clone();
        }
        self.token_stream.advance();
    }

    fn go_back(&mut self) {
        // we need to go back twice in order for us to stay 1 token behind the parser's current token
        self.token_stream.go_back();
        self.token_stream.go_back();
        self.advance();
    }

    fn reset(&mut self) {
        self.token_stream.reset();
        let curr = self.token_stream.get_next_and_advance().unwrap().clone();
        self.curr = curr;
    }
}

pub(crate) struct ParseContext {
    input: String,
    last_result: Option<Number>,
    vars: HashMap<String, (bool, Number)>,
    funcs: HashMap<String, Function>,
    builtin_funcs: HashMap<String, BuiltInFunction>,
}

impl ParseContext {
    pub fn new() -> Self {
        let mut ret = Self {
            input: String::new(),
            last_result: None,
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
        register_builtin_func!(ret, "deg", 1, |nums| nums[0]
            * (Number::from(180) / Number::PI));
        register_builtin_func!(ret, "rad", 1, |nums| nums[0]
            * (Number::PI / Number::from(180)));
        ret
    }

    #[inline]
    pub(crate) fn set_input(&mut self, input: String) {
        self.input = input;
    }

    #[inline]
    pub fn get_input(&self) -> &String {
        &self.input
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

    pub fn lookup_var(&self, name: &String) -> Option<Number> {
        let name = name.to_lowercase();
        self.vars.get(name.as_str()).map(|x| x.1.clone())
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

    pub fn try_call_func(&self, name: &String, args: Box<[AstNode]>) -> Option<PResult<AstNode>> {
        let name = name.to_lowercase();
        let func = self.funcs.get(&*name);
        match func {
            None => {
                let result = self.call_builtin_func(&name, args);
                result.map(|pr| {
                    pr.map(|num| {
                        AstNode::Lit(LiteralToken {
                            span: Span::NONE,
                            content: num.to_string(),
                            sign: SignKind::Default,
                            kind: LiteralKind::Number,
                            trailing_space: TrailingSpace::Maybe,
                        })
                    })
                })
            }
            Some(func) => Some(func.build_tokens(args, self)),
        }
    }

    pub fn register_func(&mut self, func: Function) {
        let name = func.name.to_lowercase();
        self.funcs.insert(name, func);
    }

    pub(crate) fn exists_builtin_func(&self, name: &String) -> bool {
        let name = name.to_lowercase();
        self.builtin_funcs.contains_key(&*name)
    }

    fn call_builtin_func(&self, name: &String, args: Box<[AstNode]>) -> Option<PResult<Number>> {
        let func_name = name.to_lowercase();
        match self.builtin_funcs.get(&*func_name) {
            None =>
            /*diagnostic_builder!(
                self.input.clone(),
                format!("There is no function such as `{}`.", name) // FIXME: emmit this error message somewhere else!
            )*/
            {
                None
            }
            Some(func) => Some(func.build_tokens(args, self)),
        }
    }

    fn register_builtin_func(&mut self, func: BuiltInFunction) {
        let name = func.name.to_lowercase();
        self.builtin_funcs.insert(name, func);
    }
}

/*
pub(crate) fn shunting_yard(input: Vec<Token>, parse_ctx: &ParseContext) -> PResult<Vec<Token>> {
    let mut output = vec![];
    let mut operator_stack: Vec<BinOpKind> = vec![];
    for token in input {
        match &token {
            Token::OpenParen(_) => operator_stack.push(BinOpKind::OpenParen),
            Token::ClosedParen(_) => loop {
                if let Some(op) = operator_stack.pop() {
                    if op == BinOpKind::OpenParen {
                        break;
                    }
                    output.push(Token::BinOp(usize::MAX, op));
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
            Token::BinOp(_, op) => {
                while !operator_stack.is_empty()
                    && (op.precedence() < operator_stack.last().unwrap().precedence()
                        || (op.precedence() == operator_stack.last().unwrap().precedence()
                            && op.associativity() == Associativity::Left))
                {
                    output.push(Token::BinOp(usize::MAX, operator_stack.pop().unwrap()));
                }
                operator_stack.push(*op);
            }
            Token::Literal(lit_tok) => {
                if lit_tok.kind == LiteralKind::CharSeq {
                    return diagnostic_builder_spanned!(
                        parse_ctx.input.clone(),
                        "Failed to evaluate literal correctly",
                        lit_tok.span
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
        output.push(Token::BinOp(usize::MAX, operator));
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
            Token::BinOp(_, op) => match op {
                BinOpKind::Add => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 + num_2)
                }
                BinOpKind::Subtract => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 - num_2)
                }
                BinOpKind::Divide => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 / num_2)
                }
                BinOpKind::Multiply => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 * num_2)
                }
                BinOpKind::Modulo => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1 % num_2)
                }
                BinOpKind::Pow => {
                    let num_2 = num_stack.pop().unwrap();
                    let num_1 = num_stack.pop().unwrap();
                    num_stack.push(num_1.powd(num_2))
                }
                BinOpKind::OpenParen => unreachable!(),
            },
            Token::Literal(lit_tok) => {
                if lit_tok.kind == LiteralKind::CharSeq {
                    return diagnostic_builder_spanned!(
                        parse_ctx.input.clone(),
                        format!("Failed to evaluate literal `{}` correctly", lit_tok.content),
                        lit_tok.span
                    );
                } else {
                    let mut num = lit_tok.content.parse::<Number>().unwrap();
                    if lit_tok.sign == SignKind::Minus {
                        num = num.neg();
                    }
                    num_stack.push(num);
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
}*/

#[derive(Debug, Clone)]
pub enum Action {
    DefineVar(String),                                    // var name
    DefineFunc(String, Box<[String]>),                    // function name, function arguments
    DefineRecFunc(String, Box<[String]>, (usize, usize)), // function name, function arguments, (recursive min idx, recursive min val)
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

struct FunctionWalker<'a> {
    parse_ctx: &'a ParseContext,
    arg_replacements: HashMap<String, Number>,
}

impl LitWalkerMut for FunctionWalker<'_> {
    fn walk_lit(&self, node: &mut LiteralToken) -> Result<(), DiagnosticBuilder> {
        if node.kind == LiteralKind::CharSeq {
            if let Some(val) = self.arg_replacements.get(&node.content) {
                node.kind = LiteralKind::Number;
                node.content = val.to_string();
            }
        }
        Ok(())
    }
}

struct FunctionInitValidator<'a> {
    parse_ctx: &'a ParseContext,
    arg_names: &'a Box<[String]>,
    func_name: &'a String,
}

impl LitWalker for FunctionInitValidator<'_> {
    fn walk_lit(&self, lit_tok: &LiteralToken) -> Result<(), DiagnosticBuilder> {
        if lit_tok.kind == LiteralKind::CharSeq
            && !self.arg_names.contains(&lit_tok.content)
            && !self.parse_ctx.exists_const(&lit_tok.content)
            && (!self.parse_ctx.exists_fn(&lit_tok.content) || &lit_tok.content == self.func_name)
        {
            return diagnostic_builder_spanned!(
                self.parse_ctx.input.clone(),
                "Not an argument or const",
                lit_tok.span
            );
        }
        Ok(())
    }
}

pub(crate) struct Function {
    name: String,
    ast: AstNode,
    args: Box<[String]>,
}

impl Function {
    pub fn new(
        name: String,
        arg_names: Box<[String]>,
        structure: AstNode,
        parse_ctx: &ParseContext,
    ) -> PResult<Self> {
        // verify arguments are valid
        let validator = FunctionInitValidator {
            parse_ctx,
            arg_names: &arg_names,
            func_name: &name,
        };
        validator.walk(&structure)?;

        Ok(Self {
            name,
            ast: structure,
            args: arg_names,
        })
    }

    pub fn build_tokens(
        &self,
        arg_values: Box<[AstNode]>,
        parse_ctx: &ParseContext,
    ) -> PResult<AstNode> {
        if self.args.len() != arg_values.len() {
            let args_txt = if self.args.len() == 1 {
                "argument"
            } else {
                "arguments"
            };
            return diagnostic_builder!(
                parse_ctx.input.clone(),
                format!(
                    "expected {} {}, got {}",
                    self.args.len(),
                    args_txt,
                    arg_values.len()
                )
            );
        }
        let mut arg_replacements = HashMap::new();
        for val in arg_values.into_iter().enumerate() {
            let eval_walker = EvalWalker { ctx: parse_ctx };
            arg_replacements.insert(self.args[val.0].clone(), eval_walker.walk(val.1)?);
        }

        let mut walker = FunctionWalker {
            parse_ctx,
            arg_replacements,
        };

        let mut result = self.ast.clone();
        walker.walk(&mut result)?;
        Ok(result)
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
        arg_values: Box<[AstNode]>,
        parse_ctx: &ParseContext,
    ) -> PResult<Number> {
        if self.arg_count != arg_values.len() {
            let args_txt = pluralize!(self.arg_count);
            return diagnostic_builder!(
                parse_ctx.input.clone(),
                format!(
                    "Expected {} argument{}, got {}",
                    self.arg_count,
                    args_txt,
                    arg_values.len()
                )
            );
        }
        let mut args = vec![];
        for arg in arg_values.iter() {
            /*let rpn = shunting_yard(arg.clone(), parse_ctx)?;
            let result = eval_rpn(rpn, parse_ctx)?;
            args.push(result);*/
            let eval_walker = EvalWalker { ctx: parse_ctx };
            let walked = eval_walker.walk(arg);
            args.push(walked?);
        }
        // let (result, sign) = num_to_num_and_sign((self.inner)(args));
        let result = (self.inner)(args);
        Ok(result)
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

#[repr(transparent)]
pub struct ParseResult<T>(pub(crate) Result<(T, Option<DiagnosticBuilder>), DiagnosticBuilder>);

impl<T: Default> Default for ParseResult<T> {
    fn default() -> Self {
        Self(Ok((T::default(), None)))
    }
}

impl<T> ParseResult<T> {
    #[inline]
    pub(crate) fn new_err(diagnostics_builder: DiagnosticBuilder) -> ParseResult<T> {
        ParseResult(Err(diagnostics_builder))
    }

    #[inline]
    pub(crate) fn new_ok(val: T) -> ParseResult<T> {
        ParseResult(Ok((val, None)))
    }

    pub fn diagnostics(&self) -> Option<&DiagnosticBuilder> {
        match &self.0 {
            Ok(val) => val.1.as_ref(),
            Err(diagnostics) => Some(diagnostics),
        }
    }

    #[inline]
    pub(crate) fn unwrap(self) -> (T, Option<DiagnosticBuilder>) {
        self.0.unwrap()
    }

    // This is a temporary solution which we use until try_v2 gets stabilized!
    #[inline]
    fn to_wrapped(self) -> Result<ParseResult<T>, ParseResult<T>> {
        match self.0 {
            Ok(_) => Ok(self),
            Err(_) => Err(self),
        }
    }
}

#[test]
fn test_simple_ops() {
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::Never,
        Mode::Eval,
    ));
    let result = _lib::eval(String::from("8*4+6*0+4*3*5*0+3*0+0*3*9"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "32");
    let result = _lib::eval(String::from("0+0*(8+3)-(8+3)*0"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "0");
    let result = _lib::eval(String::from("0/(5*3+4)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "0");
    let result = _lib::eval(String::from("(-1)*(5*6)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "-30");
}

#[test]
fn test_functions() {
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::Never,
        Mode::Eval,
    ));
    _lib::eval(String::from("a(x)=(x/2)+3"), &mut context).unwrap();
    let result = _lib::eval(String::from("a(-12.4)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "-3.2");
    _lib::eval(String::from("b(x, y)=(x/2)+3+y"), &mut context).unwrap();
    let result = _lib::eval(String::from("b(-12.4, 3)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "-0.2");
    _lib::eval(String::from("f(x) = x+4*3"), &mut context).unwrap();
    let result = _lib::eval(String::from("f(2)*2"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "28");
}

#[test]
fn test_ans() {
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::WhenImplicit,
        Mode::Eval,
    ));
    let result = _lib::eval(String::from("(-0.1)*2"), &mut context) // FIXME: -0.1*2 causes failures!
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "-0.2");
    let result = _lib::eval(String::from("*4"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "-0.8");
}

#[test]
fn test_vars() {
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::Never,
        Mode::Eval,
    ));
    let result = _lib::eval(String::from("k = 34"), &mut context).unwrap().1;
    assert!(result.is_none());
    let result = _lib::eval(String::from("k + 4"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .normalize()
        .to_string();
    assert_eq!(result, "38");
}
