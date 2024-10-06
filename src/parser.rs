use crate::ast::{
    AstEntry, AstNode, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, PartialBinOpNode,
    RecFuncTail, UnaryOpNode,
};
use crate::ast_walker::{AstWalker, AstWalkerMut, LitWalker, LitWalkerMut};
use crate::conc_once_cell::ConcurrentOnceCell;
use crate::equation_evaluator::{resolve_simple, EvalWalker};
use crate::equation_simplifier::simplify;
use crate::error::DiagnosticBuilder;
use crate::shared::{
    ArgPosition, BinOpKind, ImplicitlyMultiply, LiteralKind, LiteralToken, Number,
    Token, TokenKind, TrailingSpace,
};
use crate::span::{FixedTokenSpan, Span};
use crate::token_stream::TokenStream;
use crate::{
    diagnostic_builder, diagnostic_builder_spanned, equation_evaluator, pluralize,
    register_builtin_func, ANSMode, Mode,
};
use std::collections::HashMap;

const NONE: usize = usize::MAX;

pub(crate) struct Parser<'a> {
    token_stream: TokenStream,
    parse_ctx: &'a mut ParseContext,
    ans_mode: ANSMode,
    curr: Token,
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

        let func = match self.try_parse_function() {
            Ok(val) => val,
            Err(err) => return ParseResult::new_err(err),
        };

        // Make implicit multiplications explicit!
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
                match &mut idx {
                    None => {
                        idx = Some(1);
                    }
                    Some(idx) => {
                        *idx += 1;
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

        while !self.eat(TokenKind::EOF) {
            let curr_token_mult = if let Some(lit) = self.parse_lit() {
                if self.parse_ctx.exists_fn(&lit.content)
                    || self.parse_ctx.exists_builtin_func(&lit.content)
                    || func
                        .as_ref()
                        .map(|node| match &node.node {
                            AstNode::FuncCallOrFuncDef(func) => {
                                !self.parse_ctx.vars.contains_key(&func.name)
                                    && func.name == lit.content
                            }
                            AstNode::MaybeFunc(func) => {
                                !self.parse_ctx.vars.contains_key(&func.name)
                                    && func.name == lit.content
                            }
                            // AstNode::RecFuncDef(_) => {}
                            _ => false,
                        })
                        .unwrap_or(false)
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
            tokens.insert(
                x.0 + *x.1,
                Token::BinOp(FixedTokenSpan::new(x.0), BinOpKind::Multiply),
            );
        }
        self.token_stream
            .replace_internal_tokens(tokens.into_boxed_slice());
        self.reset();

        let head_expr = match self.parse_expr() {
            Ok(val) => val,
            Err(err) => {
                return ParseResult::new_err(err);
            }
        };
        let tail = match self.try_parse_rec_tail() {
            Ok(val) => val,
            Err(err) => {
                return ParseResult::new_err(err);
            }
        };

        match mode {
            Mode::Eval => {
                let val =
                    equation_evaluator::eval(self.parse_ctx, self.ans_mode, head_expr, tail);
                match val {
                    Ok(val) => {
                        self.parse_ctx.set_last(val.clone());
                        ParseResult::new_ok(val)
                    }
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

    fn try_parse_function(&mut self) -> PResult<Option<AstEntry>> {
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
                        "expected `)`",
                        Span::single_token(self.curr.span().end) // FIXME: should we add 1 to the passed value?
                    );
                }

                if params.len() > 1 {
                    Ok(Some(AstEntry {
                        span: Span::multi_token(
                            lit.span.start,
                            params
                                .last()
                                .map(|p| p.span.end + 1)
                                .unwrap_or(lit.span.end + 2),
                        ),
                        node: AstNode::FuncCallOrFuncDef(FuncCallOrFuncDefNode {
                            name: lit.content,
                            params: params.into_boxed_slice(),
                        }),
                    }))
                } else {
                    // this may still be an implicit multiplication
                    Ok(Some(AstEntry {
                        span: Span::multi_token(
                            lit.span.start,
                            params
                                .last()
                                .map(|p| p.span.end + 1)
                                .unwrap_or(lit.span.end + 2),
                        ),
                        node: AstNode::MaybeFunc(MaybeFuncNode {
                            name: lit.content,
                            param: params.pop().map(Box::new),
                        }),
                    }))
                }
            } else {
                self.go_back();
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }

    fn try_parse_rec_tail(&mut self) -> PResult<Option<RecFuncTail>> {
        if !self.eat(TokenKind::At) {
            // FIXME: is this the correct handling?
            return Ok(None);
        }

        let idx = self.try_parse_natural()?;

        if idx.is_none() {
            if let Some(token) = self.token_stream.get_next() {
                return diagnostic_builder_spanned!(
                    "expected natural number after `@`",
                    token.span()
                ); // FIXME: improve this error message!
            } else {
                return diagnostic_builder!("expected natural number after `@`");
                // FIXME: improve this error message!
            }
        }

        if !self.eat_binop(BinOpKind::Eq) {
            return diagnostic_builder!("expected `=`"); // FIXME: improve this error message!
        }

        let val = self.parse_primary()?/*self.parse_binop()?*/;

        Ok(Some(RecFuncTail {
            idx: idx.unwrap(),
            val,
        }))
    }

    fn try_parse_natural(&mut self) -> PResult<Option<usize>> {
        if let Token::Literal(lit) = &self.curr {
            let ret = lit.clone();
            self.advance();
            match ret.content.parse() {
                Ok(val) => Ok(Some(val)),
                Err(_) => diagnostic_builder_spanned!(
                    format!("expected natural number, found `{}`", &ret.content),
                    ret.span
                ),
            }
        } else {
            Ok(None)
        }
    }

    fn parse_expr(&mut self) -> PResult<AstEntry> {
        self.parse_binop()
    }

    fn parse_binop(&mut self) -> PResult<AstEntry> {
        let lhs = self.parse_primary()?;
        self.parse_bin_op_rhs(0, lhs)
    }

    fn parse_bin_op_rhs(&mut self, prec: u8, mut lhs: AstEntry) -> PResult<AstEntry> {
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
            self.eat(TokenKind::BinOp);

            let mut rhs = Some(self.parse_primary()?);

            // If BinOp binds less tightly with RHS than the operator after RHS, let
            // the pending operator take RHS as its LHS.
            let next_bin_op = if let Token::BinOp(_, bin_op) = &self.curr {
                Some(*bin_op)
            } else {
                None
            };

            match next_bin_op {
                None => {
                    return Ok(AstEntry {
                        span: lhs.span.merge_with(rhs.as_ref().unwrap().span),
                        node: AstNode::BinOp(BinOpNode {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs.take().unwrap()),
                            op: bin_op,
                        }),
                    });
                }
                Some(next_bin_op) => {
                    if bin_op.precedence() < next_bin_op.precedence() {
                        rhs = rhs.map(|rhs| {
                            self.parse_bin_op_rhs(bin_op.precedence() + 1, rhs).unwrap()
                        });
                    }
                    lhs = AstEntry {
                        span: lhs.span.merge_with(rhs.as_ref().unwrap().span),
                        node: AstNode::BinOp(BinOpNode {
                            lhs: Box::new(lhs),
                            rhs: Box::new(rhs.take().unwrap()),
                            op: bin_op,
                        }),
                    };
                }
            }
        }
    }

    fn parse_paren_expr(&mut self) -> PResult<AstEntry> {
        if !self.eat(TokenKind::OpenParen) {
            return diagnostic_builder_spanned!("expected `{`", self.curr.span());
        }
        let expr = self.parse_expr()?;

        if !self.eat(TokenKind::ClosedParen) {
            return diagnostic_builder_spanned!("expected `}`", self.curr.span());
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> PResult<AstEntry> {
        match &self.curr {
            Token::OpenParen(_) => {
                let lhs = self.parse_paren_expr()?;
                self.try_parse_unary_arg_lhs(lhs)
            }
            Token::Literal(_) => {
                let lhs = if let Some(func) = self.try_parse_function()? {
                    func
                } else {
                    let lit = self.parse_lit().unwrap();
                    AstEntry {
                        span: lit.span,
                        node: AstNode::Lit(lit),
                    }
                };
                self.try_parse_unary_arg_lhs(lhs)
            }
            Token::UnaryOp(_, op) => {
                if op.arg_position() == ArgPosition::RHS {
                    self.parse_unary_arg_rhs()
                } else {
                    diagnostic_builder_spanned!(
                        format!("can't find argument of unary {}", self.curr.to_raw()),
                        self.curr.span()
                    )
                }
            }
            Token::BinOp(_, op) => {
                let op = *op;
                self.advance();
                let rhs = Box::new(self.parse_primary()?);
                Ok(AstEntry {
                    span: rhs.span.expand_lo().unwrap(),
                    node: AstNode::PartialBinOp(PartialBinOpNode { op, rhs }),
                })
            }
            _ => diagnostic_builder_spanned!(
                format!(
                    "expected expression, found {} of type {:?}",
                    self.curr.to_raw(),
                    self.curr.kind()
                ),
                self.curr.span()
            ),
        }
    }

    fn parse_unary_arg_rhs(&mut self) -> PResult<AstEntry> {
        if let Token::UnaryOp(_, op) = &self.curr {
            let op = *op;
            self.advance();
            let rhs = self.parse_primary()?;
            Ok(AstEntry {
                span: rhs.span.expand_lo().unwrap(),
                node: AstNode::UnaryOp(UnaryOpNode {
                    op,
                    val: Box::new(rhs),
                }),
            })
        } else {
            unreachable!()
        }
    }

    fn try_parse_unary_arg_lhs(&mut self, lhs: AstEntry) -> PResult<AstEntry> {
        if let Token::UnaryOp(_, op) = &self.curr {
            let op = *op;
            if op.arg_position() == ArgPosition::LHS {
                self.advance();

                return Ok(AstEntry {
                    span: lhs.span.expand_hi(),
                    node: AstNode::UnaryOp(UnaryOpNode {
                        op,
                        val: Box::new(lhs),
                    }),
                });
            }
        }
        Ok(lhs)
    }

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
    rec_funcs: HashMap<String, RecursiveFunction>,
    builtin_funcs: HashMap<String, BuiltInFunction>,
    pub registered_sets: Vec<ConstantSetKind>,
}

impl ParseContext {
    pub fn new() -> Self {
        let mut ret = Self {
            input: String::new(),
            last_result: None,
            vars: Default::default(),
            funcs: Default::default(),
            rec_funcs: Default::default(),
            builtin_funcs: Default::default(),
            registered_sets: vec![],
        };
        ret.register_set(ConstantSetKind::Math);
        register_builtin_func!(ret, "abs", 1, |nums| nums[0].clone().abs());
        register_builtin_func!(ret, "sin", 1, |nums| nums[0].clone().sin());
        register_builtin_func!(ret, "cos", 1, |nums| nums[0].clone().cos());
        register_builtin_func!(ret, "tan", 1, |nums| nums[0].clone().tan());
        register_builtin_func!(
            ret,
            "asin",
            &[(Some((-1.0, true)), Some((1.0, true)))],
            |nums| nums[0].clone().asin()
        );
        register_builtin_func!(
            ret,
            "acos",
            &[(Some((-1.0, true)), Some((1.0, true)))],
            |nums| nums[0].clone().acos()
        );
        register_builtin_func!(
            ret,
            "atan",
            &[(Some((-1.0, true)), Some((1.0, true)))],
            |nums| nums[0].clone().atan()
        );
        register_builtin_func!(ret, "ln", &[(Some((0.0, false)), None)], |nums| nums[0].clone().ln());
        register_builtin_func!(ret, "round", 1, |nums| nums[0].clone().round()); // FIXME: Should we even keep this one?
                                                                         // even though these are approximations, they should be good enough
        register_builtin_func!(ret, "floor", 1, |nums| nums[0].clone().floor());
        register_builtin_func!(ret, "ceil", 1, |nums| nums[0].clone().ceil());
        register_builtin_func!(ret, "sqrt", &[(Some((0.0, true)), None)], |nums| nums[0].clone().sqrt());
        register_builtin_func!(ret, "max", 2, |nums| nums[0].clone().max(nums[1].clone()));
        register_builtin_func!(ret, "min", 2, |nums| nums[0].clone().min(nums[1].clone()));
        register_builtin_func!(ret, "deg", 1, |nums| nums[0].clone()
            * (Number::from_u64(180) / Number::pi()));
        register_builtin_func!(ret, "rad", 1, |nums| nums[0].clone()
            * (Number::pi() / Number::from_u64(180)));
        ret
    }

    pub fn register_set(&mut self, set: ConstantSetKind) {
        self.registered_sets.push(set);
        for con in set.values().iter() {
            self.register_const(con.0, con.1.clone());
        }
    }

    pub fn unregister_set(&mut self, set: ConstantSetKind) {
        let idx = self
            .registered_sets
            .iter()
            .enumerate()
            .find(|(idx, curr)| **curr == set)
            .unwrap()
            .0;
        self.registered_sets.remove(idx);
        for con in set.values().iter() {
            self.vars.remove(con.0);
        }
    }

    pub fn unregister_all_sets(&mut self) {
        for set in self.registered_sets.iter() {
            for con in set.values().iter() {
                self.vars.remove(con.0);
            }
        }
        self.registered_sets.clear();
    }

    #[inline]
    pub(crate) fn set_input(&mut self, input: String) {
        self.input = input;
    }

    #[inline]
    pub fn get_input(&self) -> &String {
        &self.input
    }

    #[inline]
    pub fn get_last(&self) -> &Option<Number> {
        &self.last_result
    }

    #[inline]
    pub fn set_last(&mut self, val: Option<Number>) {
        self.last_result = val;
    }

    pub fn exists_const(&self, name: &String) -> bool {
        let name = name.to_lowercase();
        let con = self.vars.get(&*name);
        if let Some(con) = con {
            return !con.0;
        }
        false
    }

    pub fn lookup_const(
        &self,
        name: &String,
        parse_ctx: &ParseContext,
        span: Span,
    ) -> PResult<Number> {
        let name = name.to_lowercase();
        let tmp = self.vars.get(name.as_str()).unwrap().clone();
        if !tmp.0 {
            return Ok(tmp.1);
        }
        diagnostic_builder_spanned!(format!("`{}` is not a const", name), span)
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
        span: Span,
    ) -> Result<bool, DiagnosticBuilder> {
        let name = name.to_lowercase();
        if self.exists_fn(&name) {
            return diagnostic_builder_spanned!(
                format!("There is already a variable named `{}`", name),
                span
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
    pub(crate) fn register_const(&mut self, name: &str, value: Number) -> bool {
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
        self.funcs.contains_key(&*name)
            || self.rec_funcs.contains_key(&*name)
            || self.builtin_funcs.contains_key(&*name)
    }

    pub fn try_call_func(
        &self,
        name: &String,
        args: Box<[AstEntry]>,
        span: Span,
    ) -> Option<PResult<AstEntry>> {
        let name = name.to_lowercase();
        let func = self.funcs.get(&*name);
        match func {
            None => {
                if self.rec_funcs.contains_key(&name) {
                    self.call_rec_func(&name, args, span)
                } else {
                    let result = self.call_builtin_func(&name, args, span);
                    result.map(|pr| {
                        pr.map(|num| AstEntry {
                            span,
                            node: AstNode::Lit(LiteralToken {
                                span,
                                content: num.to_string(),
                                kind: LiteralKind::Number,
                                trailing_space: TrailingSpace::Maybe,
                            }),
                        })
                    })
                }
            }
            Some(func) => Some(func.build_tokens(args, self, span)),
        }
    }

    pub fn register_func(&mut self, func: Function) {
        let name = func.name.to_lowercase();
        self.funcs.insert(name, func);
    }

    fn call_rec_func(
        &self,
        name: &String,
        args: Box<[AstEntry]>,
        span: Span,
    ) -> Option<PResult<AstEntry>> {
        let func_name = name.to_lowercase();
        self.rec_funcs.get(&*func_name).map(|func| func.build_tokens(args, self, span))
    }

    pub fn register_rec_func(&mut self, func: RecursiveFunction) {
        let name = func.name.to_lowercase();
        self.rec_funcs.insert(name, func);
    }

    pub(crate) fn exists_builtin_func(&self, name: &String) -> bool {
        let name = name.to_lowercase();
        self.builtin_funcs.contains_key(&*name)
    }

    fn call_builtin_func(
        &self,
        name: &String,
        args: Box<[AstEntry]>,
        span: Span,
    ) -> Option<PResult<Number>> {
        let func_name = name.to_lowercase();
        self.builtin_funcs.get(&*func_name).map(|func| func.build_tokens(args, self, span))
    }

    fn register_builtin_func(&mut self, func: BuiltInFunction) {
        let name = func.name.to_lowercase();
        self.builtin_funcs.insert(name, func);
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum ConstantSetKind {
    Math,
    Physics,
}

impl ConstantSetKind {
    pub fn name(&self) -> &str {
        match self {
            ConstantSetKind::Math => "math",
            ConstantSetKind::Physics => "physics",
        }
    }

    pub fn values(&self) -> &Vec<(&'static str, Number)> {
        match self {
            ConstantSetKind::Math => {
                SET_MATH.get_or_else(|| vec![("pi", Number::pi()), ("e", Number::e())])
            }
            ConstantSetKind::Physics => SET_PHYS.get_or_else(|| {
                vec![
                    ("c", Number::from_u64(299792458)), // speed of light
                    ("h", Number::from_f64(6.6261 / 10.0_f64.powi(34))),
                    ("me", Number::from_f64(9.10939 / 10.0_f64.powi(31))), // mass of an electron
                    ("ev", Number::from_f64(1.6022 / 10.0_f64.powi(19))),  // charge of an electron
                    ("u", Number::from_f64(1.660539 / 10.0_f64.powi(27))), // unit of atom mass
                ]
            }),
        }
    }
}

static SET_MATH: ConcurrentOnceCell<Vec<(&str, Number)>> = ConcurrentOnceCell::new();
static SET_PHYS: ConcurrentOnceCell<Vec<(&str, Number)>> = ConcurrentOnceCell::new();

#[derive(Debug, Clone)]
pub enum Action {
    DefineVar(String),                                    // var name
    DefineFunc(String, Box<[String]>),                    // function name, function arguments
    DefineRecFunc(String, Box<[String]>, (usize, usize)), // function name, function arguments, (recursive min idx, recursive min val)
    Eval(Option<PartialBinOpNode>),
}

impl Action {
    pub fn kind(&self) -> ActionKind {
        match self {
            Action::DefineVar(_) => ActionKind::DefineVar,
            Action::DefineFunc(_, _) => ActionKind::DefineFunc,
            Action::Eval(_) => ActionKind::Eval,
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
    fn walk_lit(&self, node: &mut LiteralToken, span: Span) -> Result<(), DiagnosticBuilder> {
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

impl AstWalker<()> for FunctionInitValidator<'_> {
    // FIXME: ensure that functions that are being used actually exist!
    fn walk_lit(&self, lit_tok: &LiteralToken, span: Span) -> Result<(), DiagnosticBuilder> {
        if lit_tok.kind == LiteralKind::CharSeq
            && !self.arg_names.contains(&lit_tok.content)
            && !self.parse_ctx.exists_const(&lit_tok.content)
        {
            return diagnostic_builder_spanned!("Not an argument or const", lit_tok.span);
        }

        Ok(())
    }

    fn walk_binop(&self, node: &BinOpNode, span: Span) -> PResult<()> {
        self.walk(&node.lhs)?;
        self.walk(&node.rhs)
    }

    fn walk_unary_op(&self, node: &UnaryOpNode, span: Span) -> PResult<()> {
        self.walk(&node.val)
    }

    fn walk_maybe_func(&self, node: &MaybeFuncNode, span: Span) -> PResult<()> {
        // FIXME: also allow this name to be a variable name!
        if &node.name == self.func_name {
            return diagnostic_builder_spanned!(
                "Can't call non-recursive function inside its definition",
                span
            );
        }

        if !self.parse_ctx.exists_fn(&node.name) {
            return diagnostic_builder_spanned!(
                format!("There's no function called {}", &node.name),
                span
            );
        }

        if let Some(param) = node.param.as_ref() {
            self.walk(param)?;
        }

        Ok(())
    }

    fn walk_func_call_or_func_def(&self, node: &FuncCallOrFuncDefNode, span: Span) -> PResult<()> {
        if &node.name == self.func_name {
            return diagnostic_builder_spanned!(
                "Can't call non-recursive function inside its definition",
                span
            );
        }

        if !self.parse_ctx.exists_fn(&node.name) {
            return diagnostic_builder_spanned!(
                format!("There's no function called {}", &node.name),
                span
            );
        }

        for arg in node.params.iter() {
            self.walk(arg)?;
        }

        Ok(())
    }
}

struct RecFunctionInitValidator<'a> {
    parse_ctx: &'a ParseContext,
    arg_names: &'a Box<[String]>,
    func_name: &'a String,
}

impl LitWalker for RecFunctionInitValidator<'_> {
    fn walk_lit(&self, lit_tok: &LiteralToken, span: Span) -> Result<(), DiagnosticBuilder> {
        if lit_tok.kind == LiteralKind::CharSeq
            && !self.arg_names.contains(&lit_tok.content)
            && !self.parse_ctx.exists_const(&lit_tok.content)
            && (!self.parse_ctx.exists_fn(&lit_tok.content) || &lit_tok.content == self.func_name)
        {
            return diagnostic_builder_spanned!("Not an argument, function or const", lit_tok.span);
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
        structure: AstEntry,
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
            ast: structure.node,
            args: arg_names,
        })
    }

    pub fn build_tokens(
        &self,
        arg_values: Box<[AstEntry]>,
        parse_ctx: &ParseContext,
        span: Span,
    ) -> PResult<AstEntry> {
        if self.args.len() != arg_values.len() {
            return diagnostic_builder_spanned!(
                format!(
                    "expected {} argument{}, got {}",
                    self.args.len(),
                    pluralize!(self.args.len()),
                    arg_values.len()
                ),
                span
            );
        }
        let mut arg_replacements = HashMap::new();
        for val in arg_values.iter().enumerate() {
            let eval_walker = EvalWalker { ctx: parse_ctx };
            arg_replacements.insert(self.args[val.0].clone(), eval_walker.walk(val.1)?);
        }

        let walker = FunctionWalker {
            parse_ctx,
            arg_replacements,
        };

        let mut result = AstEntry {
            span,
            node: self.ast.clone(),
        };
        walker.walk(&mut result)?;
        Ok(result)
    }
}

pub(crate) struct BuiltInFunction {
    name: String,
    args: Vec<(Option<(Number, bool)>, Option<(Number, bool)>)>,
    inner: Box<dyn Fn(Vec<Number>) -> Number + Send + Sync>,
}

impl BuiltInFunction {
    pub fn new(
        name: String,
        args: &[(Option<(f64, bool)>, Option<(f64, bool)>)],
        inner: Box<dyn Fn(Vec<Number>) -> Number + Send + Sync>,
    ) -> Self {
        Self {
            name,
            args: args
                .iter()
                .map(|arg| {
                    (
                        arg.0.map(|(arg, eq)| (Number::from_f64(arg), eq)),
                        arg.1.map(|(arg, eq)| (Number::from_f64(arg), eq)),
                    )
                })
                .collect::<Vec<_>>(),
            inner,
        }
    }

    pub fn build_tokens(
        &self,
        arg_values: Box<[AstEntry]>,
        parse_ctx: &ParseContext,
        span: Span,
    ) -> PResult<Number> {
        if self.args.len() != arg_values.len() {
            return diagnostic_builder_spanned!(
                format!(
                    "Expected {} argument{}, got {}",
                    self.args.len(),
                    pluralize!(self.args.len()),
                    arg_values.len()
                ),
                span
            );
        }
        let mut args = vec![];
        for arg in arg_values.iter() {
            let eval_walker = EvalWalker { ctx: parse_ctx };
            let walked = eval_walker.walk(arg)?;
            let curr_arg = &self.args[args.len()];
            if curr_arg
                .0
                .as_ref()
                .map(|(arg, eq)| !(arg < &walked || (*eq && arg == &walked)))
                .unwrap_or(false)
            {
                return diagnostic_builder_spanned!(
                    format!(
                        "{} is not >{} than the minimum value {}",
                        &walked,
                        if curr_arg.0.as_ref().unwrap().1 {
                            "="
                        } else {
                            ""
                        },
                        curr_arg.0.as_ref().unwrap().0
                    ),
                    arg.span
                );
            }
            if curr_arg
                .1
                .as_ref()
                .map(|(arg, eq)| !(arg > &walked || (*eq && arg == &walked)))
                .unwrap_or(false)
            {
                return diagnostic_builder_spanned!(
                    format!(
                        "{} is not <{} than the maximum value {}",
                        &walked,
                        if curr_arg.1.as_ref().unwrap().1 {
                            "="
                        } else {
                            ""
                        },
                        curr_arg.1.as_ref().unwrap().0
                    ),
                    arg.span
                );
            }
            args.push(walked);
        }
        let result = (self.inner)(args);
        Ok(result)
    }
}

pub(crate) struct RecursiveFunction {
    name: String,
    ast: AstNode,
    args: Box<[String]>,
    end_idx: usize,
    end_val: AstNode,
}

impl RecursiveFunction {
    pub fn new(
        name: String,
        arg_names: Box<[String]>,
        structure: AstEntry,
        end_idx: usize,
        end_val: AstNode,
        parse_ctx: &ParseContext,
        span: Span,
    ) -> PResult<Self> {
        if arg_names.is_empty() {
            return diagnostic_builder_spanned!(
                "a recursion argument is required for a recursive function",
                span
            );
        }

        // verify arguments are valid
        let validator = FunctionInitValidator {
            parse_ctx,
            arg_names: &arg_names,
            func_name: &name,
        };
        validator.walk(&structure)?;

        Ok(Self {
            name,
            ast: structure.node,
            args: arg_names,
            end_idx,
            end_val,
        })
    }

    pub fn build_tokens(
        &self,
        arg_values: Box<[AstEntry]>,
        parse_ctx: &ParseContext,
        span: Span,
    ) -> PResult<AstEntry> {
        if self.args.len() != arg_values.len() {
            return diagnostic_builder_spanned!(
                format!(
                    "expected {} argument{}, got {}",
                    self.args.len(),
                    pluralize!(self.args.len()),
                    arg_values.len()
                ),
                span
            );
        }

        let rec_param = resolve_simple(parse_ctx, &arg_values[0])?.to_string();
        let mut def = false;
        if rec_param.parse::<usize>().is_err() && (self.end_idx != 0 || rec_param.parse::<isize>().is_err()) {
            return diagnostic_builder_spanned!(
                format!(
                    "expected a natural number as the recursion parameter, found `{}`",
                    rec_param
                ),
                arg_values[0].span
            );
        }
        if rec_param
            .parse::<usize>()
            .map_or(true, |val| val <= self.end_idx)
        {
            // FIXME: is <= correct here?
            def = true;
        }

        let mut arg_replacements = HashMap::new();
        for val in arg_values
            .iter()
            .enumerate()
            .skip(if def { 1 } else { 0 })
        {
            let eval_walker = EvalWalker { ctx: parse_ctx };
            arg_replacements.insert(self.args[val.0].clone(), eval_walker.walk(val.1)?);
        }

        let walker = FunctionWalker {
            parse_ctx,
            arg_replacements,
        };

        let result = if def {
            self.end_val.clone()
        } else {
            self.ast.clone()
        };
        let mut result = AstEntry { span, node: result };
        walker.walk(&mut result)?;
        Ok(result)
    }
}

pub type PResult<T> = Result<T, DiagnosticBuilder>;

mod macros {
    #[macro_export]
    macro_rules! register_builtin_func {
        ($ctx:ident, $name:literal, $arg_cnt:literal, $func:expr/*tt*/) => {
            $ctx.register_builtin_func(BuiltInFunction::new(
                $name.to_string(),
                &[(None, None); $arg_cnt],
                Box::new($func),
            ));
        };
        ($ctx:ident, $name:literal, $args:expr, $func:expr/*tt*/) => {
            $ctx.register_builtin_func(BuiltInFunction::new(
                $name.to_string(),
                $args,
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
    use crate::_lib;
    use crate::_lib::*;
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::Never,
        CircleUnit::Degrees,
    ));
    let result = _lib::eval(String::from("8*4+6*0+4*3*5*0+3*0+0*3*9"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "32");
    let result = _lib::eval(String::from("0+0*(8+3)-(8+3)*0"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "0");
    let result = _lib::eval(String::from("0/(5*3+4)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "0");
    let result = _lib::eval(String::from("-1*(5*6)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "-30");
    let result = _lib::eval(String::from("5!"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "119.9999999999997"); // FIXME: make this 120
}

#[test]
fn test_functions() {
    use crate::_lib;
    use crate::_lib::*;
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::Never,
        CircleUnit::Degrees,
    ));
    _lib::eval(String::from("a(x)=(x/2)+3"), &mut context).unwrap();
    let result = _lib::eval(String::from("a(-12.4)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "-3.2");
    _lib::eval(String::from("b(x, y)=(x/2)+3+y"), &mut context).unwrap();
    let result = _lib::eval(String::from("b(-12.4, 3)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "-0.2");
    _lib::eval(String::from("f(x) = x+4*3"), &mut context).unwrap();
    let result = _lib::eval(String::from("f(2)*2"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "28");
}

#[test]
fn test_ans() {
    use crate::_lib;
    use crate::_lib::*;
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::WhenImplicit,
        CircleUnit::Degrees,
    ));
    let result = _lib::eval(String::from("(-0.1)*2"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "-0.2");
    let result = _lib::eval(String::from("*4"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "-0.8");
}

#[test]
fn test_vars() {
    use crate::_lib;
    use crate::_lib::*;
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::Never,
        CircleUnit::Degrees,
    ));
    let result = _lib::eval(String::from("k = 34"), &mut context).unwrap().1;
    assert!(result.is_none());
    let result = _lib::eval(String::from("k + 4"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "38");
    let result = _lib::eval(String::from("k(2)"), &mut context)
        .unwrap()
        .0
        .unwrap()
        .to_string();
    assert_eq!(result, "68");
}
