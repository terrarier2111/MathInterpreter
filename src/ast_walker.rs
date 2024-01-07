use crate::ast::{AstNode, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, UnaryOpNode, AstEntry};
use crate::span::Span;
use crate::{diagnostic_builder, diagnostic_builder_spanned};
use crate::error::DiagnosticBuilder;
use crate::parser::PResult;
use crate::shared::LiteralToken;

pub trait AstWalker<T> {
    fn walk_binop(&self, node: &BinOpNode, span: Span) -> PResult<T>;

    fn walk_lit(&self, node: &LiteralToken, span: Span) -> PResult<T>;

    fn walk_unary_op(&self, node: &UnaryOpNode, span: Span) -> PResult<T>;

    fn walk_maybe_func(&self, node: &MaybeFuncNode, span: Span) -> PResult<T>;

    fn walk_func_call_or_func_def(&self, node: &FuncCallOrFuncDefNode, span: Span) -> PResult<T>;

    fn walk(&self, entry: &AstEntry) -> PResult<T> {
        match &entry.node {
            AstNode::FuncCallOrFuncDef(node) => self.walk_func_call_or_func_def(node, entry.span),
            AstNode::MaybeFunc(node) => self.walk_maybe_func(node, entry.span),
            AstNode::Lit(node) => self.walk_lit(node, entry.span),
            AstNode::BinOp(node) => self.walk_binop(node, entry.span),
            AstNode::UnaryOp(node) => self.walk_unary_op(node, entry.span),
            AstNode::PartialBinOp(_) => diagnostic_builder!("found a `PartialBinOp` midst the Ast."), // FIXME: add span by using `AstEntry`!
        }
    }

    fn get_input(&self) -> &String;
}

pub trait AstWalkerMut<T> {
    fn walk_binop(&self, node: &mut BinOpNode, span: Span) -> PResult<T>;

    fn walk_lit(&self, node: &mut LiteralToken, span: Span) -> PResult<T>;

    fn walk_unary_op(&self, node: &mut UnaryOpNode, span: Span) -> PResult<T>;

    fn walk_maybe_func(&self, node: &mut MaybeFuncNode, span: Span) -> PResult<T>;

    fn walk_func_call_or_func_def(&self, node: &mut FuncCallOrFuncDefNode, span: Span) -> PResult<T>;

    fn walk(&self, entry: &mut AstEntry) -> PResult<T> {
        let span = entry.span;
        match &mut entry.node {
            AstNode::FuncCallOrFuncDef(node) => self.walk_func_call_or_func_def(node, span),
            AstNode::MaybeFunc(node) => self.walk_maybe_func(node, span),
            AstNode::Lit(node) => self.walk_lit(node, span),
            AstNode::BinOp(node) => self.walk_binop(node, span),
            AstNode::UnaryOp(node) => self.walk_unary_op(node, span),
            AstNode::PartialBinOp(_) => panic!(),
        }
    }
}

pub trait AstWalkerConsuming<T> {
    fn walk_binop(&self, node: BinOpNode, span: Span) -> PResult<T>;

    fn walk_lit(&self, node: LiteralToken, span: Span) -> PResult<T>;

    fn walk_unary_op(&self, node: UnaryOpNode, span: Span) -> PResult<T>;

    fn walk_maybe_func(&self, node: MaybeFuncNode, span: Span) -> PResult<T>;

    fn walk_func_call_or_func_def(&self, node: FuncCallOrFuncDefNode, span: Span) -> PResult<T>;

    fn walk(&self, entry: AstEntry) -> PResult<T> {
        let span = entry.span;
        match entry.node {
            AstNode::FuncCallOrFuncDef(node) => self.walk_func_call_or_func_def(node, span),
            AstNode::MaybeFunc(node) => self.walk_maybe_func(node, span),
            AstNode::Lit(node) => self.walk_lit(node, span),
            AstNode::BinOp(node) => self.walk_binop(node, span),
            AstNode::UnaryOp(node) => self.walk_unary_op(node, span),
            AstNode::PartialBinOp(_) => panic!(),
        }
    }
}

pub trait LitWalker {
    fn walk_lit(&self, node: &LiteralToken, span: Span) -> Result<(), DiagnosticBuilder>;

    fn get_input(&self) -> &String;
}

impl<T: LitWalker> AstWalker<()> for T {
    fn walk_binop(&self, node: &BinOpNode, span: Span) -> Result<(), DiagnosticBuilder> {
        self.walk(&*node.lhs)?;
        self.walk(&*node.rhs)
    }

    fn walk_lit(&self, node: &LiteralToken, span: Span) -> Result<(), DiagnosticBuilder> {
        LitWalker::walk_lit(self, node, span)
    }

    fn walk_unary_op(&self, node: &UnaryOpNode, span: Span) -> Result<(), DiagnosticBuilder> {
        self.walk(&*node.val)?;
        Ok(())
    }

    fn walk_maybe_func(&self, node: &MaybeFuncNode, span: Span) -> Result<(), DiagnosticBuilder> {
        if let Some(param) = &node.param {
            self.walk(&*param)
        } else {
            Ok(())
        }
    }

    fn walk_func_call_or_func_def(
        &self,
        node: &FuncCallOrFuncDefNode,
        span: Span,
    ) -> Result<(), DiagnosticBuilder> {
        for param in node.params.iter() {
            self.walk(param)?;
        }
        Ok(())
    }

    #[inline]
    fn get_input(&self) -> &String {
        LitWalker::get_input(self)
    }
}

pub trait LitWalkerMut {
    fn walk_lit(&self, node: &mut LiteralToken, span: Span) -> Result<(), DiagnosticBuilder>;
}

impl<T: LitWalkerMut> AstWalkerMut<()> for T {
    fn walk_binop(&self, node: &mut BinOpNode, span: Span) -> Result<(), DiagnosticBuilder> {
        self.walk(&mut *node.lhs)?;
        self.walk(&mut *node.rhs)
    }

    fn walk_lit(&self, node: &mut LiteralToken, span: Span) -> Result<(), DiagnosticBuilder> {
        LitWalkerMut::walk_lit(self, node, span)
    }

    fn walk_unary_op(&self, node: &mut UnaryOpNode, span: Span) -> Result<(), DiagnosticBuilder> {
        self.walk(&mut *node.val)?;
        Ok(())
    }

    fn walk_maybe_func(&self, node: &mut MaybeFuncNode, span: Span) -> Result<(), DiagnosticBuilder> {
        if let Some(param) = &mut node.param {
            self.walk(&mut *param)
        } else {
            Ok(())
        }
    }

    fn walk_func_call_or_func_def(
        &self,
        node: &mut FuncCallOrFuncDefNode,
        span: Span,
    ) -> Result<(), DiagnosticBuilder> {
        for param in node.params.iter_mut() {
            self.walk(param)?;
        }
        Ok(())
    }
}

pub trait LitWalkerConsuming {
    fn walk_lit(&self, node: LiteralToken, span: Span) -> Result<(), DiagnosticBuilder>;
}

impl<T: LitWalkerConsuming> AstWalkerConsuming<()> for T {
    fn walk_binop(&self, node: BinOpNode, span: Span) -> Result<(), DiagnosticBuilder> {
        self.walk(*node.lhs)?;
        self.walk(*node.rhs)
    }

    fn walk_lit(&self, node: LiteralToken, span: Span) -> Result<(), DiagnosticBuilder> {
        LitWalkerConsuming::walk_lit(self, node, span)
    }

    fn walk_unary_op(&self, node: UnaryOpNode, span: Span) -> Result<(), DiagnosticBuilder> {
        self.walk(*node.val)?;
        Ok(())
    }

    fn walk_maybe_func(&self, node: MaybeFuncNode, span: Span) -> Result<(), DiagnosticBuilder> {
        if let Some(param) = node.param {
            self.walk(*param)
        } else {
            Ok(())
        }
    }

    fn walk_func_call_or_func_def(
        &self,
        node: FuncCallOrFuncDefNode,
        span: Span,
    ) -> Result<(), DiagnosticBuilder> {
        for param in node.params.into_iter() {
            self.walk(param.clone())?; // FIXME: try getting rid of this clone!
        }
        Ok(())
    }
}
