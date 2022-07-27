use crate::ast::{AstNode, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, UnaryOpNode};
use crate::error::DiagnosticBuilder;
use crate::parser::PResult;
use crate::shared::LiteralToken;

pub trait AstWalker<T> {
    fn walk_binop(&self, node: &BinOpNode) -> PResult<T>;

    fn walk_lit(&self, node: &LiteralToken) -> PResult<T>;

    fn walk_unary_op(&self, node: &UnaryOpNode) -> PResult<T>;

    fn walk_maybe_func(&self, node: &MaybeFuncNode) -> PResult<T>;

    fn walk_func_call_or_func_def(&self, node: &FuncCallOrFuncDefNode) -> PResult<T>;

    fn walk(&self, entry: &AstNode) -> PResult<T> {
        match entry {
            AstNode::FuncCallOrFuncDef(node) => self.walk_func_call_or_func_def(node),
            AstNode::MaybeFunc(node) => self.walk_maybe_func(node),
            AstNode::Lit(node) => self.walk_lit(node),
            AstNode::BinOp(node) => self.walk_binop(node),
            AstNode::UnaryOp(node) => self.walk_unary_op(node),
        }
    }
}

pub trait AstWalkerMut<T> {
    fn walk_binop(&self, node: &mut BinOpNode) -> PResult<T>;

    fn walk_lit(&self, node: &mut LiteralToken) -> PResult<T>;

    fn walk_unary_op(&self, node: &mut UnaryOpNode) -> PResult<T>;

    fn walk_maybe_func(&self, node: &mut MaybeFuncNode) -> PResult<T>;

    fn walk_func_call_or_func_def(&self, node: &mut FuncCallOrFuncDefNode) -> PResult<T>;

    fn walk(&self, entry: &mut AstNode) -> PResult<T> {
        match entry {
            AstNode::FuncCallOrFuncDef(node) => self.walk_func_call_or_func_def(node),
            AstNode::MaybeFunc(node) => self.walk_maybe_func(node),
            AstNode::Lit(node) => self.walk_lit(node),
            AstNode::BinOp(node) => self.walk_binop(node),
            AstNode::UnaryOp(node) => self.walk_unary_op(node),
        }
    }
}

pub trait AstWalkerConsuming<T> {
    fn walk_binop(&self, node: BinOpNode) -> PResult<T>;

    fn walk_lit(&self, node: LiteralToken) -> PResult<T>;

    fn walk_unary_op(&self, node: UnaryOpNode) -> PResult<T>;

    fn walk_maybe_func(&self, node: MaybeFuncNode) -> PResult<T>;

    fn walk_func_call_or_func_def(&self, node: FuncCallOrFuncDefNode) -> PResult<T>;

    fn walk(&self, entry: AstNode) -> PResult<T> {
        match entry {
            AstNode::FuncCallOrFuncDef(node) => self.walk_func_call_or_func_def(node),
            AstNode::MaybeFunc(node) => self.walk_maybe_func(node),
            AstNode::Lit(node) => self.walk_lit(node),
            AstNode::BinOp(node) => self.walk_binop(node),
            AstNode::UnaryOp(node) => self.walk_unary_op(node),
        }
    }
}

pub trait LitWalker {
    fn walk_lit(&self, node: &LiteralToken) -> Result<(), DiagnosticBuilder>;
}

impl<T: LitWalker> AstWalker<()> for T {
    fn walk_binop(&self, node: &BinOpNode) -> Result<(), DiagnosticBuilder> {
        self.walk(&*node.lhs)?;
        self.walk(&*node.rhs)
    }

    fn walk_lit(&self, node: &LiteralToken) -> Result<(), DiagnosticBuilder> {
        LitWalker::walk_lit(self, node)
    }

    fn walk_unary_op(&self, _node: &UnaryOpNode) -> Result<(), DiagnosticBuilder> {
        todo!()
    }

    fn walk_maybe_func(&self, node: &MaybeFuncNode) -> Result<(), DiagnosticBuilder> {
        if let Some(param) = &node.param {
            self.walk(&*param)
        } else {
            Ok(())
        }
    }

    fn walk_func_call_or_func_def(
        &self,
        node: &FuncCallOrFuncDefNode,
    ) -> Result<(), DiagnosticBuilder> {
        for param in node.params.iter() {
            self.walk(param)?;
        }
        Ok(())
    }
}

pub trait LitWalkerMut {
    fn walk_lit(&self, node: &mut LiteralToken) -> Result<(), DiagnosticBuilder>;
}

impl<T: LitWalkerMut> AstWalkerMut<()> for T {
    fn walk_binop(&self, node: &mut BinOpNode) -> Result<(), DiagnosticBuilder> {
        self.walk(&mut *node.lhs)?;
        self.walk(&mut *node.rhs)
    }

    fn walk_lit(&self, node: &mut LiteralToken) -> Result<(), DiagnosticBuilder> {
        LitWalkerMut::walk_lit(self, node)
    }

    fn walk_unary_op(&self, _node: &mut UnaryOpNode) -> Result<(), DiagnosticBuilder> {
        todo!()
    }

    fn walk_maybe_func(&self, node: &mut MaybeFuncNode) -> Result<(), DiagnosticBuilder> {
        if let Some(param) = &mut node.param {
            self.walk(&mut *param)
        } else {
            Ok(())
        }
    }

    fn walk_func_call_or_func_def(
        &self,
        node: &mut FuncCallOrFuncDefNode,
    ) -> Result<(), DiagnosticBuilder> {
        for param in node.params.iter_mut() {
            self.walk(param)?;
        }
        Ok(())
    }
}

pub trait LitWalkerConsuming {
    fn walk_lit(&self, node: LiteralToken) -> Result<(), DiagnosticBuilder>;
}

impl<T: LitWalkerConsuming> AstWalkerConsuming<()> for T {
    fn walk_binop(&self, node: BinOpNode) -> Result<(), DiagnosticBuilder> {
        self.walk(*node.lhs)?;
        self.walk(*node.rhs)
    }

    fn walk_lit(&self, node: LiteralToken) -> Result<(), DiagnosticBuilder> {
        LitWalkerConsuming::walk_lit(self, node)
    }

    fn walk_unary_op(&self, _node: UnaryOpNode) -> Result<(), DiagnosticBuilder> {
        todo!()
    }

    fn walk_maybe_func(&self, node: MaybeFuncNode) -> Result<(), DiagnosticBuilder> {
        if let Some(param) = node.param {
            self.walk(*param)
        } else {
            Ok(())
        }
    }

    fn walk_func_call_or_func_def(
        &self,
        node: FuncCallOrFuncDefNode,
    ) -> Result<(), DiagnosticBuilder> {
        for param in node.params.into_iter() {
            self.walk(param.clone())?; // FIXME: try getting rid of this clone!
        }
        Ok(())
    }
}
