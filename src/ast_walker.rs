use crate::ast::{AstNode, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, UnaryOpNode};
use crate::error::DiagnosticBuilder;
use crate::shared::LiteralToken;

/*
pub struct AstWalkerImmut<'a> {
    node: &'a AstNode,
}

pub struct AstWalkerMut<'a> {
    node: &'a mut AstNode,
}

pub trait AstWalker<T> {

    fn walk(&self, node: &AstNode) -> Result<T, DiagnosticBuilder>;

    fn walk_mut(&mut self, node: &mut AstNode) -> Result<T, DiagnosticBuilder>;

    fn _walk_inner(&self) -> Result<T, DiagnosticBuilder> {

    }

    fn _walk_inner_mut(&mut self) -> Result<T, DiagnosticBuilder>;

    fn curr_node(&self) -> &AstNode;

    fn curr_node_mut(&mut self) -> &mut AstNode;

}
*/

pub trait AstWalker<T> {
    fn walk_binop(&self, node: &BinOpNode) -> Result<T, DiagnosticBuilder>;

    fn walk_lit(&self, node: &LiteralToken) -> Result<T, DiagnosticBuilder>;

    fn walk_unary_op(&self, node: &UnaryOpNode) -> Result<T, DiagnosticBuilder>;

    fn walk_maybe_func(&self, node: &MaybeFuncNode) -> Result<T, DiagnosticBuilder>;

    fn walk_func_call_or_func_def(
        &self,
        node: &FuncCallOrFuncDefNode,
    ) -> Result<T, DiagnosticBuilder>;

    fn walk(&self, entry: &AstNode) -> Result<T, DiagnosticBuilder> {
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
    fn walk_binop(&self, node: &mut BinOpNode) -> Result<T, DiagnosticBuilder>;

    fn walk_lit(&self, node: &mut LiteralToken) -> Result<T, DiagnosticBuilder>;

    fn walk_unary_op(&self, node: &mut UnaryOpNode) -> Result<T, DiagnosticBuilder>;

    fn walk_maybe_func(&self, node: &mut MaybeFuncNode) -> Result<T, DiagnosticBuilder>;

    fn walk_func_call_or_func_def(
        &self,
        node: &mut FuncCallOrFuncDefNode,
    ) -> Result<T, DiagnosticBuilder>;

    fn walk(&self, entry: &mut AstNode) -> Result<T, DiagnosticBuilder> {
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
    fn walk_binop(&self, node: BinOpNode) -> Result<T, DiagnosticBuilder>;

    fn walk_lit(&self, node: LiteralToken) -> Result<T, DiagnosticBuilder>;

    fn walk_unary_op(&self, node: UnaryOpNode) -> Result<T, DiagnosticBuilder>;

    fn walk_maybe_func(&self, node: MaybeFuncNode) -> Result<T, DiagnosticBuilder>;

    fn walk_func_call_or_func_def(
        &self,
        node: FuncCallOrFuncDefNode,
    ) -> Result<T, DiagnosticBuilder>;

    fn walk(&self, entry: AstNode) -> Result<T, DiagnosticBuilder> {
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
