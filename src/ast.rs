use crate::shared::{BinOpKind, LiteralToken, UnaryOpKind};
use crate::span::Span;

#[derive(Clone, Debug)]
pub struct AstEntry {
    span: Span,
    node: AstNode,
}

#[derive(Clone, Debug)]
pub enum AstNode {
    FuncCallOrFuncDef(FuncCallOrFuncDefNode),
    MaybeFunc(MaybeFuncNode),
    Lit(LiteralToken),
    BinOp(BinOpNode),
    UnaryOp(UnaryOpNode),
}

impl AstNode {
    pub(crate) fn kind(&self) -> AstNodeKind {
        match self {
            AstNode::FuncCallOrFuncDef(_) => AstNodeKind::FuncCallOrFuncDef,
            AstNode::MaybeFunc(_) => AstNodeKind::MaybeFunc,
            AstNode::Lit(_) => AstNodeKind::Lit,
            AstNode::BinOp(_) => AstNodeKind::BinOp,
            AstNode::UnaryOp(_) => AstNodeKind::UnaryOp,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum AstNodeKind {
    FuncCallOrFuncDef,
    MaybeFunc,
    Lit,
    BinOp,
    UnaryOp,
}

// This node could either be a function call or a function def
#[derive(Clone, Debug)]
pub struct FuncCallOrFuncDefNode {
    pub(crate) name: String,
    pub(crate) params: Box<[AstNode]>, // params consisting of their values
}

// This node could either be a function call/def or just a var
// with an implicit multiplication between an open brace
#[derive(Clone, Debug)]
pub struct MaybeFuncNode {
    pub(crate) name: String,
    pub(crate) param: Option<Box<AstNode>>, // param consisting of its value
}

#[derive(Clone, Debug)]
pub struct BinOpNode {
    pub(crate) op: BinOpKind,
    pub(crate) lhs: Box<AstNode>,
    pub(crate) rhs: Box<AstNode>,
}

#[derive(Clone, Debug)]
pub struct UnaryOpNode {
    pub(crate) op: UnaryOpKind,
    pub(crate) val: Box<AstNode>,
}
