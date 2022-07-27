use crate::ast::{
    AstNode, AstNodeKind, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, UnaryOpNode,
};
use crate::ast_walker::AstWalker;
use crate::diagnostic_builder;
use crate::error::DiagnosticBuilder;
use crate::parser::{Action, Function, PResult, ParseContext};
use crate::shared::{BinOpKind, LiteralKind, LiteralToken, Number, SignKind, TrailingSpace};
use crate::span::Span;
use std::hint::unreachable_unchecked;
use std::mem;
use std::ops::Neg;

pub(crate) fn eval(
    parse_ctx: &mut ParseContext,
    entry: AstNode,
) -> Result<Option<Number>, DiagnosticBuilder> {
    if entry.kind() != AstNodeKind::BinOp {
        let walker = EvalWalker { ctx: parse_ctx };
        return walker.walk(&entry).map(|val| Some(val));
    }

    // FIXME: get rid of all that cloning by consuming entry
    let (entry, action) = if let AstNode::BinOp(node) = entry {
        if node.op == BinOpKind::Eq {
            (
                *node.rhs,
                match *node.lhs {
                    AstNode::FuncCallOrFuncDef(func) => {
                        Action::DefineFunc(func.name, {
                            let mut result = Vec::with_capacity(func.params.len());
                            for param in func.params.into_iter() {
                                if let AstNode::Lit(lit) = param {
                                    result.push(lit.content.clone()); // FIXME: can we get rid of this clone?
                                } else {
                                    return diagnostic_builder!(
                                        parse_ctx.get_input().clone(),
                                        "expected a parameter name, but got an expression"
                                    );
                                }
                            }
                            result.into_boxed_slice()
                        })
                    }
                    AstNode::MaybeFunc(func) => Action::DefineFunc(func.name, {
                        func.param.map_or_else(
                            || Ok(Box::new([]) as Box<[String]>),
                            |param| {
                                if let AstNode::Lit(lit) = *param {
                                    Ok(Box::new([lit.content]))
                                } else {
                                    diagnostic_builder!(
                                        parse_ctx.get_input().clone(),
                                        "expected a parameter name, but got an expression"
                                    )
                                }
                            },
                        )?
                    }),
                    AstNode::Lit(var) => Action::DefineVar(var.content.clone()),
                    AstNode::BinOp(_) => {
                        return diagnostic_builder!(parse_ctx.get_input().clone(), "expected a function signature or variable name, but got a binary operation");
                    }
                    AstNode::UnaryOp(_) => {
                        return diagnostic_builder!(parse_ctx.get_input().clone(), "expected a function signature or variable name, but got an unary operation");
                    }
                },
            )
        } else {
            (AstNode::BinOp(node), Action::Eval)
        }
    } else {
        // SAFETY: above we just checked if the current token kind is BinOp
        // and after that we don't modify anything related to the current token until this check
        // and thus we will always find a BinOp token and never reach this case
        unsafe {
            unreachable_unchecked();
        }
    };

    match action {
        Action::DefineVar(name) => {
            let walker = EvalWalker { ctx: parse_ctx };

            let result = walker.walk(&entry)?;
            parse_ctx.register_var(&name, result.clone())?; // FIXME: what should we do with the boolean that gets returned?
            Ok(Some(result))
        }
        Action::DefineFunc(name, params) => {
            let func = Function::new(name, params, entry, parse_ctx)?;
            parse_ctx.register_func(func);
            Ok(None)
        }
        Action::DefineRecFunc(_, _, _) => unimplemented!(),
        Action::Eval => {
            let walker = EvalWalker { ctx: parse_ctx };

            walker.walk(&entry).map(|val| Some(val))
        }
    }
}

pub(crate) struct EvalWalker<'a> {
    pub(crate) ctx: &'a ParseContext,
}

impl AstWalker<Number> for EvalWalker<'_> {
    fn walk_binop(&self, node: &BinOpNode) -> PResult<Number> {
        let lhs = self.walk(&node.lhs)?;
        let rhs = self.walk(&node.rhs)?;
        Ok(node.op.eval((Some(lhs), Some(rhs))))
    }

    fn walk_lit(&self, node: &LiteralToken) -> PResult<Number> {
        if node.kind == LiteralKind::CharSeq {
            if let Some(val) = self.ctx.lookup_var(&node.content) {
                Ok(val)
            } else {
                diagnostic_builder!(
                    self.ctx.get_input().clone(),
                    format!("there is no variable or constant named {}", node.content)
                )
            }
        } else {
            let val = node.content.parse::<Number>().unwrap();
            Ok(val)
        }
    }

    fn walk_unary_op(&self, node: &UnaryOpNode) -> PResult<Number> {
        let val = self.walk(&*node.val)?;
        node.op.eval(val, self.ctx)
    }

    fn walk_maybe_func(&self, node: &MaybeFuncNode) -> PResult<Number> {
        if let Some(result) = self.ctx.try_call_func(
            &node.name,
            node.param.as_ref().map_or_else(
                || Box::new([]) as Box<[AstNode]>,
                |param| Box::new([*param.clone()]),
            ),
        ) {
            let result = result?;
            self.walk(&result)
        } else {
            let ast = if let Some(param) = &node.param {
                AstNode::BinOp(BinOpNode {
                    op: BinOpKind::Multiply,
                    lhs: Box::new(AstNode::Lit(LiteralToken {
                        span: Span::NONE,
                        content: node.name.clone(),
                        kind: LiteralKind::Number,
                        trailing_space: TrailingSpace::Yes,
                    })),
                    rhs: param.clone(),
                })
            } else {
                AstNode::Lit(LiteralToken {
                    span: Span::NONE,
                    content: node.name.clone(),
                    kind: LiteralKind::Number,
                    trailing_space: TrailingSpace::Yes,
                })
            };
            self.walk(&ast)
        }
    }

    fn walk_func_call_or_func_def(&self, node: &FuncCallOrFuncDefNode) -> PResult<Number> {
        if let Some(result) = self.ctx.try_call_func(&node.name, node.params.clone()) {
            let result = result?;
            self.walk(&result)
        } else {
            diagnostic_builder!(
                self.ctx.get_input().clone(),
                format!("there is no function named {}", node.name)
            )
        }
    }
}
