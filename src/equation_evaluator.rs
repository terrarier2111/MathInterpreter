use crate::ast::{AstNode, AstNodeKind, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, RecFuncTail, UnaryOpNode};
use crate::ast_walker::AstWalker;
use crate::diagnostic_builder;
use crate::error::DiagnosticBuilder;
use crate::parser::{Action, Function, PResult, ParseContext, RecursiveFunction};
use crate::shared::{BinOpKind, LiteralKind, LiteralToken, Number, SignKind, TrailingSpace, UnaryOpKind};
use crate::span::Span;
use std::hint::unreachable_unchecked;
use std::mem;
use std::ops::{Add, Neg, Sub};
use crate::_lib::ANSMode;

pub(crate) fn eval(
    parse_ctx: &mut ParseContext,
    ans_mode: ANSMode,
    entry: AstNode,
    tail: Option<RecFuncTail>,
) -> Result<Option<Number>, DiagnosticBuilder> {
        if entry.kind() != AstNodeKind::BinOp {
            let walker = EvalWalker { ctx: parse_ctx };
            let ret = if let AstNode::PartialBinOp(node) = entry {
                if node.op == BinOpKind::Eq {
                    return diagnostic_builder!(parse_ctx.get_input().clone(), "can't set a previous result equal to some new one - ANS doesn't work with `Eq`.");
                }
                if ans_mode == ANSMode::Never {
                    return diagnostic_builder!(parse_ctx.get_input().clone(), "ANS is disabled!"); // FIXME: improve this!
                }
                if let Some(last) = parse_ctx.get_last() {
                    match node.op {
                        BinOpKind::Eq | BinOpKind::Add | BinOpKind::Subtract => {
                            // `Eq` was just checked for above and the other two
                            // are transformed into unaries in the lexer.
                            unreachable!()
                        },
                        _ => {
                            walker.walk(&*node.rhs).map(|x| node.op.eval((Some(last.clone()), Some(x))))
                        }
                    }
                } else {
                    if ans_mode == ANSMode::WhenImplicit && (node.op == BinOpKind::Add || node.op == BinOpKind::Subtract) {
                        let mut ret = walker.walk(&*node.rhs);
                        if node.op == BinOpKind::Subtract {
                            ret = ret.map(|x| x.neg());
                        }
                        ret
                    } else {
                        return diagnostic_builder!(parse_ctx.get_input().clone(), "There is no previous result to be used in the ANS calculation.");
                    }
                }
            } else if let AstNode::UnaryOp(node) = &entry {
                if ans_mode == ANSMode::Always && (node.op == UnaryOpKind::Neg || node.op == UnaryOpKind::Pos) {
                    if let Some(last) = parse_ctx.get_last() {
                        if node.op == UnaryOpKind::Neg {
                            walker.walk(&*node.val).map(|x| last.sub(x))
                        } else {
                            walker.walk(&*node.val).map(|x| last.add(x))
                        }
                    } else {
                        return diagnostic_builder!(parse_ctx.get_input().clone(), "There is no previous result to be used in the ANS calculation.");
                    }
                } else {
                    walker.walk(&entry)
                }
            } else {
                walker.walk(&entry)
            }.map(|val| Some(val));

            return ret;
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
                    AstNode::PartialBinOp(_) => {
                        /*if ans_mode == ANSMode::Never {
                            return diagnostic_builder!(parse_ctx.get_input().clone(), "expected a function signature or variable name, but got a partial binary operation");
                        }
                        match node.op {
                            BinOpKind::Add => {
                                if ans_mode != ANSMode::WhenImplicit {
                                    parse_ctx
                                }
                            }
                            BinOpKind::Subtract => {}
                            BinOpKind::Divide => {}
                            BinOpKind::Multiply => {}
                            BinOpKind::Modulo => {}
                            BinOpKind::Pow => {}
                            BinOpKind::Eq => panic!(),
                        }*/
                        panic!()
                    }
                },
            )
        } else {
            (AstNode::BinOp(node), Action::Eval(None))
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
            if let Some(tail) = tail {
                let func = RecursiveFunction::new(name, params, entry, tail.idx, tail.val, parse_ctx)?;
                parse_ctx.register_rec_func(func);
            } else {
                let func = Function::new(name, params, entry, parse_ctx)?;
                parse_ctx.register_func(func);
            }
            Ok(None)
        }
        Action::DefineRecFunc(_, _, _) => unimplemented!(),
        Action::Eval(_) => {
            let walker = EvalWalker { ctx: parse_ctx };

            walker.walk(&entry).map(|val| Some(val))
        }
    }
}

pub(crate) fn resolve_simple(parse_ctx: &ParseContext, node: &AstNode) -> PResult<Number> {
    let walker = EvalWalker { ctx: parse_ctx };
    walker.walk(node)
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
                    format!("there is no variable, function or constant named {}", node.content)
                )
            }
        } else {
            let val = node.content.parse::<Number>().expect(&format!("expected number, but found {}", &node.content));
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
                        kind: LiteralKind::CharSeq,
                        trailing_space: TrailingSpace::Yes,
                    })),
                    rhs: param.clone(),
                })
            } else {
                AstNode::Lit(LiteralToken {
                    span: Span::NONE,
                    content: node.name.clone(),
                    kind: LiteralKind::CharSeq,
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

    #[inline]
    fn get_input(&self) -> &String {
        self.ctx.get_input()
    }
}
