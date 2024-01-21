use crate::ast::{AstNode, AstNodeKind, BinOpNode, FuncCallOrFuncDefNode, MaybeFuncNode, RecFuncTail, UnaryOpNode, AstEntry};
use crate::ast_walker::{AstWalker, AstWalkerMut};
use crate::{diagnostic_builder, diagnostic_builder_spanned};
use crate::error::DiagnosticBuilder;
use crate::parser::{Action, Function, PResult, ParseContext, RecursiveFunction};
use crate::shared::{BinOpKind, LiteralKind, LiteralToken, Number, SignKind, TrailingSpace, UnaryOpKind, num_from_f64};
use crate::span::Span;
use std::hint::unreachable_unchecked;
use std::mem;
use std::ops::{Add, Neg, Sub};
use crate::_lib::ANSMode;

pub(crate) fn eval(
    parse_ctx: &mut ParseContext,
    ans_mode: ANSMode,
    mut entry: AstEntry,
    tail: Option<RecFuncTail>,
) -> Result<Option<Number>, DiagnosticBuilder> {
        if entry.node.kind() != AstNodeKind::BinOp {
            let walker = EvalWalker { ctx: parse_ctx };
            let ret = if let AstNode::PartialBinOp(node) = &entry.node {
                if node.op == BinOpKind::Eq {
                    if let AstNode::Lit(lit) = &node.rhs.node {
                        if lit.kind == LiteralKind::CharSeq {
                            if let Some(last) = parse_ctx.get_last().clone() {
                                parse_ctx.register_var(&lit.content, last.clone(), entry.span)?;
                                return Ok(Some(last));
                            } else {
                                return diagnostic_builder_spanned!(format!("there is no previous result to define `{}` with", &lit.content), entry.span);
                            }
                        }
                    }
                    return diagnostic_builder_spanned!("can't set a previous result equal to some new one that's not a variable name", entry.span);
                }
                if ans_mode == ANSMode::Never {
                    return diagnostic_builder_spanned!("ANS is disabled!", entry.span); // FIXME: improve this!
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
                        return diagnostic_builder_spanned!("There is no previous result to be used in the ANS calculation.", entry.span);
                    }
                }
            } else if let AstNode::UnaryOp(node) = &entry.node {
                if ans_mode == ANSMode::Always && (node.op == UnaryOpKind::Neg || node.op == UnaryOpKind::Pos) {
                    if let Some(last) = parse_ctx.get_last() {
                        if node.op == UnaryOpKind::Neg {
                            walker.walk(&*node.val).map(|x| last.sub(x))
                        } else {
                            walker.walk(&*node.val).map(|x| last.add(x))
                        }
                    } else {
                        return diagnostic_builder_spanned!("There is no previous result to be used in the ANS calculation.", entry.span);
                    }
                } else {
                    walker.walk(&entry)
                }
            } else {
                walker.walk(&entry)
            }.map(|val| Some(val));

            return ret;
        }

    if let Some(last) = parse_ctx.get_last().as_ref() {
        let op_replacer = PartialOpReplacer(last);
        op_replacer.walk(&mut entry)?;
    }

    // FIXME: get rid of all that cloning by consuming entry
    let (entry, action) = if let AstNode::BinOp(node) = entry.node {
        if node.op == BinOpKind::Eq {
            (
                *node.rhs,
                match node.lhs.node {
                    AstNode::FuncCallOrFuncDef(func) => {
                        Action::DefineFunc(func.name, {
                            let mut result = Vec::with_capacity(func.params.len());
                            for param in func.params.into_iter() {
                                if let AstNode::Lit(lit) = &param.node {
                                    result.push(lit.content.clone()); // FIXME: can we get rid of this clone?
                                } else {
                                    return diagnostic_builder_spanned!(
                                        "expected a parameter name, but got an expression",
                                        param.span
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
                                if let AstNode::Lit(lit) = param.node {
                                    Ok(Box::new([lit.content]))
                                } else {
                                    diagnostic_builder_spanned!(
                                        "expected a parameter name, but got an expression",
                                        param.span
                                    )
                                }
                            },
                        )?
                    }),
                    AstNode::Lit(var) => Action::DefineVar(var.content.clone()),
                    AstNode::BinOp(_) => {
                        return diagnostic_builder_spanned!("expected a function signature or variable name, but got a binary operation", entry.span);
                    }
                    AstNode::UnaryOp(_) => {
                        return diagnostic_builder_spanned!("expected a function signature or variable name, but got an unary operation", entry.span);
                    }
                    AstNode::PartialBinOp(_) => unreachable!(),
                },
            )
        } else {
            (AstEntry {
                span: node.lhs.span.merge_with(node.rhs.span),
                node: AstNode::BinOp(node),
            }, Action::Eval(None))
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
            parse_ctx.register_var(&name, result.clone(), entry.span)?; // FIXME: what should we do with the boolean that gets returned?
            Ok(Some(result))
        }
        Action::DefineFunc(name, params) => {
            if let Some(tail) = tail {
                let span = entry.span;
                let func = RecursiveFunction::new(name, params, entry, tail.idx, tail.val.node, parse_ctx, span)?;
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

pub(crate) fn resolve_simple(parse_ctx: &ParseContext, node: &AstEntry) -> PResult<Number> {
    let walker = EvalWalker { ctx: parse_ctx };
    walker.walk(node)
}

pub(crate) struct EvalWalker<'a> {
    pub(crate) ctx: &'a ParseContext,
}

impl AstWalker<Number> for EvalWalker<'_> {
    fn walk_binop(&self, node: &BinOpNode, span: Span) -> PResult<Number> {
        let lhs = self.walk(&node.lhs)?;
        let rhs = self.walk(&node.rhs)?;
        Ok(node.op.eval((Some(lhs), Some(rhs))))
    }

    fn walk_lit(&self, node: &LiteralToken, span: Span) -> PResult<Number> {
        if node.kind == LiteralKind::CharSeq {
            if let Some(val) = self.ctx.lookup_var(&node.content) {
                Ok(val)
            } else {
                diagnostic_builder_spanned!(
                    format!("there is no variable, function or constant named {}", node.content),
                    span
                )
            }
        } else {
            let val = num_from_f64(node.content.parse::<f64>().expect(&format!("expected number, but found {}", &node.content)));
            Ok(val)
        }
    }

    fn walk_unary_op(&self, node: &UnaryOpNode, span: Span) -> PResult<Number> {
        let val = self.walk(&*node.val)?;
        node.op.eval(val, self.ctx)
    }

    fn walk_maybe_func(&self, node: &MaybeFuncNode, span: Span) -> PResult<Number> {
        if let Some(result) = self.ctx.try_call_func(
            &node.name,
            node.param.as_ref().map_or_else(
                || Box::new([]) as Box<[AstEntry]>,
                |param| Box::new([*param.clone()]),
            ),
            span,
        ) {
            let result = result?;
            self.walk(&result)
        } else {
            let ast = if let Some(param) = &node.param {
                AstNode::BinOp(BinOpNode {
                    op: BinOpKind::Multiply,
                    lhs: Box::new(AstEntry {
                        span: Span::NONE, // FIXME: support name spans!
                        node: AstNode::Lit(LiteralToken {
                            span: Span::NONE, // FIXME: support name spans!
                            content: node.name.clone(),
                            kind: LiteralKind::CharSeq,
                            trailing_space: TrailingSpace::Yes,
                        }),
                    }),
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
            let ast = AstEntry {
                span,
                node: ast,
            };
            self.walk(&ast)
        }
    }

    fn walk_func_call_or_func_def(&self, node: &FuncCallOrFuncDefNode, span: Span) -> PResult<Number> {
        if let Some(result) = self.ctx.try_call_func(&node.name, node.params.clone(), span) {
            let result = result?;
            self.walk(&result)
        } else {
            diagnostic_builder_spanned!(
                format!("there is no function named {}", node.name),
                span
            )
        }
    }
}

struct PartialOpReplacer<'a>(&'a Number);

impl<'a> AstWalkerMut<()> for PartialOpReplacer<'a> {
    fn walk_binop(&self, node: &mut BinOpNode, span: Span) -> PResult<()> {
        self.walk(&mut node.lhs);
        Ok(())
    }

    fn walk_lit(&self, node: &mut LiteralToken, span: Span) -> PResult<()> {
        Ok(())
    }

    fn walk_unary_op(&self, node: &mut UnaryOpNode, span: Span) -> PResult<()> {
        Ok(())
    }

    fn walk_maybe_func(&self, node: &mut MaybeFuncNode, span: Span) -> PResult<()> {
        Ok(())
    }

    fn walk_func_call_or_func_def(&self, node: &mut FuncCallOrFuncDefNode, span: Span) -> PResult<()> {
        Ok(())
    }

    fn walk(&self, entry: &mut AstEntry) -> PResult<()> {
        match &mut entry.node {
            AstNode::FuncCallOrFuncDef(_) => Ok(()),
            AstNode::MaybeFunc(_) => Ok(()),
            AstNode::Lit(_) => Ok(()),
            AstNode::BinOp(node) => self.walk_binop(node, entry.span),
            AstNode::UnaryOp(_) => Ok(()), // FIXME: should we support lhs arg unary ops for ANS?
            AstNode::PartialBinOp(node) => {
                if node.op == BinOpKind::Eq {
                    return diagnostic_builder_spanned!("can't set a previous result equal to some new one that's not a variable name", entry.span);
                }
                entry.node = AstNode::BinOp(BinOpNode { op: node.op, lhs: Box::new(AstEntry {
                    span: entry.span,
                    node: AstNode::Lit(LiteralToken {
                        span: entry.span,
                        content: self.0.clone().to_string(),
                        kind: LiteralKind::Number,
                        trailing_space: TrailingSpace::No,
                    }),
                }), rhs: node.rhs.clone() });
                Ok(())
            },
        }
    }
}
