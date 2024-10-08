// FIXME: Rename this to lib.rs once possible!

use crate::__lib;
use crate::__lib::EvalContext;
use crate::parser::ParseResult;
use crate::shared::Number;

#[derive(Clone)]
pub struct Config {
    diagnostics: DiagnosticsConfig,
    pub(crate) ans_mode: ANSMode,
    pub(crate) circle_unit: CircleUnit,
}

impl Config {
    #[inline]
    pub const fn new(
        diagnostics: DiagnosticsConfig,
        ans_mode: ANSMode,
        circle_unit: CircleUnit,
    ) -> Self {
        Self {
            diagnostics,
            ans_mode,
            circle_unit,
        }
    }
}

#[derive(Clone, Default)]
pub struct DiagnosticsConfig {
    color: bool,                              // FIXME: Currently unsupported
    printing_order: DiagnosticsPrintingOrder, // FIXME: Currently unsupported
}

#[derive(Copy, Clone, Default)]
pub enum DiagnosticsPrintingOrder {
    Before,
    #[default]
    After,
}

#[derive(Copy, Clone, Debug)]
pub enum Mode {
    Eval,
    Simplify,
    Solve,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum ANSMode {
    Never,
    WhenImplicit,
    Always,
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum CircleUnit {
    Radians,
    Degrees,
}

pub fn eval(input: String, eval_ctx: &EvalContext) -> ParseResult<Option<Number>> {
    eval_ctx.parse_ctx.write().unwrap().set_input(input.clone());
    __lib::eval(input, eval_ctx, Mode::Eval)
}

pub fn simplify(input: String, eval_ctx: &EvalContext) -> ParseResult<Option<Number>> {
    eval_ctx.parse_ctx.write().unwrap().set_input(input.clone());
    __lib::eval(input, eval_ctx, Mode::Simplify)
}

pub fn solve(input: String, eval_ctx: &EvalContext) -> ParseResult<Option<Number>> {
    eval_ctx.parse_ctx.write().unwrap().set_input(input.clone());
    __lib::eval(input, eval_ctx, Mode::Solve)
}

#[inline]
pub fn new_eval_ctx(config: Config) -> EvalContext {
    EvalContext::new(config)
}
