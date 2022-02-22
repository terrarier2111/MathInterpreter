// FIXME: Rename this to lib.rs once possible!

use crate::__lib;
use crate::__lib::EvalContext;
use crate::_lib::DiagnosticsPrintingOrder::After;
use crate::parser::ParseResult;
use crate::shared::Number;

pub struct Config {
    diagnostics: DiagnosticsConfig,
    pub(crate) ans_mode: ANSMode,
    pub(crate) mode: Mode,
}

impl Config {
    pub fn new(diagnostics: DiagnosticsConfig, ans_mode: ANSMode, mode: Mode) -> Self {
        Self {
            diagnostics,
            ans_mode,
            mode,
        }
    }
}

#[derive(Default)]
pub struct DiagnosticsConfig {
    color: bool,                              // FIXME: Currently unsupported
    printing_order: DiagnosticsPrintingOrder, // FIXME: Currently unsupported
}

pub enum DiagnosticsPrintingOrder {
    Before,
    After,
}

impl Default for DiagnosticsPrintingOrder {
    fn default() -> Self {
        After
    }
}

#[derive(Copy, Clone)]
#[repr(u8)]
pub enum Mode {
    Eval,
    Simplify,
    Solve,
}

#[derive(Copy, Clone, Eq, PartialEq)]
#[repr(u8)]
pub enum ANSMode {
    Never,
    WhenImplicit,
    Always,
}

pub fn eval(input: String, eval_ctx: &mut EvalContext) -> ParseResult<Option<Number>> {
    eval_ctx.parse_ctx.set_input(input.clone());
    __lib::eval(input, eval_ctx)
}

pub fn new_eval_ctx(config: Config) -> EvalContext {
    EvalContext::new(config)
}
