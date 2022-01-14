// FIXME: Rename this to lib.rs once possible!

use crate::eval::__lib;
use crate::eval::__lib::EvalContext;
use crate::eval::_lib::DiagnosticsPrintingOrder::After;
use crate::eval::parser::PResult;
use crate::eval::shared::Number;

pub struct Config {
    diagnostics: DiagnosticsConfig,
}

impl Config {

    pub fn new(diagnostics: DiagnosticsConfig) -> Self {
        Self {
            diagnostics
        }
    }

}

#[derive(Default)]
pub struct DiagnosticsConfig {
    color: bool, // FIXME: Currently unsupported
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

pub fn eval(input: String, eval_ctx: &mut EvalContext) -> PResult<Option<Number>> {
    eval_ctx.parse_ctx.set_input(input.clone());
    __lib::eval(input, eval_ctx)
}

pub fn new_eval_ctx(config: Config)  -> EvalContext {
    EvalContext::new(config)
}