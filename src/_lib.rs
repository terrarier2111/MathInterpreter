// FIXME: Rename this to lib.rs once possible!

use crate::__lib;
use crate::__lib::EvalContext;
use crate::_lib::DiagnosticsPrintingOrder::After;
use crate::parser::PResult;
use crate::shared::Number;

pub struct Config {
    decimal_accuracy: u16,
    diagnostics: DiagnosticsConfig,
}

impl Config {

    pub fn new(decimal_accuracy: u16, diagnostics: DiagnosticsConfig) -> Self {
        Self {
            decimal_accuracy,
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