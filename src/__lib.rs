use std::sync::{RwLock, Arc};

use swap_arc::SwapArc;

use crate::_lib::Mode;
use crate::lexer::Lexer;
use crate::parser::{ParseContext, ParseResult, Parser};
use crate::shared::Number;
use crate::Config;

pub fn eval(input: String, eval_ctx: &EvalContext, mode: Mode) -> ParseResult<Option<Number>> {
    let lexer = Lexer::new();
    let tokens = match lexer.lex(input) {
        Ok(val) => val,
        Err(diagnostic_builder) => {
            return ParseResult::new_err(diagnostic_builder);
        }
    };
    let mut parse_ctx = eval_ctx.parse_ctx.write().unwrap();
    let cfg = eval_ctx.config.load();
    let mut parser = Parser::new(tokens, &mut parse_ctx, cfg.ans_mode);
    let parsed = parser.parse(mode);
    parsed
}

pub struct EvalContext {
    pub(crate) parse_ctx: RwLock<ParseContext>,
    pub(crate) config: SwapArc<Config>,
}

impl EvalContext {
    pub fn new(config: Config) -> Self {
        Self {
            parse_ctx: RwLock::new(ParseContext::new()),
            config: SwapArc::new(Arc::new(config)),
        }
    }
}
