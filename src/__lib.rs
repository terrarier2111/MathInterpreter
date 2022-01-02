use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::Config;
use crate::error::DiagnosticBuilder;
use crate::lexer::Lexer;
use crate::parser::{ParseContext, Parser, PResult};
use crate::shared::Token;

pub fn eval(input: String, eval_ctx: &mut EvalContext) -> PResult<Option<f64>> {
    let mut lexer = Lexer::new();
    let tokens = match lexer.lex(input) {
        Ok(val) => val,
        Err(diagnostic_builder) => {
            return Err(diagnostic_builder);
        },
    };
    let mut parser = Parser::new(tokens);
    let parsed = parser.parse(&mut eval_ctx.parse_ctx);
    parsed
}

#[derive(Debug)]
pub struct EvalError(String);

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&*self.0)
    }
}

impl Error for EvalError {}

pub struct EvalContext {
    pub(crate) parse_ctx: ParseContext,
    pub(crate) config: Config,
}

impl EvalContext {

    pub fn new(config: Config) -> Self {
        Self {
            parse_ctx: ParseContext::new(),
            config,
        }
    }

}