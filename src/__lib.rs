use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::Config;
use crate::lexer::Lexer;
use crate::parser::{ParseContext, Parser, PResult};

pub fn eval(input: String, eval_ctx: &mut EvalContext) -> Result<Option<f64>, EvalError> {
    let mut lexer = Lexer::new();
    let tokens = lexer.lex(input).unwrap();
    let mut parser = Parser::new(tokens);
    let parsed = parser.parse(&mut eval_ctx.parse_ctx);
    match parsed {
        PResult::Ok(_, val) => {
            // TODO: Deal with the diagnostics!
            Ok(val)
        },
        PResult::Err(_, err) => {
            // TODO: Deal with the diagnostics!
            Err(EvalError(err.to_string()))
        }
    }
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