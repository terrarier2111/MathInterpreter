use crate::lexer::Lexer;
use crate::parser::{PResult, ParseContext, Parser};
use crate::shared::Number;
use crate::Config;

pub fn eval(input: String, eval_ctx: &mut EvalContext) -> PResult<Option<Number>> {
    let lexer = Lexer::new();
    let tokens = match lexer.lex(input) {
        Ok(val) => val,
        Err(diagnostic_builder) => {
            return Err(diagnostic_builder);
        }
    };
    let mut parser = Parser::new(tokens);
    let parsed = parser.parse(&mut eval_ctx.parse_ctx);
    parsed
}

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
