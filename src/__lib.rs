use crate::Config;
use crate::lexer::Lexer;
use crate::parser::{ParseContext, Parser, PResult};

pub fn eval(input: String, eval_ctx: &mut EvalContext) -> PResult<Option<f64>> {
    let lexer = Lexer::new();
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