use crate::lexer::Lexer;
use crate::parser::{ParseContext, ParseResult, Parser};
use crate::shared::Number;
use crate::Config;

pub fn eval(input: String, eval_ctx: &mut EvalContext) -> ParseResult<Option<Number>> {
    let lexer = Lexer::new();
    let tokens = match lexer.lex(input) {
        Ok(val) => val,
        Err(diagnostic_builder) => {
            return ParseResult::new_err(diagnostic_builder);
        }
    };
    println!("lexed stuff!");
    let mut parser = Parser::new(tokens, &mut eval_ctx.parse_ctx, eval_ctx.config.ans_mode);
    println!("starting parsing!");
    let parsed = parser.parse(eval_ctx.config.mode);
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
