extern crate core;

mod __lib;
mod _lib;
mod ast;
mod ast_walker;
mod equation_evaluator;
mod equation_simplifier;
mod equation_solver;
mod error;
mod lexer;
mod parser;
mod shared;
mod span;
mod token_stream;
mod utils;
mod cli_core;
mod cmd_line;

use std::{sync::Arc, ops::Deref, fmt::Display, error::Error};

use __lib::EvalContext;
use cli_core::{CommandBuilder, CommandImpl, UsageBuilder, CommandParam};
use cmd_line::{CLIBuilder, CmdLineInterface, FallbackHandler};

use crate::_lib::{ANSMode, Config, DiagnosticsConfig, Mode};

// TODO: add ans support to new parser
// TODO: support 2 types of numbers (via an enum): whole numbers and floating point numbers in order to avoid inaccurracies when doing calculations
// TODO: do a better job at preserving spans until the end

fn main() {
    let cli = CLIBuilder::new().prompt("Please insert what is to be evaluated: ".to_string())
    .command(CommandBuilder::new("$mode", CmdMode).params(UsageBuilder::new().required(CommandParam {
        name: "mode".to_string(),
        ty: cli_core::CommandParamTy::String(cli_core::CmdParamStrConstraints::Variants { variants: &["simplify", "eval", "solve"], ignore_case: true }),
    }))).fallback(Box::new(CalcFallback)).build();
    let cli = CmdLineInterface::new(cli);
    let context = Arc::new(_lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::WhenImplicit,
        Mode::Eval,
    )));
    let calc = Calculator {
        ctx: context,
        cli,
    };
    loop {
        calc.cli.await_input(&calc);
    }
}

pub struct Calculator {
    pub ctx: Arc<EvalContext>,
    pub cli: CmdLineInterface<Calculator>,
}

struct CmdMode;

impl CommandImpl for CmdMode {
    type CTX = Calculator;

    fn execute(&self, ctx: &Self::CTX, input: &[&str]) -> anyhow::Result<()> {
        let mode = match input[0].to_lowercase().as_str() {
            "eval" => Ok(Mode::Eval),
            "simplify" => Ok(Mode::Simplify),
            "solve" => Ok(Mode::Solve),
            _ => Err(ModeDoesNotExistError(input[0].to_string())),
        }?;
        let mut cfg = ctx.ctx.config.load().deref().deref().clone();
        cfg.mode = mode;
        ctx.ctx.config.store(Arc::new(cfg));
        ctx.cli.println(format!("Switched to {:?} mode", mode).as_str());
        Ok(())
    }
}

#[derive(Debug)]
struct ModeDoesNotExistError(String);

impl Error for ModeDoesNotExistError {}

impl Display for ModeDoesNotExistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the mode ")?;
        f.write_str(self.0.as_str())?;
        f.write_str(" does not exist")
    }
}

struct CalcFallback;

impl FallbackHandler<Calculator> for CalcFallback {
    fn handle(&self, input: String, window: &cmd_line::Window<Calculator>, ctx: &Calculator) -> anyhow::Result<bool> {
        let result = _lib::eval(input, &ctx.ctx);
        match result.0 {
            Ok(val) => {
                if let Some(num) = val.0 {
                    ctx.cli.println(format!("Result: {}", num).as_str());
                    if let Some(err) = val.1 {
                        ctx.cli.println(format!("{:?}", err).as_str());
                    }
                } else if let Some(err) = val.1 {
                    ctx.cli.println(format!("{:?}", err).as_str());
                } else {
                    ctx.cli.println("Ok!");
                }
            },
            Err(err) => {
                ctx.cli.println_input_aligned(format!("{}", err).as_str());
            }
        }
        Ok(false)
    }
}
