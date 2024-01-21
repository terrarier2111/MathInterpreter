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
mod conc_once_cell;
mod sized_box;

use std::{sync::{Arc, Mutex}, ops::Deref, fmt::Display, error::Error};

use __lib::EvalContext;
use _lib::CircleUnit;
use cli_core::{CommandBuilder, CommandImpl, UsageBuilder, CommandParam, EnumVal};
use cmd_line::{CLIBuilder, CmdLineInterface, FallbackHandler};
use parser::ConstantSetKind;

use crate::_lib::{ANSMode, Config, DiagnosticsConfig, Mode};

// TODO: support 2 types of numbers (via an enum): whole numbers and floating point numbers in order to avoid inaccurracies when doing calculations

fn main() {
    let cli = CLIBuilder::new().prompt("Please insert what is to be evaluated: ".to_string())
    .command(CommandBuilder::new("$mode", CmdMode).params(UsageBuilder::new().required(CommandParam {
        name: "mode".to_string(),
        ty: cli_core::CommandParamTy::String(cli_core::CmdParamStrConstraints::Variants { variants: &["simplify", "eval", "solve"], ignore_case: true }),
    }))).command(CommandBuilder::new("$set", CmdSet).params(UsageBuilder::new().required(CommandParam {
        name: "action".to_string(),
        ty: cli_core::CommandParamTy::String(cli_core::CmdParamStrConstraints::Variants { variants: &["register", "unregister", "list"], ignore_case: true }),
    }).required(CommandParam { name: "set".to_string(), ty: cli_core::CommandParamTy::Enum(cli_core::CmdParamEnumConstraints::IgnoreCase(&[("math", EnumVal::None), ("physics", EnumVal::None)])) })))
    .fallback(Box::new(CalcFallback)).build();
    let cli = CmdLineInterface::new(cli);
    let context = Arc::new(_lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::WhenImplicit,
        CircleUnit::Radians,
    )));
    let calc = Calculator {
        ctx: context,
        cli,
        mode: Mutex::new(Mode::Eval),
    };
    loop {
        calc.cli.await_input(&calc);
    }
}

pub struct Calculator {
    pub ctx: Arc<EvalContext>,
    pub cli: CmdLineInterface<Calculator>,
    mode: Mutex<Mode>,
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
        *ctx.mode.lock().unwrap() = mode;
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

struct CmdSet;

impl CommandImpl for CmdSet {
    type CTX = Calculator;

    // TODO: support `all` superset.
    fn execute(&self, ctx: &Self::CTX, input: &[&str]) -> anyhow::Result<()> {
        let set = match input[1].to_lowercase().as_str() {
            "math" => Ok(ConstantSetKind::Math),
            "physics" => Ok(ConstantSetKind::Physics),
            _ => Err(anyhow::Error::from(SetDoesNotExistError(input[1].to_string()))),
        }?;
        let mut set_ctx = ctx.ctx.parse_ctx.write().unwrap();
        match input[0].to_lowercase().as_str() {
            "register" => {
                if set_ctx.registered_sets.contains(&set) {
                    return Err(anyhow::Error::from(SetAlreadyRegisteredError(input[1].to_string())));
                }
                set_ctx.register_set(set);
                ctx.cli.println(format!("Registered the set `{}`", input[1]).as_str());
                Ok(())
            },
            "unregister" => {
                if !set_ctx.registered_sets.contains(&set) {
                    return Err(anyhow::Error::from(SetNotRegisteredError(input[1].to_string())));
                }
                set_ctx.unregister_set(set);
                ctx.cli.println(format!("Unregistered the set `{}`", input[1]).as_str());
                Ok(())
            },
            "list" => {
                ctx.cli.println(format!("Sets({}):", set_ctx.registered_sets.len()).as_str());
                for set in set_ctx.registered_sets.iter() {
                    let mut values = String::new();
                    for value in set.values().iter() {
                        values.push_str(value.0);
                        values.push_str(": ");
                        values.push_str(value.1.as_f64().to_string().as_str()); // TODO: decide how many places after 0 we want!
                        values.push_str(", ");
                    }
                    if !values.is_empty() {
                        values.pop();
                        values.pop();
                    }
                    ctx.cli.println(format!("{}: {}", set.name(), values).as_str());
                }
                Ok(())
            }
            _ => Err(anyhow::Error::from(ModeDoesNotExistError(input[0].to_string())))
        }
    }
}

#[derive(Debug)]
struct SetDoesNotExistError(String);

impl Error for SetDoesNotExistError {}

impl Display for SetDoesNotExistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the set ")?;
        f.write_str(self.0.as_str())?;
        f.write_str(" does not exist")
    }
}

#[derive(Debug)]
struct SetAlreadyRegisteredError(String);

impl Error for SetAlreadyRegisteredError {}

impl Display for SetAlreadyRegisteredError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the set ")?;
        f.write_str(self.0.as_str())?;
        f.write_str(" is already registered")
    }
}

#[derive(Debug)]
struct SetNotRegisteredError(String);

impl Error for SetNotRegisteredError {}

impl Display for SetNotRegisteredError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("the set ")?;
        f.write_str(self.0.as_str())?;
        f.write_str(" is not yet registered")
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
