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

use crate::_lib::{ANSMode, Config, DiagnosticsConfig, Mode};

// TODO: add ans support to new parser
// TODO: support 2 types of numbers (via an enum): whole numbers and floating point numbers in order to avoid inaccurracies when doing calculations
// TODO: do a better job at preserving spans until the end

fn main() {
    colored::control::set_override(true);
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::WhenImplicit,
        Mode::Eval,
    ));
    loop {
        let input = utils::input("Please insert what is to be evaluated: ".to_string()).unwrap();
        let result = _lib::eval(input, &mut context);
        match result.0 {
            Ok(val) => {
                if let Some(num) = val.0 {
                    println!("Result: {}", num);
                    if let Some(err) = val.1 {
                        println!("{:?}", err);
                    }
                } else if let Some(err) = val.1 {
                    println!("{:?}", err);
                } else {
                    println!("Ok!");
                }
            },
            Err(err) => println!("Encountered an error:\n{}", err),
        }
    }
}
