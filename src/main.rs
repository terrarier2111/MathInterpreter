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

use crate::_lib::{ANSMode, Config, DiagnosticsConfig, Mode};

// TODO: add ans support to new parser
// TODO: support 2 types of numbers (via an enum): whole numbers and floating point numbers in order to avoid inaccurracies when doing calculations

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
            Ok(val) => println!("Result: {:?}", val),
            Err(err) => println!("Encountered an error:\n{}", err),
        }
    }
}
