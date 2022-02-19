mod __lib;
mod _lib;
mod equation_simplifier;
mod equation_solver;
mod error;
mod lexer;
mod parser;
mod shared;
mod utils;

use crate::_lib::{ANSMode, Config, DiagnosticsConfig, Mode};
use colored::*;

fn main() {
    colored::control::set_override(true);
    let mut context = _lib::new_eval_ctx(Config::new(
        DiagnosticsConfig::default(),
        ANSMode::WhenImplicit,
        Mode::Normal,
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
