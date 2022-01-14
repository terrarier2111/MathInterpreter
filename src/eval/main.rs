
// mod eval;

use crate::eval::_lib::{Config, DiagnosticsConfig};
use colored::*;
use crate::eval::{_lib, utils};

pub fn main() {
    colored::control::set_override(true);
    let mut context = _lib::new_eval_ctx(Config::new(DiagnosticsConfig::default()));
    loop {
        let input = utils::input("Please insert what is to be evaluated: ".to_string()).unwrap();
        let result = _lib::eval(input, &mut context);
        match result {
            Result::Ok(val) => println!("Result: {:?}", val),
            Result::Err(err) => println!("Encountered an error:\n{}", err),
        }
    }
}
