mod lexer;
mod parser;
mod shared;
mod utils;
mod error;
mod equation_solver;
mod _lib;
mod __lib;
use crate::_lib::{Config, DiagnosticsConfig};

fn main() {
    /*let mut context = ParseContext::new();
    loop {
        let input = utils::input("Please insert what is to be evaluated: ".to_string()).unwrap();
        let mut lexer = Lexer::new();
        let tokens = lexer.lex(input).unwrap();
        for token in tokens.iter() {
            println!("{:?}", token);
        }
        println!("------------------------------");
        let mut parser = Parser::new(tokens);
        let parsed = parser.parse(&mut context);
        println!("------------------------------");
        match parsed {
            PResult::Ok(_, val) => println!("Result: {:?}", val),
            PResult::Err(_, err) => {
                println!("Error: {:?}", err);
            }
        }
    }*/
    let mut context = _lib::new_eval_ctx(Config::new(DiagnosticsConfig::default()));
    loop {
        let input = utils::input("Please insert what is to be evaluated: ".to_string()).unwrap();
        let result = _lib::eval(input, &mut context);
        match result {
            Result::Ok(val) => println!("Result: {:?}", val),
            Result::Err(err) => {
                println!("Error: {}", err);
            }
        }
    }
}
