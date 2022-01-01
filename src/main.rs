use crate::lexer::Lexer;
use crate::parser::{ParseContext, Parser, PResult};

mod lexer;
mod parser;
mod shared;
mod utils;
mod error;
mod equation_solver;

fn main() {
    let mut context = ParseContext::new();
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
    }
}
