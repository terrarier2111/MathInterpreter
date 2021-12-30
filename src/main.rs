use crate::lexer::Lexer;
use crate::parser::{ParseContext, Parser};

mod lexer;
mod parser;
mod shared;
mod utils;

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
        println!("result: {:?}", parsed);
    }
}
