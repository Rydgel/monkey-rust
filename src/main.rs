extern crate monkey_lib;
#[macro_use]
extern crate clap;
extern crate nom;

use monkey_lib::evaluator::*;
use monkey_lib::lexer::token::*;
use monkey_lib::lexer::*;
use monkey_lib::parser::*;
use nom::*;
use std::fs::File;
use std::io::prelude::*;

use cmd::*;
mod cmd;

fn read_file(file_path: String) -> Result<String, ::std::io::Error> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() {
    let code_string = match cmd::read_command() {
        Command::FileReadCommand(file_path) => read_file(file_path).ok(),
        Command::RunInlineCode(code) => Some(code),
        Command::Noop => None,
    };

    if code_string.is_some() {
        let code_string = code_string.unwrap();
        let mut evaluator = Evaluator::new();
        let lex_tokens = Lexer::lex_tokens(code_string.as_bytes());
        match lex_tokens {
            Ok((_, r)) => {
                let tokens = Tokens::new(&r);
                let parsed = Parser::parse_tokens(tokens);
                match parsed {
                    Ok((_, program)) => {
                        let eval = evaluator.eval_program(&program);
                        println!("{}", eval);
                    }
                    Err(Err::Error(_)) => println!("Parser error"),
                    Err(Err::Failure(_)) => println!("Parser failure"),
                    Err(Err::Incomplete(_)) => println!("Incomplete parsing"),
                }
            }
            Err(Err::Error(_)) => println!("Lexer error"),
            Err(Err::Failure(_)) => println!("Lexer failure"),
            Err(Err::Incomplete(_)) => println!("Incomplete lexing"),
        }
    }
}
