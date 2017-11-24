extern crate monkey_lib;
extern crate rustyline;
extern crate nom;

use rustyline::completion::FilenameCompleter;
use rustyline::error::ReadlineError;
use rustyline::{Config, CompletionType, Editor};
use monkey_lib::lexer::*;
use monkey_lib::lexer::token::*;
use monkey_lib::parser::*;
use monkey_lib::evaluator::*;
use nom::*;

#[cfg(unix)]
static PROMPT: &'static str = "\x1b[1;32mmonkey >>\x1b[0m ";

#[cfg(windows)]
static PROMPT: &'static str = "monkey >> ";


fn main() {
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(CompletionType::List)
        .build();
    let c = FilenameCompleter::new();
    let mut rl = Editor::with_config(config);
    rl.set_completer(Some(c));

    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }

    println!();
    println!("This is the monkey language repl v0.2.3");
    println!("Press Ctrl-D or enter \"quit\" to exit.");
    println!();

    let mut evaluator = Evaluator::new();

    loop {
        let readline = rl.readline(PROMPT);
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_ref());
                let lex_tokens = Lexer::lex_tokens(line.as_bytes());
                match lex_tokens {
                    IResult::Done(_, r) => {
                        let tokens = Tokens::new(&r);
                        let parsed = Parser::parse_tokens(tokens);
                        match parsed {
                            IResult::Done(_, program) => {
                                let eval = evaluator.eval_program(&program);
                                println!("{}", eval);
                            }
                            IResult::Error(_) => println!("Parser error"),
                            IResult::Incomplete(_) => println!("Incomplete parsing"),
                        }
                    }
                    IResult::Error(_) => println!("Lexer error"),
                    IResult::Incomplete(_) => println!("Incomplete lexing"),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }

    rl.save_history("history.txt").unwrap();
}
