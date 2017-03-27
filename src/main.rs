extern crate monkey_lib;

use monkey_lib::lexer::*;

fn main() {
    println!("{:?}", Lexer::lex_tokens(b"let five = 5;"));
}
