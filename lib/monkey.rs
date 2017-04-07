#![feature(box_syntax, box_patterns)]
#[macro_use]

extern crate nom;

pub mod lexer;
pub mod parser;
pub mod evaluator;
