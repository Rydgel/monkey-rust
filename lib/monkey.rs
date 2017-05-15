#![feature(box_syntax, box_patterns)]
#![feature(closure_to_fn_coercion)]
#[macro_use]

extern crate nom;

pub mod lexer;
pub mod parser;
pub mod evaluator;
