# monkey-rust

An interpreter for the Monkey programming language written in Rust

![The Monkey Programming Language](https://cloud.githubusercontent.com/assets/1013641/22617482/9c60c27c-eb09-11e6-9dfa-b04c7fe498ea.png)

## What’s Monkey?

Monkey has a C-like syntax, supports **variable bindings**, **prefix** and **infix operators**, has **first-class** and **higher-order functions**, can handle **closures** with ease and has **integers**, **booleans**, **arrays** and **hashes** built-in.

There is a book about learning how to make an interpreter: [Writing An Interpreter In Go](https://interpreterbook.com/#the-monkey-programming-language). This is where the Monkey programming language come from.

## Instruction

### Build and test

```bash
$ cargo build
$ cargo test
```

### Running the REPL

```bash
$ cargo run --release --bin monkey_repl
```

### Running the Interpreter

```bash
$ cargo run --release --bin monkey_exe -- --src examples/hash.mk
```

## License

[BSD3](LICENSE)
