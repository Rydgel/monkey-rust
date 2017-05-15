use parser::ast::*;
use evaluator::object::*;

pub struct BuiltinsFunctions;

impl BuiltinsFunctions {
    pub fn new() -> Self {
        BuiltinsFunctions {}
    }

    pub fn get_builtins(&self) -> Vec<(Ident, Object)> {
        vec!(
            (Ident(String::from("print")), self.bprint()),
            (Ident(String::from("len")), self.blen()),
            (Ident(String::from("head")), self.bhead()),
            (Ident(String::from("tail")), self.btail()),
            (Ident(String::from("cons")), self.bcons()),
        )
    }

    fn bprint(&self) -> Object {
        Object::Builtin(
            String::from("print"), 1, |args: Vec<Object>| {
                match args.iter().next() {
                    Some(&Object::String(ref t)) => {
                        println!("{}", t);
                        Ok(Object::Null)
                    },
                    Some(ref o) => {
                        println!("{}", o);
                        Ok(Object::Null)
                    },
                    _ => Err(String::from("invalid arguments for print")),
                }
            }
        )
    }

    fn blen(&self) -> Object {
        Object::Builtin(
            String::from("len"), 1, |args: Vec<Object>| {
                match args.iter().next() {
                    Some(&Object::String(ref s)) => Ok(Object::Integer(s.len() as i64)),
                    Some(&Object::Array(ref arr)) => Ok(Object::Integer(arr.len() as i64)),
                    _ => Err(String::from("invalid arguments for len")),
                }
            }
        )
    }

    fn bhead(&self) -> Object {
        Object::Builtin(
            String::from("head"), 1, |args: Vec<Object>| {
                match args.iter().next() {
                    Some(&Object::Array(ref arr)) => {
                        match arr.first() {
                            None => Err(String::from("empty array")),
                            Some(x) => Ok(x.clone()),
                        }
                    },
                    _ => Err(String::from("invalid arguments for head")),
                }
            }
        )
    }

    fn btail(&self) -> Object {
        Object::Builtin(
            String::from("tail"), 1, |args: Vec<Object>| {
                match args.iter().next() {
                    Some(&Object::Array(ref arr)) => {
                        match arr.len() {
                            0 => Err(String::from("empty array")),
                            _ => {
                                let tail = &arr[1..];
                                Ok(Object::Array(tail.to_vec()))
                            },
                        }
                    },
                    _ => Err(String::from("invalid arguments for tail")),
                }
            }
        )
    }

    fn bcons(&self) -> Object {
        Object::Builtin(
            String::from("cons"), 2, |args: Vec<Object>| {
                let mut args = args.iter();
                match (args.next(), args.next()) {
                    (Some(&ref o), Some(&Object::Array(ref os))) => {
                        let mut vectors = vec!();
                        vectors.push(o.clone());
                        for object in os { vectors.push(object.clone()); }
                        Ok(Object::Array(vectors))
                    },
                    _ => Err(String::from("invalid arguments for cons")),
                }
            }
        )
    }
}
