use parser::ast::*;
use evaluator::object::*;

pub struct BuiltinsFunctions;

impl Default for BuiltinsFunctions {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinsFunctions {
    pub fn new() -> Self {
        BuiltinsFunctions {}
    }

    pub fn get_builtins(&self) -> Vec<(Ident, Object)> {
        vec![
            add_builtin("print", 1, bprint_fn),
            add_builtin("len", 1, blen_fn),
            add_builtin("head", 1, bhead_fn),
            add_builtin("tail", 1, btail_fn),
            add_builtin("cons", 2, bcons_fn),
        ]
    }
}

fn add_builtin(name: &str, param_num: usize, func: BuiltinFunction) -> (Ident, Object) {
    let name_string = String::from(name);
    (
        Ident(name_string.clone()),
        Object::Builtin(name_string, param_num, func),
    )
}

fn bprint_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(&Object::String(ref t)) => {
            println!("{}", t);
            Ok(Object::Null)
        }
        Some(o) => {
            println!("{}", o);
            Ok(Object::Null)
        }
        _ => Err(String::from("invalid arguments for print")),
    }
}

fn blen_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(&Object::String(ref s)) => Ok(Object::Integer(s.len() as i64)),
        Some(&Object::Array(ref arr)) => Ok(Object::Integer(arr.len() as i64)),
        _ => Err(String::from("invalid arguments for len")),
    }
}

fn bhead_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(&Object::Array(ref arr)) => {
            match arr.first() {
                None => Err(String::from("empty array")),
                Some(x) => Ok(x.clone()),
            }
        }
        _ => Err(String::from("invalid arguments for head")),
    }
}

fn btail_fn(args: Vec<Object>) -> Result<Object, String> {
    match args.get(0) {
        Some(&Object::Array(ref arr)) => {
            match arr.len() {
                0 => Err(String::from("empty array")),
                _ => {
                    let tail = &arr[1..];
                    Ok(Object::Array(tail.to_vec()))
                }
            }
        }
        _ => Err(String::from("invalid arguments for tail")),
    }
}

fn bcons_fn(args: Vec<Object>) -> Result<Object, String> {
    let mut args = args.iter();
    match (args.next(), args.next()) {
        (Some(o), Some(&Object::Array(ref os))) => {
            let mut vectors = vec![o.clone()];
            for object in os {
                vectors.push(object.clone());
            }
            Ok(Object::Array(vectors))
        }
        _ => Err(String::from("invalid arguments for cons")),
    }
}
