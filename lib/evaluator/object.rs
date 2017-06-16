use std::cell::RefCell;
use std::rc::Rc;
use std::fmt;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use parser::ast::*;
use evaluator::environment::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Function(Vec<Ident>, BlockStmt, Rc<RefCell<Environment>>),
    Builtin(String, usize, BuiltinFunction),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

pub type BuiltinFunction = fn(Vec<Object>) -> Result<Object, String>;

impl Object {
    pub fn is_returned(&self) -> bool {
        match *self {
            Object::ReturnValue(_) => true,
            _ => false,
        }
    }

    pub fn returned(&self) -> Self {
        match *self {
            Object::ReturnValue(ref o) => *o.clone(),
            ref o => o.clone(),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Integer(ref i) => write!(f, "{}", i),
            Object::Boolean(ref b) => {
                if *b {
                    write!(f, "true")
                } else {
                    write!(f, "false")
                }
            }
            Object::String(ref s) => write!(f, "{}", s),
            Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("[");
                for (i, o) in v.iter().enumerate() {
                    fmt_string.push_str(format!("{}", o).as_str());
                    if i < v.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("]");
                write!(f, "{}", fmt_string)
            }
            Object::Hash(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("{");
                for (i, (k, v)) in hashmap.iter().enumerate() {
                    fmt_string.push_str(format!("{} : {}", k, v).as_str());
                    if i < hashmap.len() - 1 {
                        fmt_string.push_str(", ");
                    }
                }
                fmt_string.push_str("}");
                write!(f, "{}", fmt_string)
            }
            Object::Function(_, _, _) => write!(f, "[function]"),
            Object::Builtin(ref name, _, _) => write!(f, "[built-in function: {}]", *name),
            Object::Null => write!(f, "null"),
            Object::ReturnValue(ref o) => write!(f, "{}", *o),
            Object::Error(ref s) => write!(f, "Error: {}", s),
        }
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match *self {
            Object::Integer(ref i) => i.hash(state),
            Object::Boolean(ref b) => b.hash(state),
            Object::String(ref s) => s.hash(state),
            _ => "".hash(state),
        }
    }
}
