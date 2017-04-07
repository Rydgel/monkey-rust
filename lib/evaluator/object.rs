use std::fmt;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use parser::ast::*;
use evaluator::environment::*;

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Integer(usize),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Hash(HashMap<Object, Object>),
    Function(Vec<Ident>, BlockStmt, Environment),
    Builtin(String, BuiltinFunction),
    Null,
    ReturnValue(Box<Object>),
    Error(String),
}

type BuiltinFunction = fn(Vec<Object>) -> Object;

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Object::Integer(ref i) => write!(f, "{}", i),
            &Object::Boolean(ref b) => if *b { write!(f, "true") } else { write!(f, "false") },
            &Object::String(ref s) => write!(f, "{}", s),
            &Object::Array(ref v) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("[");
                for o in v {
                    fmt_string.push_str(format!("{}", o).as_str());
                }
                fmt_string.push_str("]");
                write!(f, "{}", fmt_string)
            },
            &Object::Hash(ref hashmap) => {
                let mut fmt_string = String::new();
                fmt_string.push_str("{");
                for (k, v) in hashmap {
                    fmt_string.push_str(format!("{} : {},", k, v).as_str());
                }
                fmt_string.push_str("}");
                write!(f, "{}", fmt_string)
            },
            &Object::Function(_, _, _) => write!(f, "[function]"),
            &Object::Builtin(ref name, _) => write!(f, "[built-in function: {}]", *name),
            &Object::Null => write!(f, "null"),
            &Object::ReturnValue(box ref o) => write!(f, "{}", *o),
            &Object::Error(ref s) => write!(f, "Error: {}", s),
        }
    }
}

/*impl PartialEq for Object {
    fn eq(&self, other: &Object) -> bool {
        match (self, other) {
            (&Object::Integer(ref i1), &Object::Integer(ref i2))
                => *i1 == *i2,
            (&Object::Boolean(ref b1), &Object::Boolean(ref b2))
                => *b1 == *b2,
            (&Object::String(ref s1), &Object::String(ref s2))
                => *s1 == *s2,
            (&Object::Array(ref v1), &Object::Array(ref v2))
                => *v1 == *v2,
            _ => false,
        }
    }
}*/

impl Eq for Object { }

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            &Object::Integer(ref i) => i.hash(state),
            &Object::Boolean(ref b) => b.hash(state),
            &Object::String(ref s) => s.hash(state),
            _ => panic!("Not supported for hashing error {}", self),
        }
    }
}
