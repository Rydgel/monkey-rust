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
        )
    }

    fn bprint(&self) -> Object {
        Object::Builtin(
            String::from("print"), 1, |args: Vec<Object>| {
                match args[..] {
                    [Object::String(ref t)] => {
                        println!("{}", t);
                        Ok(Object::Null)
                    },
                    [ref o] => {
                        println!("{}", o);
                        Ok(Object::Null)
                    },
                    _ => Err(String::from("invalid arguments for print")),
                }
            }
        )
    }
}
