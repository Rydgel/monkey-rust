use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use evaluator::object::*;
use evaluator::builtins::*;
use parser::ast::*;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new() -> Self {
        let mut hashmap = HashMap::new();
        Self::fill_env_with_builtins(&mut hashmap);
        Environment {
            store: hashmap,
            parent: None,
        }
    }

    pub fn new_with_outer(outer: Rc<RefCell<Environment>>) -> Self {
        let mut hashmap = HashMap::new();
        Self::fill_env_with_builtins(&mut hashmap);
        Environment {
            store: hashmap,
            parent: Some(outer),
        }
    }

    fn fill_env_with_builtins(hashmap: &mut HashMap<String, Object>) {
        let builtins_functions = BuiltinsFunctions::new();
        let builtins = builtins_functions.get_builtins();
        for (ident, object) in builtins {
            let Ident(name) = ident.clone();
            hashmap.insert(name.clone(), object);
        }
    }

    pub fn set(&mut self, name: &String, val: &Object) -> () {
        let name = name.clone();
        let val = val.clone();
        self.store.insert(name, val);
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        match self.store.get(name) {
            Some(&ref o) => Some(o.clone()),
            None => match &self.parent {
                &Some(ref parent_env) => {
                    let env = parent_env.borrow();
                    env.get(name)
                },
                &None => None,
            }
        }
    }
}
