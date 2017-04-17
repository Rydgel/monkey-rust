use std::collections::HashMap;
use evaluator::object::*;
use evaluator::builtins::*;
use parser::ast::*;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<Box<Environment>>,
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

    pub fn new_with_outer(outer: Box<Environment>) -> Self {
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

    pub fn get(&self, name: &String) -> Option<&Object> {
        let current_scope_object = self.store.get(name);
        match current_scope_object {
            Some(_) => current_scope_object,
            None => match &self.parent {
                &Some(box ref p) => p.get(name),
                &None => None,
            }
        }
    }
}
