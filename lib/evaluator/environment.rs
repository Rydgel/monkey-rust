use std::collections::HashMap;
use evaluator::object::*;
use evaluator::builtins::*;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        let builtins_functions = BuiltinsFunctions::new();
        let builtins = builtins_functions.get_builtins();

        Environment {
            store: HashMap::new(),
            parent: None,
        }
    }

    pub fn new_with_outer(outer: Box<Environment>) -> Self {
        Environment {
            store: HashMap::new(),
            parent: Some(outer),
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
