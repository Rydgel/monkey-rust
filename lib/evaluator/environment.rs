use std::collections::HashMap;
use evaluator::object::*;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    parent: Option<Box<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
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
        let name = (*name).clone();
        let val = (*val).clone();
        self.store.insert(name, val);
    }

    pub fn get(&self, name: &String) -> Option<&Object> {
        self.store.get(name)
    }
}
