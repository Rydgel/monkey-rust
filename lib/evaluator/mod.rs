pub mod object;
pub mod environment;

use parser::ast::*;
use evaluator::object::*;
use evaluator::environment::*;

pub struct Evaluator {
    env: Environment
}

impl Evaluator {
    pub fn new() -> Self {
        Evaluator {
            env: Environment::new()
        }
    }

    pub fn eval_program(&mut self, prog: Program) -> Object {
        match prog[..] {
            [] => Object::Null,
            [ref s] => self.eval_statement(s.clone()),
            [ref s, ref ss..] => {
                let object = self.eval_statement(s.clone());
                if object.is_returned() {
                    object
                } else {
                    self.eval_program(ss.to_vec())
                }
            },
        }
    }

    pub fn eval_statement(&mut self, stmt: Stmt) -> Object {
        match stmt {
            Stmt::ExprStmt(expr) => self.eval_expr(expr),
            Stmt::ReturnStmt(expr) => Object::ReturnValue(box self.eval_expr(expr)),
            Stmt::LetStmt(ident, expr) => {
                let object = self.eval_expr(expr);
                self.register_ident(ident, object)
            },
        }
    }

    pub fn register_ident(&mut self, ident: Ident, object: Object) -> Object {
        let Ident(name) = ident;
        self.env.set(name, object.clone());
        object
    }

    pub fn eval_expr(&mut self, expr: Expr) -> Object {
        Object::Null
    }
}
