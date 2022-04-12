use crate::core_types::{Expression, Identifier, Program, Statement, Type};
use crate::type_checker::TypeCheckError::NoScope;
use crate::typed_types::{TypedExpression, TypedProgram, TypedStatement};
use std::collections::HashMap;
use std::iter::Map;

pub struct TypeCheckEnv {
    // Vec representing the scopes and the hashmap the
    // variables (and their types) within that scope.
    vars: Vec<HashMap<Identifier, Type>>,
}

impl TypeCheckEnv {
    fn new() -> Self {
        TypeCheckEnv {
            vars: vec![HashMap::new()],
        }
    }

    /// If a variable with the given id exists,
    /// returns Some with its type,
    /// Otherwise returns none.
    fn lookup_var(&self, id: Identifier) -> Option<Type> {
        for scope in self.vars {
            if let Some(t) = scope.get(&id) {
                return Some(t.clone());
            }
        }
        None
    }

    /// If a variable with the given id exists within the current scope,
    /// returns Some with its type,
    /// otherwise returns none.
    fn lookup_var_current_scope(&self, id: &Identifier) -> Option<Type> {
        let scope = self.vars.last()?;
        if let Some(t) = scope.get(id) {
            return Some(t.clone());
        }
        None
    }

    fn insert_var(self, id: &Identifier, t: Type) -> TypeCheckResult<()> {
        self.vars.last().ok_or(Err(TypeCheckError::NoScope));
    }
}

pub enum TypeCheckError {
    #[error("The variable already exists within the current scope")]
    VarExists,
    #[error("No scope exists")]
    NoScope,
}

pub type TypeCheckResult<T> = Result<T, TypeCheckError>;

pub fn type_check(prog: Program) -> TypedProgram {
    let env = TypeCheckEnv::new();

    TypedProgram {
        typed_stmts: prog
            .statements
            .into_iter()
            .map(|s| type_check_stmt(s, env))
            .collect(),
    }
}

fn type_check_stmt(stmt: Statement, env: TypeCheckEnv) -> TypeCheckResult<TypedStatement> {
    match stmt {
        Statement::Let(id, expr) => {
            if let Some(_) = env.lookup_var_current_scope(&id) {
                return Err(TypeCheckError::VarExists);
            }
            let expr_type = type_check_expr(expr, env);
        }
    }
}

fn type_check_expr(expr: Expression, env: TypeCheckEnv) -> TypedExpression {}
