use crate::core_types::{Expression, Identifier, Program, Statement, Type};
use crate::type_checker::TypeCheckError::NoSuchFunc;
use crate::typed_types::{TypedExpression, TypedProgram, TypedStatement};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeCheckEnv {
    // Vec representing the scopes and the hashmap the
    // variables (and their types) within that scope.
    vars: Vec<HashMap<Identifier, Type>>,

    functions: HashMap<Identifier, (Vec<Type>, Type)>,
}

impl TypeCheckEnv {
    fn new() -> Self {
        let mut default_functions = HashMap::new();
        default_functions.insert(
            Identifier::from("print_number"),
            (vec![Type::Integer], Type::Void),
        );

        TypeCheckEnv {
            vars: vec![HashMap::new()],
            functions: default_functions,
        }
    }

    /// If a variable with the given id exists,
    /// returns Some with its type,
    /// Otherwise returns none.
    fn lookup_var(&self, id: &Identifier) -> Option<Type> {
        for scope in self.vars.iter() {
            if let Some(t) = scope.get(id) {
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

    fn insert_var(&mut self, id: Identifier, t: Type) -> TypeCheckResult<()> {
        let mut scope = self.vars.pop().ok_or(TypeCheckError::NoScope)?;
        scope.insert(id, t);
        self.vars.push(scope);
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeCheckError {
    #[error("The variable already exists within the current scope")]
    VarExists,
    #[error("No scope exists")]
    NoScope,
    #[error("Types does not match, expected `{0}`, got `{1}`")]
    TypeMismatch(Type, Type),
    #[error("No such function exists `{0}`")]
    NoSuchFunc(Identifier),
    #[error("Call to func `{0}` has an invalid amount of arguments `{1}`, expected `{2}` args")]
    ArgLenMissmatch(Identifier, usize, usize),
    #[error("Invalid argument type in call to function `{0}`, got type `{1}`, expected `{2}`")]
    ArgTypeMissmatch(Identifier, Type, Type),
    #[error("No variable exists with name `{0}`")]
    NoSuchVar(Identifier),
    #[error("Arithmetic operation with non-number type `{0}`")]
    ArithInvalidType(Type),
}

pub type TypeCheckResult<T> = Result<T, TypeCheckError>;

pub fn type_check(prog: Program) -> TypeCheckResult<TypedProgram> {
    let mut env = TypeCheckEnv::new();

    Ok(TypedProgram {
        typed_stmts: prog
            .statements
            .into_iter()
            .map(|s| type_check_stmt(s, &mut env))
            .collect::<TypeCheckResult<Vec<TypedStatement>>>()?,
    })
}

fn type_check_stmt(stmt: Statement, env: &mut TypeCheckEnv) -> TypeCheckResult<TypedStatement> {
    match stmt {
        Statement::Let(id, expr) => {
            if let Some(_) = env.lookup_var_current_scope(&id) {
                return Err(TypeCheckError::VarExists);
            }
            let expr_typed = type_check_expr(&expr, env)?;
            let t = expr_typed.get_type();
            env.insert_var(id.clone(), t.clone())?;
            Ok(TypedStatement::Let(id, expr_typed, t))
        }
        Statement::Expression(expr) => {
            let expr_typed = type_check_expr(&expr, env)?;
            let t = expr_typed.get_type();
            Ok(TypedStatement::Expression(expr_typed, t))
        }
    }
}

fn type_check_expr(expr: &Expression, env: &mut TypeCheckEnv) -> TypeCheckResult<TypedExpression> {
    Ok(match expr {
        Expression::IntegerLiteral(n) => TypedExpression::IntegerLiteral(n.clone()),
        Expression::BooleanLiteral(b) => TypedExpression::BooleanLiteral(b.clone()),
        Expression::Plus(a, b) => {
            let (a, b, t) = type_check_arith(a, b, env)?;
            TypedExpression::Plus(a, b, t)
        }
        Expression::Minus(a, b) => {
            let (a, b, t) = type_check_arith(a, b, env)?;
            TypedExpression::Minus(a, b, t)
        }
        Expression::Times(a, b) => {
            let (a, b, t) = type_check_arith(a, b, env)?;
            TypedExpression::Times(a, b, t)
        }
        Expression::Divide(a, b) => {
            let (a, b, t) = type_check_arith(a, b, env)?;
            TypedExpression::Divide(a, b, t)
        }
        Expression::FunctionCall(name, args) => {
            let (arg_types, ret_type) = match env.functions.get(name) {
                Some(vals) => Ok(vals.clone()),
                None => Err(NoSuchFunc(name.clone())),
            }?;

            let typed_args = type_check_func_args(&name, args, &arg_types, env)?;

            TypedExpression::FunctionCall(name.clone(), typed_args, ret_type)
        }
        Expression::Variable(name) => match env.lookup_var(name) {
            Some(t) => TypedExpression::Variable(name.clone(), t),
            None => return Err(TypeCheckError::NoSuchVar(name.clone())),
        },
    })
}

fn type_check_func_args(
    name: &Identifier,
    args: &Vec<Expression>,
    expected: &Vec<Type>,
    env: &mut TypeCheckEnv,
) -> TypeCheckResult<Vec<TypedExpression>> {
    if expected.len() != args.len() {
        return Err(TypeCheckError::ArgLenMissmatch(
            name.clone(),
            args.len(),
            expected.len(),
        ));
    }

    args.into_iter()
        .enumerate()
        .map(|(i, arg)| {
            let typed = type_check_expr(arg, env)?;
            let expected_type = match expected.get(i) {
                None => {
                    return Err(TypeCheckError::ArgLenMissmatch(
                        name.clone(),
                        args.len(),
                        expected.len(),
                    ))
                }
                Some(a) => a.clone(),
            };

            if typed.get_type() != expected_type {
                return Err(TypeCheckError::ArgTypeMissmatch(
                    name.clone(),
                    typed.get_type(),
                    expected_type,
                ));
            }

            Ok(typed)
        })
        .collect::<TypeCheckResult<Vec<TypedExpression>>>()
}

fn type_check_arith(
    a: &Expression,
    b: &Expression,
    env: &mut TypeCheckEnv,
) -> TypeCheckResult<(Box<TypedExpression>, Box<TypedExpression>, Type)> {
    let typed_a = type_check_expr(&a, env)?;
    let typed_b = type_check_expr(&b, env)?;

    let type_a = typed_a.get_type();
    let type_b = typed_b.get_type();

    if type_a != type_b {
        return Err(TypeCheckError::TypeMismatch(type_a, type_b));
    }

    if type_a != Type::Integer {
        return Err(TypeCheckError::ArithInvalidType(type_a));
    }

    Ok((Box::new(typed_a), Box::new(typed_b), type_a))
}
