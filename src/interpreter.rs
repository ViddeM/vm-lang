use crate::core_types::{ComparisonOperator, Identifier, Type};
use crate::typed_types::{TypedExpression, TypedProgram, TypedStatement};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Integer(i) => format!("Integer({})", i),
                Value::Boolean(b) => format!("Boolean({})", b),
                Value::Void => String::from("Void"),
            }
        )
    }
}

#[derive(Debug)]
pub struct InterpretEnv {
    vars: Vec<HashMap<Identifier, Value>>,
    functions: HashMap<Identifier, Vec<TypedStatement>>,
}

impl InterpretEnv {
    fn new() -> Self {
        let mut default_functions = HashMap::new();
        default_functions.insert(Identifier::from("print_number"), vec![]);
        Self {
            vars: vec![HashMap::new()],
            functions: default_functions,
        }
    }

    fn insert_var(&mut self, id: Identifier, val: Value) -> InterpretResult<()> {
        let mut scope = self.vars.pop().ok_or(InterpretError::NoScope)?;
        scope.insert(id, val);
        self.vars.push(scope);
        Ok(())
    }

    fn update_var(&mut self, id: Identifier, val: Value) -> InterpretResult<()> {
        let mut scopes = vec![];
        loop {
            let mut scope = self
                .vars
                .pop()
                .ok_or(InterpretError::NoSuchVar(id.clone()))?;
            if scope.contains_key(&id) {
                scope.insert(id.clone(), val);
                self.vars.push(scope);
                break;
            }
            scopes.push(scope);
        }
        scopes.into_iter().for_each(|s| self.vars.push(s));
        Ok(())
    }

    fn lookup_var(&self, id: &Identifier) -> Option<Value> {
        for scope in self.vars.iter() {
            if let Some(val) = scope.get(id) {
                return Some(val.clone());
            }
        }
        None
    }

    fn new_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }
}

#[derive(Debug, thiserror::Error)]
pub enum InterpretError {
    #[error("No scope exists")]
    NoScope,
    #[error(
        "Value type error (should've been handled by typechecker!), expected `{0}`, got `{1}`"
    )]
    ValTypeError(Type, Value),
    #[error("No such function `{0}`")]
    NoSuchFunction(Identifier),
    #[error("Invalid amount of function arguments, expected `{0}`, got `{1}`")]
    InvalidFunctionArgCount(usize, usize),
    #[error("Variable `{0}` could not be found")]
    NoSuchVar(Identifier),
    #[error("Cannot compare value `{0}` with value `{1}`")]
    CompareValueMismatch(Value, Value),
}

pub type InterpretResult<T> = Result<T, InterpretError>;

pub fn interpret(prog: TypedProgram) -> InterpretResult<()> {
    let mut env = InterpretEnv::new();
    for s in prog.typed_stmts.into_iter() {
        eval_statement(&s, &mut env)?;
    }

    Ok(())
}

fn eval_statement(stmt: &TypedStatement, env: &mut InterpretEnv) -> InterpretResult<()> {
    Ok(match stmt {
        TypedStatement::Let(id, expr) => {
            let val = eval_expression(&expr, env)?;
            env.insert_var(id.clone(), val)?;
        }
        TypedStatement::Expression(expr) => {
            eval_expression(&expr, env)?;
        }
        TypedStatement::While(expr, stmts) => loop {
            env.new_scope();
            match eval_expression(&expr, env)? {
                Value::Boolean(true) => {
                    for s in stmts.iter() {
                        eval_statement(s, env)?;
                    }
                    env.pop_scope();
                }
                Value::Boolean(false) => {
                    env.pop_scope();
                    break;
                }
                v => return Err(InterpretError::ValTypeError(Type::Boolean, v)),
            };
        },
        TypedStatement::If(expr, stmts) => {
            env.new_scope();
            match eval_expression(&expr, env)? {
                Value::Boolean(true) => {
                    for s in stmts.iter() {
                        eval_statement(s, env)?;
                    }
                }
                Value::Boolean(false) => {}
                v => return Err(InterpretError::ValTypeError(Type::Boolean, v)),
            }
            env.pop_scope();
        }
        TypedStatement::IfElse(expr, stmts, else_stmts) => {
            env.new_scope();
            match eval_expression(&expr, env)? {
                Value::Boolean(true) => {
                    for s in stmts.iter() {
                        eval_statement(s, env)?;
                    }
                }
                Value::Boolean(false) => {
                    for s in else_stmts.iter() {
                        eval_statement(s, env)?;
                    }
                }
                v => return Err(InterpretError::ValTypeError(Type::Boolean, v)),
            }
        }
    })
}

fn eval_expression(expr: &TypedExpression, env: &mut InterpretEnv) -> InterpretResult<Value> {
    Ok(match expr {
        TypedExpression::IntegerLiteral(i) => Value::Integer(i.clone()),
        TypedExpression::BooleanLiteral(b) => Value::Boolean(b.clone()),
        TypedExpression::Plus(a, b, _) => eval_arith(&a, &b, env, |a, b| a + b)?,
        TypedExpression::Minus(a, b, _) => eval_arith(&a, &b, env, |a, b| a - b)?,
        TypedExpression::Times(a, b, _) => eval_arith(&a, &b, env, |a, b| a * b)?,
        TypedExpression::Divide(a, b, _) => eval_arith(&a, &b, env, |a, b| a * b)?,
        TypedExpression::FunctionCall(name, args, _) => call_function(name, args, env)?,
        TypedExpression::Variable(name, _) => env
            .lookup_var(name)
            .ok_or(InterpretError::NoSuchVar(name.clone()))?,
        TypedExpression::Comparison(a, b, op, _) => eval_comparison(a, b, op, env)?,
        TypedExpression::Assignment(name, expr, _) => {
            let val = eval_expression(expr, env)?;
            env.update_var(name.clone(), val.clone())?;
            val
        }
    })
}

fn eval_arith(
    a: &TypedExpression,
    b: &TypedExpression,
    env: &mut InterpretEnv,
    op: fn(i64, i64) -> i64,
) -> InterpretResult<Value> {
    let val_a = eval_expression(a, env)?;
    let val_b = eval_expression(b, env)?;

    let num_a = match val_a {
        Value::Integer(a) => a,
        _ => return Err(InterpretError::ValTypeError(Type::Integer, val_a)),
    };
    let num_b = match val_b {
        Value::Integer(b) => b,
        _ => return Err(InterpretError::ValTypeError(Type::Integer, val_b)),
    };

    Ok(Value::Integer(op(num_a, num_b)))
}

fn eval_comparison(
    a: &TypedExpression,
    b: &TypedExpression,
    op: &ComparisonOperator,
    env: &mut InterpretEnv,
) -> InterpretResult<Value> {
    let val_a = eval_expression(a, env)?;
    let val_b = eval_expression(b, env)?;

    let cmp_ok = match (&val_a, &val_b) {
        (Value::Integer(a), Value::Integer(b)) => compare(a, b, op),
        (Value::Boolean(a), Value::Boolean(b)) => compare(a, b, op),
        _ => {
            return Err(InterpretError::CompareValueMismatch(
                val_a.clone(),
                val_b.clone(),
            ))
        }
    };

    Ok(Value::Boolean(cmp_ok))
}

fn compare<T>(a: &T, b: &T, op: &ComparisonOperator) -> bool
where
    T: PartialEq<T> + PartialOrd<T>,
{
    match op {
        ComparisonOperator::Equals => a == b,
        ComparisonOperator::NotEquals => a != b,
        ComparisonOperator::LessOrEqual => a <= b,
        ComparisonOperator::GreaterOrEqual => a >= b,
        ComparisonOperator::LessThan => a < b,
        ComparisonOperator::GreaterThan => a > b,
    }
}

fn call_function(
    name: &Identifier,
    args: &Vec<TypedExpression>,
    env: &mut InterpretEnv,
) -> InterpretResult<Value> {
    let arg_vals = args
        .iter()
        .map(|a| eval_expression(a, env))
        .collect::<InterpretResult<Vec<Value>>>()?;

    let statements = match name.as_str() {
        "print_number" => {
            let arg = args
                .first()
                .ok_or(InterpretError::InvalidFunctionArgCount(1, 0))?;

            if arg_vals.len() > 1 {
                return Err(InterpretError::InvalidFunctionArgCount(1, arg_vals.len()));
            }

            return match eval_expression(arg, env)? {
                Value::Integer(a) => {
                    println!("Print number {}", a);
                    Ok(Value::Void)
                }
                val => Err(InterpretError::ValTypeError(Type::Integer, val)),
            };
        }
        _ => env
            .functions
            .get(name)
            .ok_or(InterpretError::NoSuchFunction(name.clone()))?,
    };

    todo!("Self-implemented functions are not yet supported")
}
