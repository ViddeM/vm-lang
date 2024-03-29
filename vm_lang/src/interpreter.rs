use crate::gr_std_lib::builtins::{default_function_definitions, execute_builtin, BuiltinError};
use crate::types::core_types::{
    BooleanComparisonOperator, ComparisonOperator, Identifier, Type, UnaryOperator,
};
use crate::types::typed_types::{TypedExpression, TypedFunction, TypedProgram, TypedStatement};
use crate::types::value::Value;
use std::collections::HashMap;

type FunctionEnv = Vec<HashMap<Identifier, Value>>;

pub struct InterpretEnv<'a> {
    call_stack: Vec<FunctionEnv>,
    vars: FunctionEnv,
    functions: HashMap<Identifier, InterpretFunction>,
    print: &'a mut dyn FnMut(String),
    read: &'a mut dyn FnMut() -> String,
}

#[derive(Clone, Debug)]
pub enum InterpretFunction {
    Builtin,
    Defined(TypedFunction),
}

impl<'a> InterpretEnv<'a> {
    fn new(
        print_func: &'a mut dyn FnMut(String),
        read_func: &'a mut dyn FnMut() -> String,
    ) -> Self {
        let functions = HashMap::from_iter(
            default_function_definitions()
                .into_iter()
                .map(|name| (Identifier::from(name), InterpretFunction::Builtin)),
        );

        Self {
            call_stack: vec![],
            vars: vec![HashMap::new()],
            functions: functions,
            print: print_func,
            read: read_func,
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
        scopes.reverse();
        scopes.into_iter().for_each(|s| self.vars.push(s));
        Ok(())
    }

    fn lookup_var(&self, id: &Identifier) -> Option<Value> {
        for scope in self.vars.iter().rev() {
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

    /// Prepares the environment for a new function by popping the last scope
    /// (in which the arguments for the new function is assumed to exist)
    /// and then pushing the remaining var env to the call_stack.
    fn new_func(&mut self) -> InterpretResult<()> {
        let args = self.vars.pop().ok_or(InterpretError::NoScope)?;

        let prev_func = self.vars.clone();
        self.call_stack.push(prev_func);
        self.vars = vec![args];
        Ok(())
    }

    fn func_return(&mut self) -> InterpretResult<()> {
        let func_env = self.call_stack.pop().ok_or(InterpretError::NoScope)?;
        self.vars = func_env;
        Ok(())
    }

    fn get_func(&self, name: &Identifier) -> InterpretResult<InterpretFunction> {
        let func = self
            .functions
            .get(name)
            .ok_or(InterpretError::NoSuchFunction(name.clone()))?;
        Ok(func.clone())
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
    #[error("Expected value of type list, got `{0}`")]
    ListTypeError(Value),
    #[error("No such function `{0}`")]
    NoSuchFunction(Identifier),
    #[error("Invalid amount of function arguments, expected `{0}`, got `{1}`")]
    InvalidFunctionArgCount(usize, usize),
    #[error("Variable `{0}` could not be found")]
    NoSuchVar(Identifier),
    #[error("Cannot compare value `{0}` with value `{1}`")]
    CompareValueMismatch(Value, Value),
    #[error("Expected list, got `{0}`")]
    ListTypeMissmatch(Value),
    #[error("Tried to get value outside of list bounds. List size: `{0}` Tried to get `{1}`")]
    OutOfBounds(usize, i64),
    #[error("Error whilst executing builtin function: `{0}`")]
    BuiltinError(#[from] BuiltinError),
}

pub type InterpretResult<T> = Result<T, InterpretError>;

pub fn interpret(
    prog: TypedProgram,
    print_func: &mut dyn FnMut(String),
    read_func: &mut dyn FnMut() -> String,
) -> InterpretResult<()> {
    let mut env = InterpretEnv::new(print_func, read_func);

    for f in prog.functions.into_iter() {
        env.functions
            .insert(f.name.clone(), InterpretFunction::Defined(f));
    }

    for s in prog.main_function.statements.into_iter() {
        if let Some(_) = eval_statement(&s, &mut env)? {
            break;
        }
    }

    Ok(())
}

fn eval_statement(stmt: &TypedStatement, env: &mut InterpretEnv) -> InterpretResult<Option<Value>> {
    Ok(match stmt {
        TypedStatement::Let(id, expr) => {
            let val = eval_expression(&expr, env)?;
            env.insert_var(id.clone(), val)?;
            None
        }
        TypedStatement::Expression(expr) => {
            eval_expression(&expr, env)?;
            None
        }
        TypedStatement::While(expr, stmt) => {
            loop {
                env.new_scope();
                match eval_expression(&expr, env)? {
                    Value::Boolean(true) => {
                        if let Some(s) = eval_statement(stmt, env)? {
                            env.pop_scope();
                            return Ok(Some(s));
                        }
                    }
                    Value::Boolean(false) => {
                        env.pop_scope();
                        break;
                    }
                    v => return Err(InterpretError::ValTypeError(Type::Boolean, v)),
                };
                env.pop_scope();
            }
            None
        }
        TypedStatement::For(id, expr, stmt) => {
            env.new_scope();
            let list = eval_expression(expr, env)?;
            env.pop_scope();
            match list {
                Value::List(list) => {
                    for elem in list.iter() {
                        env.new_scope();
                        env.insert_var(id.clone(), elem.clone())?;
                        if let Some(s) = eval_statement(stmt, env)? {
                            env.pop_scope();
                            return Ok(Some(s));
                        }
                        env.pop_scope();
                    }
                }
                other => return Err(InterpretError::ListTypeError(other)),
            }
            None
        }
        TypedStatement::If(expr, stmt) => {
            env.new_scope();
            let expr_val = eval_expression(&expr, env)?;
            env.pop_scope();
            env.new_scope();
            match expr_val {
                Value::Boolean(true) => {
                    if let Some(s) = eval_statement(stmt, env)? {
                        return Ok(Some(s));
                    }
                }
                Value::Boolean(false) => {}
                v => return Err(InterpretError::ValTypeError(Type::Boolean, v)),
            }
            env.pop_scope();
            None
        }
        TypedStatement::IfElse(expr, if_stmt, else_stmt) => {
            env.new_scope();
            let expr_val = eval_expression(&expr, env)?;
            env.pop_scope();
            env.new_scope();
            match expr_val {
                Value::Boolean(true) => {
                    if let Some(s) = eval_statement(if_stmt, env)? {
                        return Ok(Some(s));
                    }
                }
                Value::Boolean(false) => {
                    if let Some(s) = eval_statement(else_stmt, env)? {
                        return Ok(Some(s));
                    }
                }
                v => return Err(InterpretError::ValTypeError(Type::Boolean, v)),
            }
            env.pop_scope();
            None
        }
        TypedStatement::Return(expr_opt) => {
            let val = match expr_opt {
                None => Value::Void,
                Some(v) => eval_expression(v, env)?,
            };
            Some(val)
        }
        TypedStatement::Block(stmts) => {
            env.new_scope();
            for s in stmts.iter() {
                if let Some(val) = eval_statement(s, env)? {
                    env.pop_scope();
                    return Ok(Some(val));
                }
            }
            env.pop_scope();
            None
        }
    })
}

fn eval_expression(expr: &TypedExpression, env: &mut InterpretEnv) -> InterpretResult<Value> {
    Ok(match expr {
        TypedExpression::IntegerLiteral(i) => Value::Integer(i.clone()),
        TypedExpression::BooleanLiteral(b) => Value::Boolean(b.clone()),
        TypedExpression::StringLiteral(s) => Value::String(s.clone()),
        TypedExpression::ListLiteral(inner, _) => Value::List(
            inner
                .iter()
                .map(|e| eval_expression(e, env))
                .collect::<InterpretResult<Vec<Value>>>()?,
        ),
        TypedExpression::Plus(a, b, _) => eval_arith(&a, &b, env, |a, b| a + b)?,
        TypedExpression::Minus(a, b, _) => eval_arith(&a, &b, env, |a, b| a - b)?,
        TypedExpression::Times(a, b, _) => eval_arith(&a, &b, env, |a, b| a * b)?,
        TypedExpression::Divide(a, b, _) => eval_arith(&a, &b, env, |a, b| a * b)?,
        TypedExpression::FunctionCall(name, args, _) => call_function(name, args, env)?,
        TypedExpression::ListIndex(list, index, _) => get_val_at_list_index(list, index, env)?,
        TypedExpression::Variable(name, _) => env
            .lookup_var(name)
            .ok_or(InterpretError::NoSuchVar(name.clone()))?,
        TypedExpression::Comparison(a, b, op, _) => eval_comparison(a, b, op, env)?,
        TypedExpression::BooleanComparison(a, b, op) => eval_boolean_cmparison(a, b, op, env)?,
        TypedExpression::Assignment(name, expr, _) => {
            let val = eval_expression(expr, env)?;
            env.update_var(name.clone(), val.clone())?;
            val
        }
        TypedExpression::NotUnaryOperation(expr) => {
            let val = eval_expression(expr, env)?;
            match val {
                Value::Boolean(b) => Value::Boolean(!b),
                v => return Err(InterpretError::ValTypeError(Type::Boolean, v)),
            }
        }
        TypedExpression::PreUnaryOperation(id, op) => {
            let val = env
                .lookup_var(id)
                .ok_or(InterpretError::NoSuchVar(id.clone()))?;
            let new_val = match (val, op) {
                (Value::Integer(i), UnaryOperator::Inc) => i + 1,
                (Value::Integer(i), UnaryOperator::Dec) => i - 1,
                (val, _) => return Err(InterpretError::ValTypeError(Type::Integer, val)),
            };

            env.update_var(id.clone(), Value::Integer(new_val))?;
            Value::Integer(new_val)
        }
        TypedExpression::PostUnaryOperation(id, op) => {
            let val = env
                .lookup_var(id)
                .ok_or(InterpretError::NoSuchVar(id.clone()))?;
            let new_val = match (&val, op) {
                (Value::Integer(i), UnaryOperator::Inc) => i + 1,
                (Value::Integer(i), UnaryOperator::Dec) => i - 1,
                (val, _) => return Err(InterpretError::ValTypeError(Type::Integer, val.clone())),
            };

            env.update_var(id.clone(), Value::Integer(new_val))?;
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
        (Value::String(a), Value::String(b)) => compare(a, b, op),
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

fn eval_boolean_cmparison(
    a: &TypedExpression,
    b: &TypedExpression,
    op: &BooleanComparisonOperator,
    env: &mut InterpretEnv,
) -> InterpretResult<Value> {
    let val_a = eval_expression(a, env)?;
    let val_b = eval_expression(b, env)?;

    let cmp_res = match (val_a, val_b) {
        (Value::Boolean(a), Value::Boolean(b)) => match op {
            BooleanComparisonOperator::And => a && b,
            BooleanComparisonOperator::Or => a || b,
        },
        (a, b) => return Err(InterpretError::CompareValueMismatch(a.clone(), b.clone())),
    };
    Ok(Value::Boolean(cmp_res))
}

fn call_function(
    name: &Identifier,
    args: &Vec<TypedExpression>,
    env: &mut InterpretEnv,
) -> InterpretResult<Value> {
    env.new_scope();

    let func = env.get_func(name)?;

    match func {
        InterpretFunction::Builtin => {
            // Evaluate args
            let arg_vals = args
                .iter()
                .map(|a| eval_expression(a, env))
                .collect::<InterpretResult<Vec<Value>>>()?;

            // Setup the new function scope
            env.new_func()?;

            let val = execute_builtin(name, arg_vals, env.print, env.read)?;
            env.func_return()?;

            Ok(val)
        }
        InterpretFunction::Defined(f) => {
            // Evaluate args
            for (i, arg) in f.arguments.iter().enumerate() {
                let a = args.get(i).ok_or(InterpretError::InvalidFunctionArgCount(
                    f.arguments.len(),
                    args.len(),
                ))?;
                let val = eval_expression(a, env)?;
                env.insert_var(arg.name.clone(), val)?;
            }

            // Setup the new function scope
            env.new_func()?;

            // Execute the function
            for stmt in f.statements.iter() {
                if let Some(val) = eval_statement(stmt, env)? {
                    env.func_return()?;
                    return Ok(val);
                }
            }
            env.func_return()?;

            // If nothing has been returned, return void
            Ok(Value::Void)
        }
    }
}

fn get_val_at_list_index(
    list: &TypedExpression,
    index: &TypedExpression,
    env: &mut InterpretEnv,
) -> InterpretResult<Value> {
    let index = eval_expression(index, env)?;
    let index = match index {
        Value::Integer(i) => i,
        v => return Err(InterpretError::ValTypeError(Type::Integer, v)),
    };

    let list_val = eval_expression(list, env)?;
    let list = match list_val {
        Value::List(a) => a,
        v => return Err(InterpretError::ListTypeMissmatch(v)),
    };

    if index as usize > list.len() - 1 || index < 0 {
        return Err(InterpretError::OutOfBounds(list.len(), index));
    }

    Ok(list[index as usize].clone())
}
