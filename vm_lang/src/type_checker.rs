use crate::core_types::{Expression, Function, Identifier, Program, Statement, Type};
use crate::typed_types::{TypedExpression, TypedFunction, TypedProgram, TypedStatement};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub struct StackTrace {
    stack: Vec<String>,
}

impl StackTrace {
    fn new() -> Self {
        StackTrace { stack: vec![] }
    }

    fn push(&mut self, st: String) {
        self.stack.push(st);
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn print(&self) {
        println!("Stack:");
        for s in self.stack.iter() {
            println!("{}", s);
        }
    }
}

impl Display for StackTrace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Stack: \n{}",
            self.stack.iter().fold(String::new(), |mut s, e| {
                s.push_str(&format!("{}\n", e));
                s
            })
        )
    }
}

type FuncEnv = Vec<HashMap<Identifier, Type>>;

#[derive(Debug)]
pub struct TypeCheckEnv {
    call_stack: Vec<FuncEnv>,
    // Vec representing the scopes and the hashmap the
    // variables (and their types) within that scope.
    vars: FuncEnv,

    functions: HashMap<Identifier, (Vec<Type>, Type)>,

    stack_trace: StackTrace,
}

impl TypeCheckEnv {
    fn new() -> Self {
        let mut default_functions = HashMap::new();
        default_functions.insert(
            Identifier::from("print_number"),
            (vec![Type::Integer], Type::Void),
        );
        default_functions.insert(
            Identifier::from("print_string"),
            (vec![Type::String], Type::Void),
        );

        TypeCheckEnv {
            call_stack: vec![],
            vars: vec![HashMap::new()],
            functions: default_functions,
            stack_trace: StackTrace::new(),
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
        if scope.contains_key(&id) {
            return Err(TypeCheckError::VarExists(id.clone()));
        }
        scope.insert(id, t);
        self.vars.push(scope);
        Ok(())
    }

    fn new_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    /// Assumes that the function arguments exists in the latest scope!
    fn func_call(&mut self) -> TypeCheckResult<()> {
        let args = self.vars.pop().ok_or(TypeCheckError::NoScope)?;

        let prev_func = self.vars.clone();
        self.call_stack.push(prev_func);
        self.vars = vec![args];
        Ok(())
    }

    fn func_return(&mut self) -> TypeCheckResult<()> {
        let func_env = self.call_stack.pop().ok_or(TypeCheckError::NoScope)?;
        self.vars = func_env;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum TypeCheckError {
    #[error("The variable `{0}` already exists within the current scope")]
    VarExists(Identifier),
    #[error("No scope exists")]
    NoScope,
    #[error("Types does not match, expected `{0}`, got `{1}`")]
    TypeMismatch(Type, Type),
    #[error("No such function exists `{0}`")]
    NoSuchFunc(Identifier),
    #[error("Call to func `{0}` has an invalid amount of arguments `{1}`, expected `{2}` args")]
    ArgLenMismatch(Identifier, usize, usize),
    #[error("Invalid argument type in call to function `{0}`, got type `{1}`, expected `{2}`")]
    ArgTypeMismatch(Identifier, Type, Type),
    #[error("No variable exists with name `{0}`")]
    NoSuchVar(Identifier),
    #[error("Arithmetic operation with non-number type `{0}`")]
    ArithInvalidType(Type),
    #[error("Cannot compare type `{0}` with type `{1}`")]
    CmpTypeMismatch(Type, Type),
    #[error("Cannot assign type `{0}` to variable `{1}` of type `{2}`")]
    AssignmentMismatch(Type, Identifier, Type),
    #[error("A function with name `{0}` already exists")]
    FuncExists(Identifier),
    #[error("No main function is declared")]
    NoMainFunction,
    #[error("Invalid main function, main function must have 0 arguments and return type void")]
    InvalidMainFunction,
    #[error(
        "Function `{0}` does not end with a return statement which all non-void functions must"
    )]
    MissingReturn(Identifier),
    #[error("Cannot return type `{0}` from function `{1}` of type `{2}`")]
    InvalidReturnType(Type, Identifier, Type),
    #[error("Branches have different return types `{0}` and `{1}`")]
    BranchTypeDiff(String, String),
}

pub type TypeCheckResult<T> = Result<T, TypeCheckError>;

const MAIN_FUNC_NAME: &str = "main";

pub fn type_check(prog: Program) -> TypeCheckResult<TypedProgram> {
    let mut env = TypeCheckEnv::new();
    env.stack_trace.push(prog.to_string());

    match type_check_program(prog, &mut env) {
        Ok(p) => Ok(p),
        Err(e) => {
            println!("Error during typechecking");
            env.stack_trace.print();
            Err(e)
        }
    }
}

fn type_check_program(prog: Program, env: &mut TypeCheckEnv) -> TypeCheckResult<TypedProgram> {
    for func in prog.functions.iter() {
        type_check_function_header(&func, env)?;
        env.stack_trace.pop();
    }

    let mut typed_functions = prog
        .functions
        .into_iter()
        .map(|f| {
            let f = type_check_function(f, env)?;
            env.stack_trace.pop();
            Ok(f)
        })
        .collect::<TypeCheckResult<Vec<TypedFunction>>>()?;

    let mut main_index: Option<usize> = None;
    for (index, func) in typed_functions.iter().enumerate() {
        if &func.name == MAIN_FUNC_NAME {
            main_index = Some(index);
        }
    }
    let main_func = match main_index {
        Some(index) => typed_functions.remove(index),
        None => return Err(TypeCheckError::NoMainFunction),
    };

    if main_func.arguments.len() != 0 || main_func.return_type != Type::Void {
        return Err(TypeCheckError::InvalidMainFunction);
    }

    Ok(TypedProgram {
        main_function: main_func,
        functions: typed_functions,
    })
}

fn type_check_function_header(func: &Function, env: &mut TypeCheckEnv) -> TypeCheckResult<()> {
    env.stack_trace.push(func.to_string());
    if env.functions.contains_key(&func.name) {
        return Err(TypeCheckError::FuncExists(func.name.clone()));
    }

    let arg_types = func
        .arguments
        .iter()
        .map(|a| a.t.clone())
        .collect::<Vec<Type>>();
    env.functions
        .insert(func.name.clone(), (arg_types, func.return_type.clone()));

    Ok(())
}

fn type_check_function(func: Function, env: &mut TypeCheckEnv) -> TypeCheckResult<TypedFunction> {
    env.stack_trace.push(func.to_string());

    env.new_scope();
    for arg in func.arguments.iter() {
        env.insert_var(arg.name.clone(), arg.t.clone())?;
    }
    env.func_call()?;

    let mut ret_type: Option<Type> = None;
    let mut last_stmt_type: Option<Type> = None;
    let typed_stmts = func
        .statements
        .into_iter()
        .map(|s| {
            let (s, t) = type_check_stmt(&s, env)?;
            ret_type = check_stmt_ret_types(ret_type.clone(), t.clone())?;
            last_stmt_type = t;
            Ok(s)
        })
        .collect::<TypeCheckResult<Vec<TypedStatement>>>()?;
    env.stack_trace.pop();

    if func.return_type != Type::Void {
        if let None = last_stmt_type {
            return Err(TypeCheckError::MissingReturn(func.name.clone()));
        }
    }

    if let Some(t) = last_stmt_type {
        if t != func.return_type {
            return Err(TypeCheckError::InvalidReturnType(
                t.clone(),
                func.name.clone(),
                func.return_type.clone(),
            ));
        }
    }

    env.func_return()?;
    Ok(TypedFunction {
        name: func.name.clone(),
        arguments: func.arguments,
        statements: typed_stmts,
        return_type: func.return_type,
    })
}

fn type_check_stmt(
    stmt: &Statement,
    env: &mut TypeCheckEnv,
) -> TypeCheckResult<(TypedStatement, Option<Type>)> {
    env.stack_trace.push(stmt.to_string());
    match stmt {
        Statement::Let(id, expr) => {
            if let Some(_) = env.lookup_var_current_scope(id) {
                return Err(TypeCheckError::VarExists(id.clone()));
            }
            let expr_typed = type_check_expr(expr, env)?;
            env.stack_trace.pop();
            env.insert_var(id.clone(), expr_typed.get_type())?;
            Ok((TypedStatement::Let(id.clone(), expr_typed), None))
        }
        Statement::Expression(expr) => {
            let expr_typed = type_check_expr(expr, env)?;
            env.stack_trace.pop();
            Ok((TypedStatement::Expression(expr_typed), None))
        }
        Statement::While(expr, stmt) => {
            env.new_scope();
            let expr_typed = type_check_expr(expr, env)?;
            env.stack_trace.pop();
            let t = expr_typed.get_type();
            if t != Type::Boolean {
                return Err(TypeCheckError::TypeMismatch(Type::Boolean, t));
            }
            let (typed_stmt, t) = type_check_stmt(stmt, env)?;
            env.stack_trace.pop();
            env.pop_scope();
            return Ok((TypedStatement::While(expr_typed, Box::new(typed_stmt)), t));
        }
        Statement::If(expr, stmt) => {
            env.new_scope();
            let expr_typed = type_check_expr(expr, env)?;
            env.stack_trace.pop();
            let t = expr_typed.get_type();
            if t != Type::Boolean {
                return Err(TypeCheckError::TypeMismatch(Type::Boolean, t));
            }

            let (typed_stmt, t) = type_check_stmt(stmt, env)?;
            env.pop_scope();
            return Ok((TypedStatement::If(expr_typed, Box::new(typed_stmt)), t));
        }
        Statement::IfElse(expr, if_stmt, else_stmt) => {
            env.new_scope();
            let expr_typed = type_check_expr(&expr, env)?;
            env.stack_trace.pop();
            let t = expr_typed.get_type();
            if t != Type::Boolean {
                return Err(TypeCheckError::TypeMismatch(Type::Boolean, t));
            }

            let (if_stmt_typed, if_t) = type_check_stmt(if_stmt, env)?;
            env.stack_trace.pop();
            let (else_stmt_typed, else_t) = type_check_stmt(else_stmt, env)?;
            env.stack_trace.pop();
            env.pop_scope();

            let t = type_check_branch_types(if_t, else_t)?;

            return Ok((
                TypedStatement::IfElse(
                    expr_typed,
                    Box::new(if_stmt_typed),
                    Box::new(else_stmt_typed),
                ),
                t,
            ));
        }
        Statement::Return(expr_opt) => {
            let (typed_expr, t) = match expr_opt {
                Some(expr) => {
                    let e = type_check_expr(expr, env)?;
                    env.stack_trace.pop();
                    let t = e.get_type();
                    (Some(e), t)
                }
                None => (None, Type::Void),
            };
            return Ok((TypedStatement::Return(typed_expr), Some(t)));
        }
        Statement::Block(stmts) => {
            env.new_scope();
            let mut ret_type: Option<Type> = None;
            let typed_stmts = stmts
                .into_iter()
                .map(|stmt| {
                    let (s, t) = type_check_stmt(stmt, env)?;
                    env.stack_trace.pop();
                    ret_type = check_stmt_ret_types(ret_type.clone(), t)?;
                    Ok(s)
                })
                .collect::<TypeCheckResult<Vec<TypedStatement>>>()?;
            env.pop_scope();
            return Ok((TypedStatement::Block(typed_stmts), ret_type));
        }
    }
}

fn type_check_expr(expr: &Expression, env: &mut TypeCheckEnv) -> TypeCheckResult<TypedExpression> {
    env.stack_trace.push(expr.to_string());
    Ok(match expr {
        Expression::IntegerLiteral(n) => TypedExpression::IntegerLiteral(n.clone()),
        Expression::BooleanLiteral(b) => TypedExpression::BooleanLiteral(b.clone()),
        Expression::StringLiteral(s) => TypedExpression::StringLiteral(s.clone()),
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
                None => Err(TypeCheckError::NoSuchFunc(name.clone())),
            }?;

            let typed_args = type_check_func_args(&name, args, &arg_types, env)?;

            TypedExpression::FunctionCall(name.clone(), typed_args, ret_type)
        }
        Expression::Variable(name) => match env.lookup_var(name) {
            Some(t) => TypedExpression::Variable(name.clone(), t),
            None => return Err(TypeCheckError::NoSuchVar(name.clone())),
        },
        Expression::Comparison(a, b, op) => {
            let (a, b, _) = type_check_comparison(a, b, env)?;
            TypedExpression::Comparison(a, b, op.clone(), Type::Boolean)
        }
        Expression::Assignment(name, expr) => match env.lookup_var(name) {
            None => return Err(TypeCheckError::NoSuchVar(name.clone())),
            Some(t) => {
                let typed_expr = type_check_expr(expr, env)?;
                env.stack_trace.pop();
                if typed_expr.get_type() != t {
                    return Err(TypeCheckError::AssignmentMismatch(
                        typed_expr.get_type(),
                        name.clone(),
                        t,
                    ));
                }

                TypedExpression::Assignment(name.clone(), Box::new(typed_expr), t)
            }
        },
        Expression::NotUnaryOperation(expr) => {
            let typed_expr = type_check_expr(expr, env)?;
            if typed_expr.get_type() != Type::Boolean {
                return Err(TypeCheckError::TypeMismatch(
                    Type::Boolean,
                    typed_expr.get_type(),
                ));
            }
            TypedExpression::NotUnaryOperation(Box::new(typed_expr))
        }
        Expression::PreUnaryOperation(id, op) => {
            let var_type = env
                .lookup_var(id)
                .ok_or(TypeCheckError::NoSuchVar(id.clone()))?;
            if var_type != Type::Integer {
                return Err(TypeCheckError::TypeMismatch(Type::Integer, var_type));
            }
            TypedExpression::PreUnaryOperation(id.clone(), op.clone())
        }
        Expression::PostUnaryOperation(id, op) => {
            let var_type = env
                .lookup_var(id)
                .ok_or(TypeCheckError::NoSuchVar(id.clone()))?;
            if var_type != Type::Integer {
                return Err(TypeCheckError::TypeMismatch(Type::Integer, var_type));
            }
            TypedExpression::PostUnaryOperation(id.clone(), op.clone())
        }
    })
}

fn type_check_func_args(
    name: &Identifier,
    args: &Vec<Expression>,
    expected: &Vec<Type>,
    env: &mut TypeCheckEnv,
) -> TypeCheckResult<Vec<TypedExpression>> {
    if expected.len() != args.len() {
        return Err(TypeCheckError::ArgLenMismatch(
            name.clone(),
            args.len(),
            expected.len(),
        ));
    }

    env.stack_trace.push(name.clone());
    env.stack_trace
        .push(args.iter().fold(String::new(), |mut s, a| {
            s.push_str(&format!("{}, ", a.to_string()));
            s
        }));

    args.into_iter()
        .enumerate()
        .map(|(i, arg)| {
            let typed = type_check_expr(arg, env)?;
            env.stack_trace.pop();
            let expected_type = match expected.get(i) {
                None => {
                    return Err(TypeCheckError::ArgLenMismatch(
                        name.clone(),
                        args.len(),
                        expected.len(),
                    ))
                }
                Some(a) => a.clone(),
            };

            if typed.get_type() != expected_type {
                return Err(TypeCheckError::ArgTypeMismatch(
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
    env.stack_trace.pop();
    let typed_b = type_check_expr(&b, env)?;
    env.stack_trace.pop();

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

fn type_check_comparison(
    a: &Expression,
    b: &Expression,
    env: &mut TypeCheckEnv,
) -> TypeCheckResult<(Box<TypedExpression>, Box<TypedExpression>, Type)> {
    let typed_a = type_check_expr(&a, env)?;
    env.stack_trace.pop();
    let typed_b = type_check_expr(&b, env)?;
    env.stack_trace.pop();

    let type_a = typed_a.get_type();
    let type_b = typed_b.get_type();

    if type_a != type_b {
        return Err(TypeCheckError::CmpTypeMismatch(type_a, type_b));
    }

    Ok((Box::new(typed_a), Box::new(typed_b), type_a))
}

fn type_check_branch_types(t1: Option<Type>, t2: Option<Type>) -> TypeCheckResult<Option<Type>> {
    if t1 == t2 {
        Ok(t1)
    } else {
        return Err(TypeCheckError::BranchTypeDiff(
            match t1 {
                Some(t1) => t1.to_string(),
                None => "None".to_string(),
            },
            match t2 {
                Some(t2) => t2.to_string(),
                None => "None".to_string(),
            },
        ));
    }
}

fn check_stmt_ret_types(
    existing: Option<Type>,
    curr: Option<Type>,
) -> TypeCheckResult<Option<Type>> {
    if let Some(curr_type) = &curr {
        match &existing {
            None => {
                return Ok(curr);
            }
            Some(existing_type) => {
                if existing_type != curr_type {
                    return Err(TypeCheckError::TypeMismatch(
                        existing_type.clone(),
                        curr_type.clone(),
                    ));
                }
            }
        }
    }
    Ok(existing)
}
