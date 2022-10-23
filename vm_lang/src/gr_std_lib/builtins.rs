use std::{collections::HashMap, io};

use crate::types::{
    core_types::{Identifier, Type},
    value::Value,
};

#[derive(Debug, thiserror::Error)]
pub enum BuiltinError {
    #[error("Received invalid arguments to builtin function (should probably have been handled by typechecker?)")]
    InvalidArgs,
    #[error("No builtin function with name `{0}` exists")]
    NoSuchBuiltin(Identifier),
    #[error("An IO error occured whilst executing builtin function, err: `{0}`")]
    IOError(#[from] io::Error),
    #[error("An unknown error occured whilst executing a builtin function, err: `{0}`")]
    Unknown(String),
}

type BuiltinResult<T> = Result<T, BuiltinError>;

pub fn default_function_definitions() -> Vec<&'static str> {
    vec![
        "print_number",
        "print_string",
        "print_bool",
        "read_number",
        "read_string",
        "read_bool",
    ]
}

pub fn default_function_definitions_typechecker() -> HashMap<String, (Vec<Type>, Type)> {
    let mut default_functions = HashMap::new();
    default_functions.insert(
        Identifier::from("print_number"),
        (vec![Type::Integer], Type::Void),
    );
    default_functions.insert(
        Identifier::from("print_string"),
        (vec![Type::String], Type::Void),
    );
    default_functions.insert(
        Identifier::from("print_bool"),
        (vec![Type::Boolean], Type::Void),
    );
    default_functions.insert(Identifier::from("read_number"), (vec![], Type::Integer));
    default_functions.insert(Identifier::from("read_string"), (vec![], Type::String));
    default_functions.insert(Identifier::from("read_bool"), (vec![], Type::Boolean));
    default_functions
}

pub fn execute_builtin<'a>(
    name: &Identifier,
    args: Vec<Value>,
    print_func: &'a mut dyn FnMut(String),
) -> BuiltinResult<Value> {
    match name.as_str() {
        "print_number" => {
            let arg_val = args.first().ok_or(BuiltinError::InvalidArgs)?;
            match arg_val {
                Value::Integer(a) => (print_func)(format!("{}", a)),
                _ => Err(BuiltinError::InvalidArgs)?,
            }
        }
        "print_string" => {
            let arg_val = args.first().ok_or(BuiltinError::InvalidArgs)?;
            match arg_val {
                Value::String(a) => (print_func)(format!("{}", a)),
                _ => Err(BuiltinError::InvalidArgs)?,
            }
        }
        "print_bool" => {
            let arg_val = args.first().ok_or(BuiltinError::InvalidArgs)?;
            match arg_val {
                Value::Boolean(a) => (print_func)(format!("{}", a)),
                _ => Err(BuiltinError::InvalidArgs)?,
            }
        }
        "read_number" => {
            let inp = read_input()?;
            return Ok(Value::Integer(
                inp.parse()
                    .or_else(|e| Err(BuiltinError::Unknown(format!("{}", e))))?,
            ));
        }
        "read_string" => {
            let inp = read_input()?;
            return Ok(Value::String(inp));
        }
        "read_bool" => {
            let inp = read_input()?;
            return Ok(Value::Boolean(
                inp.parse()
                    .or_else(|e| Err(BuiltinError::Unknown(format!("{}", e))))?,
            ));
        }
        _ => return Err(BuiltinError::NoSuchBuiltin(name.clone())),
    }
    Ok(Value::Void)
}

fn read_input() -> BuiltinResult<String> {
    let mut inp = String::new();
    io::stdin().read_line(&mut inp)?;
    Ok(match inp.strip_suffix("\n") {
        Some(s) => String::from(s),
        None => inp,
    })
}
