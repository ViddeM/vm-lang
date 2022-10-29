use std::{collections::HashMap, fs, io};

use crate::types::{
    core_types::{Identifier, Type},
    value::Value,
};

#[derive(Debug, thiserror::Error)]
pub enum BuiltinError {
    #[error("Received an argument of invalid type to builtin function (should probably have been handled by typechecker?)")]
    InvalidArgType,
    #[error("Received the wrong number of arguments to builtin function (should probably have been handled by typechecker?)")]
    InvalidArgsCount,
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
        "read_file",
        "split_string",
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
    default_functions.insert(
        Identifier::from("read_file"),
        (vec![Type::String], Type::String),
    );
    default_functions.insert(
        Identifier::from("split_string"),
        (
            vec![Type::String, Type::String],
            Type::List(Box::new(Type::String)),
        ),
    );
    default_functions
}

pub fn execute_builtin<'a>(
    name: &Identifier,
    args: Vec<Value>,
    print_func: &'a mut dyn FnMut(String),
    read_func: &'a mut dyn FnMut() -> String,
) -> BuiltinResult<Value> {
    let mut args = args;
    args.reverse();
    match name.as_str() {
        "print_number" => {
            let i = get_int_arg(&mut args)?;
            (print_func)(format!("{}", i));
        }
        "print_string" => {
            let s = get_string_arg(&mut args)?;
            (print_func)(format!("{}", s));
        }
        "print_bool" => {
            let b = get_bool_arg(&mut args)?;
            (print_func)(format!("{}", b));
        }
        "read_number" => {
            let inp = read_func();
            return Ok(Value::Integer(inp.parse().or_else(|e| {
                Err(BuiltinError::Unknown(format!(
                    "Failed to parse '{}' as integer, err: {}",
                    inp, e
                )))
            })?));
        }
        "read_string" => {
            let inp = read_func();
            return Ok(Value::String(inp));
        }
        "read_bool" => {
            let inp = read_func();
            return Ok(Value::Boolean(inp.parse().or_else(|e| {
                Err(BuiltinError::Unknown(format!(
                    "Failed to parse '{}' as boolean, err: {}",
                    inp, e
                )))
            })?));
        }
        "read_file" => {
            let inp = get_string_arg(&mut args)?;
            let file = fs::read_to_string(&inp)?;
            return Ok(Value::String(file));
        }
        "split_string" => {
            let str = get_string_arg(&mut args)?;
            let split_on = get_string_arg(&mut args)?;
            println!("Split on '{}'", split_on);
            return Ok(Value::List(
                str.split(&split_on)
                    .map(|s| {
                        println!("Split part: {}", s);
                        Value::String(s.to_string())
                    })
                    .collect::<Vec<Value>>(),
            ));
        }
        _ => return Err(BuiltinError::NoSuchBuiltin(name.clone())),
    }
    Ok(Value::Void)
}

fn get_string_arg(args: &mut Vec<Value>) -> BuiltinResult<String> {
    let val = args.pop().ok_or(BuiltinError::InvalidArgsCount)?;
    match val {
        Value::String(s) => Ok(s),
        _ => Err(BuiltinError::InvalidArgType),
    }
}

fn get_bool_arg(args: &mut Vec<Value>) -> BuiltinResult<bool> {
    let val = args.pop().ok_or(BuiltinError::InvalidArgsCount)?;
    match val {
        Value::Boolean(b) => Ok(b),
        _ => Err(BuiltinError::InvalidArgType),
    }
}

fn get_int_arg(args: &mut Vec<Value>) -> BuiltinResult<i64> {
    let val = args.pop().ok_or(BuiltinError::InvalidArgsCount)?;
    match val {
        Value::Integer(i) => Ok(i),
        _ => Err(BuiltinError::InvalidArgType),
    }
}
