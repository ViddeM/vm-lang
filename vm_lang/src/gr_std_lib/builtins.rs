use std::collections::HashMap;

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
}

pub fn default_function_definitions() -> Vec<&'static str> {
    vec!["print_number", "print_string", "print_bool"]
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
    default_functions
}

pub fn execute_builtin<'a>(
    name: &Identifier,
    args: Vec<Value>,
    print_func: &'a mut dyn FnMut(String),
) -> Result<Value, BuiltinError> {
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
        _ => return Err(BuiltinError::NoSuchBuiltin(name.clone())),
    }
    Ok(Value::Void)
}
