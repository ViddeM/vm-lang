use std::collections::HashMap;

use crate::types::core_types::{Identifier, Type};

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
