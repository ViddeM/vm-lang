use std::fs;

mod core_types;
mod interpreter;
mod type_checker;
mod typed_types;

use crate::interpreter::InterpretError;
use crate::type_checker::TypeCheckError;
use crate::ProgramError::LalrpopError;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub core);

#[derive(Debug)]
pub enum ProgramError {
    FileRead(String),
    TypeCheck(TypeCheckError),
    InterpretError(InterpretError),
    LalrpopError(String),
}

impl From<TypeCheckError> for ProgramError {
    fn from(e: TypeCheckError) -> Self {
        Self::TypeCheck(e)
    }
}

impl From<InterpretError> for ProgramError {
    fn from(e: InterpretError) -> Self {
        Self::InterpretError(e)
    }
}

pub fn run_program(path: &str, print_func: Box<dyn Fn(String)>) -> Result<(), ProgramError> {
    let to_parse = fs::read_to_string(path).or(Err(ProgramError::FileRead(path.to_string())))?;
    let parsed = core::PrgrParser::new()
        .parse(&to_parse)
        .or_else(|e| Err(LalrpopError(e.to_string())))?;

    let type_checked = type_checker::type_check(parsed)?;

    interpreter::interpret(type_checked, print_func)?;

    Ok(())
}
