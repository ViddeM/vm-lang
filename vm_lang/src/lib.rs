use std::fs;

mod interpreter;
mod type_checker;

use crate::interpreter::InterpretError;
use crate::type_checker::TypeCheckError;
use crate::ProgramError::LalrpopError;
use lalrpop_util::lalrpop_mod;

pub mod gr_std_lib;
pub mod types;

lalrpop_mod!(pub core);

#[derive(Debug, thiserror::Error)]
pub enum ProgramError {
    #[error("Failed to read file: {0}")]
    FileRead(String),
    #[error("Typechecking failed: {0}")]
    TypeCheck(TypeCheckError),
    #[error("Interpretation failed: {0}")]
    InterpretError(InterpretError),
    #[error("Parsing failed failed: {0}")]
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

pub fn run_program(path: &str, print_func: &mut dyn FnMut(String)) -> Result<(), ProgramError> {
    let to_parse = fs::read_to_string(path).or(Err(ProgramError::FileRead(path.to_string())))?;
    let parsed = core::PrgrParser::new()
        .parse(&to_parse)
        .or_else(|e| Err(LalrpopError(e.to_string())))?;

    let type_checked = type_checker::type_check(parsed)?;

    interpreter::interpret(type_checked, print_func)?;

    Ok(())
}
