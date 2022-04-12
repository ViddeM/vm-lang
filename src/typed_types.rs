use crate::core_types::{Expression, Identifier, Type};

#[derive(Debug)]
pub struct TypedProgram {
    pub typed_stmts: Vec<TypedStatement>,
}

#[derive(Debug)]
pub enum TypedStatement {
    Let(Identifier, TypedExpression),
}

#[derive(Debug)]
pub struct TypedExpression {
    expr: Expression,
    t: Type
}