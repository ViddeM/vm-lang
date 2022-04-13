use crate::core_types::{Identifier, Type};

#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub typed_stmts: Vec<TypedStatement>,
}

#[derive(Debug, Clone)]
pub enum TypedStatement {
    Let(Identifier, TypedExpression, Type),
    Expression(TypedExpression, Type),
}

#[derive(Debug, Clone)]
pub enum TypedExpression {
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    Plus(Box<TypedExpression>, Box<TypedExpression>, Type),
    Minus(Box<TypedExpression>, Box<TypedExpression>, Type),
    Times(Box<TypedExpression>, Box<TypedExpression>, Type),
    Divide(Box<TypedExpression>, Box<TypedExpression>, Type),
    FunctionCall(Identifier, Vec<TypedExpression>, Type),
    Variable(Identifier, Type),
}

impl TypedExpression {
    pub fn get_type(&self) -> Type {
        match self {
            TypedExpression::IntegerLiteral(_) => Type::Integer,
            TypedExpression::BooleanLiteral(_) => Type::Boolean,
            TypedExpression::Plus(_, _, t) => t.clone(),
            TypedExpression::Minus(_, _, t) => t.clone(),
            TypedExpression::Times(_, _, t) => t.clone(),
            TypedExpression::Divide(_, _, t) => t.clone(),
            TypedExpression::FunctionCall(_, _, t) => t.clone(),
            TypedExpression::Variable(_, t) => t.clone(),
        }
    }
}
