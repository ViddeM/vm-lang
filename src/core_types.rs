use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
}

pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Boolean,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Integer => "Integer",
                Type::Boolean => "Boolean",
            }
        )
    }
}
