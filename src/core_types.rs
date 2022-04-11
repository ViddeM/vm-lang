#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Let(Identifier, Expression),
}

#[derive(Debug)]
pub enum Expression {
    NumberLiteral(i64),
    BooleanLiteral(bool),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    Parenth(Box<Expression>),
}

pub type Identifier = String;
