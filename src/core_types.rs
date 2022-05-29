use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program")
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub statements: Vec<Statement>,
    pub return_type: Type,
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "function {}: {}", self.name, self.return_type)
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: Identifier,
    pub t: Type,
}

impl Display for Argument {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.t)
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(Identifier, Expression),
    While(Expression, Box<Statement>),
    If(Expression, Box<Statement>),
    IfElse(Expression, Box<Statement>, Box<Statement>),
    Return(Option<Expression>),
    Expression(Expression),
    Block(Vec<Statement>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Statement: {}",
            match self {
                Statement::Let(a, b) => format!("let {} = {}", a, b),
                Statement::Expression(e) => format!("expression {}", e),
                Statement::If(a, _) => format!("if {}", a),
                Statement::IfElse(a, _, _) => format!("if {} else", a),
                Statement::Return(a) => format!(
                    "return {}",
                    match a {
                        Some(a) => a.to_string(),
                        None => String::from("Void"),
                    }
                ),
                Statement::While(a, _) => format!("while {}", a),
                Statement::Block(_) => format!("block"),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    Variable(Identifier),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    Comparison(Box<Expression>, Box<Expression>, ComparisonOperator),
    Assignment(Identifier, Box<Expression>),
    NotUnaryOperation(Box<Expression>),
    PreUnaryOperation(Identifier, UnaryOperator),
    PostUnaryOperation(Identifier, UnaryOperator),
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Expression: {}",
            match self {
                Expression::IntegerLiteral(i) => i.to_string(),
                Expression::BooleanLiteral(b) => b.to_string(),
                Expression::Plus(a, b) => format!("{} + {}", a, b),
                Expression::Minus(a, b) => format!("{} - {}", a, b),
                Expression::Times(a, b) => format!("{} * {}", a, b),
                Expression::Divide(a, b) => format!("{} / {}", a, b),
                Expression::FunctionCall(name, args) => format!(
                    "{} ({})",
                    name,
                    args.iter().fold(String::new(), |mut acc, i| {
                        acc.push_str(&format!("{}, ", i.to_string()));
                        acc
                    })
                ),
                Expression::Variable(a) => format!("var {}", a),
                Expression::Comparison(a, b, comp) => format!("{} {} {}", a, comp, b),
                Expression::Assignment(a, b) => format!("{} = {}", a, b),
                Expression::NotUnaryOperation(expr) => format!("! {}", expr),
                Expression::PreUnaryOperation(e, op) => format!("{} {}", op, e),
                Expression::PostUnaryOperation(e, op) => format!("{} {}", e, op),
            }
        )
    }
}

pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Integer,
    Boolean,
    Void,
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Integer => "Integer",
                Type::Boolean => "Boolean",
                Type::Void => "Void",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum ComparisonOperator {
    Equals,
    NotEquals,
    LessOrEqual,
    GreaterOrEqual,
    LessThan,
    GreaterThan,
}

impl Display for ComparisonOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ComparisonOperator::Equals => "==",
                ComparisonOperator::NotEquals => "!=",
                ComparisonOperator::LessOrEqual => "<=",
                ComparisonOperator::GreaterOrEqual => ">=",
                ComparisonOperator::LessThan => "<",
                ComparisonOperator::GreaterThan => ">",
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Inc,
    Dec,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperator::Inc => "++",
                UnaryOperator::Dec => "--",
            }
        )
    }
}
