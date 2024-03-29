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
    For(Identifier, Expression, Box<Statement>),
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
                Statement::For(id, expr, _) => format!("for ({} in {})", id, expr),
                Statement::Block(_) => format!("block"),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
    ListLiteral(Vec<Expression>),
    TypedListLiteral(Vec<Expression>, Type),
    Variable(Identifier),
    Plus(Box<Expression>, Box<Expression>),
    Minus(Box<Expression>, Box<Expression>),
    Times(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    FunctionCall(Identifier, Vec<Expression>),
    ListIndex(Box<Expression>, Box<Expression>),
    Comparison(Box<Expression>, Box<Expression>, ComparisonOperator),
    BooleanComparison(Box<Expression>, Box<Expression>, BooleanComparisonOperator),
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
                Expression::StringLiteral(s) => s.to_string(),
                Expression::ListLiteral(inner) => format!(
                    "[{}]",
                    inner
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Expression::TypedListLiteral(inner, t) => format!(
                    "{}[{}]",
                    t,
                    inner
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Expression::Plus(a, b) => format!("{} + {}", a, b),
                Expression::Minus(a, b) => format!("{} - {}", a, b),
                Expression::Times(a, b) => format!("{} * {}", a, b),
                Expression::Divide(a, b) => format!("{} / {}", a, b),
                Expression::FunctionCall(name, args) => format!(
                    "{} ({})",
                    name,
                    args.iter()
                        .map(|e| e.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Expression::ListIndex(list, index) => format!("{}[{}]", list, index),
                Expression::Variable(a) => format!("var {}", a),
                Expression::Comparison(a, b, comp) => format!("{} {} {}", a, comp, b),
                Expression::BooleanComparison(a, b, comp) => format!("{} {} {}", a, comp, b),
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
    String,
    Void,
    List(Box<Type>),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Integer => "Integer".to_string(),
                Type::Boolean => "Boolean".to_string(),
                Type::Void => "Void".to_string(),
                Type::String => "String".to_string(),
                Type::List(a) => format!("List<{}>", a),
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

#[derive(Debug, Clone)]
pub enum BooleanComparisonOperator {
    And,
    Or,
}

impl Display for BooleanComparisonOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BooleanComparisonOperator::And => "&&",
                BooleanComparisonOperator::Or => "||",
            }
        )
    }
}
