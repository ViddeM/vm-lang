use crate::types::core_types::{
    Argument, BooleanComparisonOperator, ComparisonOperator, Identifier, Type, UnaryOperator,
};
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct TypedProgram {
    pub main_function: TypedFunction,
    pub functions: Vec<TypedFunction>,
}

impl Display for TypedProgram {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Program")
    }
}

#[derive(Debug, Clone)]
pub struct TypedFunction {
    pub name: Identifier,
    pub arguments: Vec<Argument>,
    pub statements: Vec<TypedStatement>,
    pub return_type: Type,
}

impl Display for TypedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Function {} : {}", self.name, self.return_type)
    }
}

#[derive(Debug, Clone)]
pub enum TypedStatement {
    Let(Identifier, TypedExpression),
    Expression(TypedExpression),
    If(TypedExpression, Box<TypedStatement>),
    IfElse(TypedExpression, Box<TypedStatement>, Box<TypedStatement>),
    Return(Option<TypedExpression>),
    While(TypedExpression, Box<TypedStatement>),
    Block(Vec<TypedStatement>),
}

impl Display for TypedStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Statement: {}",
            match self {
                TypedStatement::Let(a, b) => format!("let {} = {}", a, b),
                TypedStatement::Expression(e) => format!("expression {}", e),
                TypedStatement::If(a, _) => format!("if {}", a),
                TypedStatement::IfElse(a, _, _) => format!("if {} else", a),
                TypedStatement::Return(a) => format!(
                    "return {}",
                    match a {
                        Some(a) => a.to_string(),
                        None => String::from("void"),
                    }
                ),
                TypedStatement::While(a, _) => format!("while {}", a),

                TypedStatement::Block(_) => format!("block"),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub enum TypedExpression {
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    StringLiteral(String),
    ListLiteral(Vec<TypedExpression>, Type),
    Plus(Box<TypedExpression>, Box<TypedExpression>, Type),
    Minus(Box<TypedExpression>, Box<TypedExpression>, Type),
    Times(Box<TypedExpression>, Box<TypedExpression>, Type),
    Divide(Box<TypedExpression>, Box<TypedExpression>, Type),
    FunctionCall(Identifier, Vec<TypedExpression>, Type),
    ListIndex(Box<TypedExpression>, Box<TypedExpression>, Type),
    Variable(Identifier, Type),
    Comparison(
        Box<TypedExpression>,
        Box<TypedExpression>,
        ComparisonOperator,
        Type,
    ),
    BooleanComparison(
        Box<TypedExpression>,
        Box<TypedExpression>,
        BooleanComparisonOperator,
    ),
    Assignment(Identifier, Box<TypedExpression>, Type),
    NotUnaryOperation(Box<TypedExpression>),
    PreUnaryOperation(Identifier, UnaryOperator),
    PostUnaryOperation(Identifier, UnaryOperator),
}

impl Display for TypedExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Expression: {}",
            match self {
                TypedExpression::IntegerLiteral(i) => i.to_string(),
                TypedExpression::BooleanLiteral(b) => b.to_string(),
                TypedExpression::StringLiteral(s) => s.to_string(),
                TypedExpression::ListLiteral(list, t) => format!(
                    "{}[{}]",
                    t,
                    list.iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                TypedExpression::Plus(a, b, _) => format!("{} + {}", a, b),
                TypedExpression::Minus(a, b, _) => format!("{} - {}", a, b),
                TypedExpression::Times(a, b, _) => format!("{} * {}", a, b),
                TypedExpression::Divide(a, b, _) => format!("{} / {}", a, b),
                TypedExpression::FunctionCall(name, args, _) => format!(
                    "{} ({})",
                    name,
                    args.iter().fold(String::new(), |mut acc, i| {
                        acc.push_str(&format!("{}, ", i.to_string()));
                        acc
                    })
                ),
                TypedExpression::ListIndex(list, index, _) => format!("{}[{}]", list, index),
                TypedExpression::Variable(a, _) => format!("var {}", a),
                TypedExpression::Comparison(a, b, comp, _) => format!("{} {} {}", a, comp, b),
                TypedExpression::BooleanComparison(a, b, comp) => format!("{} {} {}", a, comp, b),
                TypedExpression::Assignment(a, b, _) => format!("{} = {}", a, b),
                TypedExpression::NotUnaryOperation(expr) => format!("! {}", expr),
                TypedExpression::PreUnaryOperation(exp, op) => format!("{} {}", op, exp),
                TypedExpression::PostUnaryOperation(exp, op) => format!("{} {}", exp, op),
            }
        )
    }
}

impl TypedExpression {
    pub fn get_type(&self) -> Type {
        match self {
            TypedExpression::IntegerLiteral(_) => Type::Integer,
            TypedExpression::BooleanLiteral(_) => Type::Boolean,
            TypedExpression::StringLiteral(_) => Type::String,
            TypedExpression::ListLiteral(_, t) => Type::List(Box::new(t.clone())),
            TypedExpression::Plus(_, _, t) => t.clone(),
            TypedExpression::Minus(_, _, t) => t.clone(),
            TypedExpression::Times(_, _, t) => t.clone(),
            TypedExpression::Divide(_, _, t) => t.clone(),
            TypedExpression::FunctionCall(_, _, t) => t.clone(),
            TypedExpression::ListIndex(_, _, t) => t.clone(),
            TypedExpression::Variable(_, t) => t.clone(),
            TypedExpression::Comparison(_, _, _, t) => t.clone(),
            TypedExpression::BooleanComparison(_, _, _) => Type::Boolean,
            TypedExpression::Assignment(_, _, t) => t.clone(),
            TypedExpression::NotUnaryOperation(_) => Type::Boolean,
            TypedExpression::PreUnaryOperation(_, _) => Type::Integer,
            TypedExpression::PostUnaryOperation(_, _) => Type::Integer,
        }
    }
}
