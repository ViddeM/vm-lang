use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Boolean(bool),
    String(String),
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Integer(i) => format!("Integer({})", i),
                Value::Boolean(b) => format!("Boolean({})", b),
                Value::String(s) => format!("String({})", s),
                Value::Void => String::from("Void"),
            }
        )
    }
}
