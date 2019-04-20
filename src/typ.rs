use crate::*;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Void, Bool, Char, Int, String,
    Func(Vec<Type>, Box<Type>),
    Array(Box<Type>, usize),
    Pointer(Box<Type>),
}

impl Type {
    pub fn is_integer(&self) -> bool {
        use self::Type::*;
        match self {
            Int => true,
            _ => false
        }
    }
}

pub fn str_of_params(params: &[Type]) -> String {
    match params {
        [] => "()".to_string(),
        [head, tail..] =>
            tail.into_iter().fold(format!("{}", head), |acc, typ| format!("{}, {}", acc, typ))
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Type::*;
        match self {
            Void => write!(f, "void"),
            Bool => write!(f, "bool"),
            Char => write!(f, "char"),
            Int => write!(f, "int"),
            String => write!(f, "string"),
            Func(from, to) => write!(f, "{} -> {}", str_of_params(from), to),
            Array(box ref typ, n) => write!(f, "array[{}; {}]", typ, n),
            Pointer(box ref typ) => write!(f, "ptr[{}]", typ),
        }
    }
}

