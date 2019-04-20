#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(slice_patterns)]

#[macro_use]
extern crate lazy_static;

extern crate llvm_sys as llvm;
extern crate libc;

mod codegen;
mod typecheck;
mod test;

use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub funcs: Vec<Func>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Void, Bool, Char, Int, String,
    Func(Vec<Type>, Box<Type>),
    Pointer(Box<Type>),
}

fn str_of_params(params: &[Type]) -> String {
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
            Pointer(box ref typ) => write!(f, "ptr<{}>", typ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: String,
    pub args: Vec<(String, Type)>,
    pub ret_type: Type,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Declare(String, Type, Expr),
    Assign(Expr, Expr),
    Return(Expr),
    ReturnVoid,
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add, Sub, Mult, Div,
    Eq, Neq, Gt, Geq, Lt, Leq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Load(Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Literal(Literal),

    // for debug
    Print(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Char(char),
    Int(i32),
    Array(Vec<Literal>, Type),
    Func(String),
}

use std::error::Error;

#[derive(Debug)]
pub enum CodegenError {
    TypeCheck(String),
    ModuleBuilding(String),
    ModuleValidation(String),
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CodegenError::*;
        match self {
            TypeCheck(ref msg) => write!(f, "type error: {}", msg),
            ModuleBuilding(ref msg) => write!(f, "module building error: {}", msg),
            ModuleValidation(ref msg) => write!(f, "module validation error: {}", msg),
        }
    }
}

impl Error for CodegenError {
    fn description(&self) -> &str {
        "code generation error"
    }
}


pub fn generate(module: Module) -> Result<String, CodegenError> {
    typecheck::check(&module)?;
    codegen::generate(module)
}

