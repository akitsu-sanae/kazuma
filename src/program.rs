use std::collections::HashMap;
use typ::{StructDef, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub struct_types: Vec<StructDef>,
    pub global_var: HashMap<String, (Type, Expr)>,
    pub funcs: Vec<Func>,
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

    // for debug
    PrintNum(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mult,
    Div,
    Eq,
    Neq,
    Gt,
    Geq,
    Lt,
    Leq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
    Load(Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    ArrayAt(Box<Expr>, Box<Expr>),
    StructAt(Box<Expr>, i32),
    Call(Box<Expr>, Vec<Expr>),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Bool(bool),
    Char(char),
    Int(i32),
    Array(Vec<Expr>, Type),
    Struct(Vec<Expr>, String),
    Func(String),
}
