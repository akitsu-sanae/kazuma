#![feature(box_syntax)]
#![feature(box_patterns)]

#[macro_use]
extern crate lazy_static;

extern crate llvm_sys as llvm;
extern crate libc;

mod codegen;
mod test;

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub funcs: Vec<Func>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void, Bool, Char, Int, String,
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
    Assign(String, Expr),
    Return(Expr),
    ReturnVoid,
    Expr(Expr),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add, Sub, Mult, Div,
    Eq, Neq, Greater, Geq, Less, Leq,
    Seq,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(String),
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
}

pub fn gencode(module: Module) -> Result<String, codegen::CodegenError> {
    codegen::generate(module)
}

