/*============================================================================
  Copyright (C) 2017 akitsu sanae
  https://github.com/akitsu-sanae/kazuma
  Distributed under the Boost Software License, Version 1.0. (See accompanying
  file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
============================================================================*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub arg_names: Vec<String>,
    pub typ : Type,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Void,
    Bool,
    Int8, Int16, Int32, Int64,
    Float32, Float64,
    Pointer(Box<Type>),
    Array(Box<Type>, i32),
    Function(Vec<Type>, Box<Type>),
    Struct(String)
}

impl Type {
    pub fn is_int_type(&self) -> bool {
        use self::Type::*;
        match *self {
            Int8 | Int16 | Int32 | Int64 => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Let(String, Box<Expression>),
    Variable(String),
    Assign(Box<Expression>, Box<Expression>),

    Add(Box<Expression>, Box<Expression>), Sub(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>), Div(Box<Expression>, Box<Expression>),
    Equal(Box<Expression>, Box<Expression>), NotEqual(Box<Expression>, Box<Expression>),
    Greater(Box<Expression>, Box<Expression>), GreaterEq(Box<Expression>, Box<Expression>),
    Less(Box<Expression>, Box<Expression>), LessEq(Box<Expression>, Box<Expression>),

    Call(Box<Expression>, Vec<Expression>),

    Block(Vec<Expression>),

    Bool(bool),
    Int(i64),
}


