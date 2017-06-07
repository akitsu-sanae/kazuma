/*============================================================================
  Copyright (C) 2017 akitsu sanae
  https://github.com/akitsu-sanae/kazuma
  Distributed under the Boost Software License, Version 1.0. (See accompanying
  file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
============================================================================*/

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    // signed integers
    Int1(bool),
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),

    // unsigned integers
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),

    Float(f64),
    Double(f64),
}

