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
pub enum Type {
    Boolean,
    Integer,
    String
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<(String, Type)>,
    pub return_type: Type,
    pub body: Expression,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add, Sub, Mult, Div,
    Equal, NotEqual, Greater, GreaterEqual, Less, LessEqual,
    Call,
    Sequent,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Variable(String),
    BinOp(BinaryOperator, Box<Expression>, Box<Expression>),
    Print(Box<Expression>),
    Let(String, Type, Box<Expression>),
    Assign(String, Box<Expression>),
    Literal(Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Integer(i32),
    String(String),
}

