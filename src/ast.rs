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
    pub body: Expression,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    Add(Box<Expression>, Box<Expression>),
    Sub(Box<Expression>, Box<Expression>),
    Mult(Box<Expression>, Box<Expression>),
    Div(Box<Expression>, Box<Expression>),
    LiteralInt32(i32),
}

