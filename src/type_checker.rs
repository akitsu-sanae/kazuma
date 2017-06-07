/*============================================================================
  Copyright (C) 2017 akitsu sanae
  https://github.com/akitsu-sanae/kazuma
  Distributed under the Boost Software License, Version 1.0. (See accompanying
  file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
============================================================================*/

use ast;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Type {
    Int32,
}

pub fn check(module: &ast::Module) -> Result<(), String> {
    for func in module.functions.iter() {
        function(&func)?;
    }
    Ok(())
}

fn function(func: &ast::Function) -> Result<(), String> {
    expression(&func.body).map(|_| ())
}

fn expression(expr: &ast::Expression) -> Result<Type, String> {
    use ast::Expression::*;
    match *expr {
        Add(box ref lhs, box ref rhs) |
        Sub(box ref lhs, box ref rhs) |
        Mult(box ref lhs, box ref rhs) |
        Div(box ref lhs, box ref rhs) => {
            let lhs = expression(lhs)?;
            let rhs = expression(rhs)?;
            if lhs == rhs {
                Ok(lhs)
            } else {
                Err(format!("invalid arithmetic operator for {:?} and {:?}", lhs, rhs))
            }
        },
        LiteralInt32(_) => Ok(Type::Int32),
    }
}

