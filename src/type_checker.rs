/*============================================================================
  Copyright (C) 2017 akitsu sanae
  https://github.com/akitsu-sanae/kazuma
  Distributed under the Boost Software License, Version 1.0. (See accompanying
  file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
============================================================================*/

use ast;

pub fn check(module: &ast::Module) -> Result<(), String> {
    for func in module.functions.iter() {
        function(&func)?;
    }
    Ok(())
}

fn function(func: &ast::Function) -> Result<(), String> {
    for stmnt in func.body.iter() {
        use ast::Statement::*;
        match *stmnt {
            Return(ref expr) => { expression(expr)?; },
            ReturnVoid => (),
            Expression(ref expr) => { expression(expr)?; },
        }
    }
    Ok(())
}

fn expression(expr: &ast::Expression) -> Result<ast::Type, String> {
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
        Literal(ref lit) => literal(lit),
    }
}

fn literal(lit: &ast::Literal) -> Result<ast::Type, String> {
    use ast::Literal::*;
    Ok(match *lit {
        Int1(_) => ast::Type::Int1,
        Int8(_) => ast::Type::Int8,
        Int16(_) => ast::Type::Int16,
        Int32(_) => ast::Type::Int32,
        Int64(_) => ast::Type::Int64,

        UInt8(_) => ast::Type::UInt8,
        UInt16(_) => ast::Type::UInt16,
        UInt32(_) => ast::Type::UInt32,
        UInt64(_) => ast::Type::UInt64,

        Float(_) => ast::Type::Float,
        Double(_) => ast::Type::Double,
    })
}

