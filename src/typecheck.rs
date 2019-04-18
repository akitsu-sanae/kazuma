use crate::*;
use std::collections::HashMap;

pub fn check(module: &Module) -> Result<(), CodegenError> {
    let mut env = HashMap::new();

    for func in &module.funcs {
        let typ = Type::Func(
            func.args.iter().cloned().map(|(_, typ)| typ).collect(),
            box func.ret_type.clone());
        env.insert(func.name.to_string(), typ);
    }

    for func in &module.funcs {
        let mut env = env.clone();
        for (var, typ) in func.args.iter() {
            env.insert(var.clone(), typ.clone());
        }
        check_body(&func.body, &mut env)?;
    }
    Ok(())
}

fn check_body(statements: &Vec<Statement>, env: &HashMap<String, Type>) -> Result<Type, CodegenError> {
    use CodegenError::*;
    let mut env = env.clone();
    let mut rets = vec!();
    for statement in statements {
        if let Some(ret) = check_statement(statement, &mut env)? {
            rets.push(ret);
        }
    }
    rets.sort_unstable();
    rets.dedup();
    if rets.is_empty() {
        Err(TypeCheck(format!("not return")))
    } else if rets.len() == 1 {
        Ok(rets.pop().unwrap())
    } else {
        Err(TypeCheck(format!("multiple return types, {}", str_of_params(&rets))))
    }
}

fn check_statement(statement: &Statement, env: &mut HashMap<String, Type>) -> Result<Option<Type>, CodegenError> {
    use Statement::*;
    use CodegenError::*;
    match statement {
        Declare(ref name, ref typ, ref expr) => {
            check_expr(expr, env)?;
            env.insert(name.to_string(), typ.clone());
            Ok(None)
        },
        Assign(ref name, ref expr) => {
            let right_type = check_expr(expr, env)?;
            match env.get(name) {
                Some(ref typ) if **typ == right_type => Ok(None),
                Some(ref typ) => Err(TypeCheck(format!("unmatch types, {} vs {}", typ, right_type))),
                None => Err(TypeCheck(format!("unbound variable {}", name))),
            }
        },
        Return(ref expr) => Ok(Some(check_expr(expr, env)?)),
        ReturnVoid => Ok(Some(Type::Void)),
        Expr(ref expr) => {
            check_expr(expr, env)?;
            Ok(None)
        }
    }
}

fn check_expr(expr: &Expr, env: &HashMap<String, Type>) -> Result<Type, CodegenError> {
    use Expr::*;
    match expr {
        Var(ref name) =>
            env.get(name).cloned().ok_or(CodegenError::TypeCheck(format!("unbound variable {}", name))),
        BinOp(op, box ref lhs, box ref rhs) => {
            let lhs = check_expr(lhs, env)?;
            let rhs = check_expr(rhs, env)?;
            if lhs == rhs {
                use BinOp::*;
                match op {
                    Add | Sub | Mult | Div => Ok(lhs),
                    Eq | Neq | Gt | Geq | Lt | Leq => Ok(Type::Bool)
                }
            } else {
                Err(CodegenError::TypeCheck(format!("unmatch {} vs {}", lhs, rhs)))
            }
        }
        If(box ref cond, box ref then, box ref else_) => {
            let cond = check_expr(cond, env)?;
            let then = check_expr(then, env)?;
            let else_ = check_expr(else_, env)?;
            if Type::Bool != cond {
                Err(CodegenError::TypeCheck(format!("condition must be bool, but {}", cond)))
            } else if then != else_ {
                Err(CodegenError::TypeCheck(format!("condition branches must have same type. but {} vs {}", then, else_)))
            } else {
                Ok(then)
            }
        },
        Call(_, _) => unimplemented!(),
        Literal(ref lit) => Ok(type_of_lit(lit)),
        _ => unimplemented!(),
    }
}

fn type_of_lit(lit: &Literal) -> Type {
    use Literal::*;
    match lit {
        Bool(_) => Type::Bool,
        Char(_) => Type::Char,
        Int(_) => Type::Int,
        Array(_, _) => unimplemented!(),
    }
}

