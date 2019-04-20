use crate::*;
use std::collections::HashMap;

use CodegenError::*;

type Env = HashMap<String, Type>;

pub fn check(module: &Module) -> Result<(), CodegenError> {
    let mut env = Env::new();

    for func in &module.funcs {
        let typ = Type::Func(
            func.args.iter().cloned().map(|(_, typ)| typ).collect(),
            box func.ret_type.clone());
        env.insert(func.name.to_string(), typ);
    }

    for func in &module.funcs {
        let mut env = env.clone();
        for (var, typ) in func.args.iter() {
            env.insert(var.clone(), Type::Pointer(box typ.clone()));
        }
        check_body(&func.body, &mut env)?;
    }
    Ok(())
}

fn check_body(statements: &Vec<Statement>, env: &Env) -> Result<Type, CodegenError> {
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

fn check_statement(statement: &Statement, env: &mut Env) -> Result<Option<Type>, CodegenError> {
    use Statement::*;
    match statement {
        Declare(ref name, ref typ, ref expr) => {
            let r_type = check_expr(expr, env)?;
            if *typ == r_type {
                let typ = Type::Pointer(box r_type);
                env.insert(name.to_string(), typ);
                Ok(None)
            } else {
                Err(TypeCheck(format!("{} has type {}, but initialized by type {}", name, typ, r_type)))
            }
        },
        Assign(ref lhs, ref rhs) => {
            let l_type = check_expr(lhs, env)?;
            let r_type = check_expr(rhs, env)?;
            if l_type == Type::Pointer(box r_type.clone()) {
                Ok(None)
            } else {
                Err(TypeCheck(format!("can not assign {} into {}", r_type, l_type)))
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

fn check_expr(expr: &Expr, env: &Env) -> Result<Type, CodegenError> {
    use Expr::*;
    match expr {
        Var(ref name) =>
            env.get(name).cloned().ok_or(TypeCheck(format!("unbound variable {}", name))),
        Load(box expr) => {
            match check_expr(expr, env)? {
                Type::Pointer(box typ) => Ok(typ),
                typ => Err(TypeCheck(format!("can not load from non-pointer type, that is {}", typ)))
            }
        }
        BinOp(op, box ref lhs, box ref rhs) => {
            let lhs = check_expr(lhs, env)?;
            let rhs = check_expr(rhs, env)?;
            use BinOp::*;
            match (op, &lhs, &rhs) {
                (Add, Type::Int, Type::Int) |
                (Sub, Type::Int, Type::Int) |
                (Mult, Type::Int, Type::Int) |
                (Div, Type::Int, Type::Int)  => Ok(Type::Int),

                (Eq, lhs, rhs) |
                (Neq, lhs, rhs) if lhs == rhs => Ok(Type::Bool),

                (Gt, Type::Int, Type::Int) |
                (Geq, Type::Int, Type::Int) |
                (Lt, Type::Int, Type::Int) |
                (Leq, Type::Int, Type::Int) => Ok(Type::Bool),

                _ => Err(TypeCheck(format!("unmatch {} vs {}", lhs, rhs)))
            }
        }
        If(box ref cond, box ref then, box ref else_) => {
            let cond = check_expr(cond, env)?;
            let then = check_expr(then, env)?;
            let else_ = check_expr(else_, env)?;
            if Type::Bool != cond {
                Err(TypeCheck(format!("condition must be bool, but {}", cond)))
            } else if then != else_ {
                Err(TypeCheck(format!("condition branches must have same type. but {} vs {}", then, else_)))
            } else {
                Ok(then)
            }
        },
        ArrayAt(box arr, box idx) => {
            let arr_type = check_expr(arr, env)?;
            match arr_type {
                Type::Pointer(box Type::Array(inner_type, _)) => {
                    let idx_type = check_expr(idx, env)?;
                    if idx_type.is_integer() {
                        // TODO: check idx is valid when idx is constant
                        Ok(Type::Pointer(inner_type))
                    } else {
                        Err(TypeCheck(format!("can not use `[]` operator with a non-integer parameter, that is {}", idx_type)))
                    }
                },
                typ => Err(TypeCheck(format!("can not use `[]` operator for non-array type, that is {}", typ)))
            }
        },
        Call(func, args) => {
            let func_type = check_expr(func, env)?;
            let args: Result<_, _> = args.iter().map(|arg| check_expr(arg, env)).collect();
            let arg_types: Vec<Type> = args?;
            match func_type {
                Type::Func(param_types, box ret_types) => {
                    if arg_types == param_types {
                        Ok(ret_types)
                    } else {
                        Err(TypeCheck(format!(
                                    "mismatch param types: {} vs {}",
                                    str_of_params(&arg_types),
                                    str_of_params(&param_types))))
                    }
                },
                typ => Err(TypeCheck(format!("can not apply for non-functional type: {}", typ))),
            }
        },
        Literal(ref lit) => Ok(type_of_lit(lit, env)?),
        _ => unimplemented!(),
    }
}

fn type_of_lit(lit: &Literal, env: &Env) -> Result<Type, CodegenError> {
    use Literal::*;
    match lit {
        Bool(_) => Ok(Type::Bool),
        Char(_) => Ok(Type::Char),
        Int(_) => Ok(Type::Int),
        Func(name) => env.get(name).cloned().ok_or(TypeCheck(format!("unknown function {}", name))),
        Array(elems, typ) => Ok(Type::Array(box typ.clone(), elems.len())),
    }
}

