use std::collections::HashMap;

use typ::*;
use program::*;
use error::CodegenError::{self, *};

type Env = HashMap<String, Type>;
type StructEnv = HashMap<String, Vec<Type>>;

pub fn check(module: &Module) -> Result<(), CodegenError> {
    let mut env = Env::new();
    let mut struct_env = StructEnv::new();

    for struct_def in module.struct_types.clone() {
        struct_env.insert(struct_def.name, struct_def.fields);
    }

    for (name, (ref typ, ref expr)) in &module.global_var {
        if *typ == check_expr(expr, &env, &struct_env)? {
            env.insert(name.clone(), Type::Pointer(box typ.clone()));
        } else {
            return Err(TypeCheck(format!("in initializing global var: {:?} is not {:?}", expr, typ)));
        }
    }

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
        check_body(&func.body, &mut env, &struct_env)?;
    }
    Ok(())
}

fn check_body(statements: &Vec<Statement>, env: &Env, struct_env: &StructEnv) -> Result<Type, CodegenError> {
    let mut env = env.clone();
    let mut rets = vec!();
    for statement in statements {
        if let Some(ret) = check_statement(statement, &mut env, struct_env)? {
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

fn check_statement(statement: &Statement, env: &mut Env, struct_env: &StructEnv) -> Result<Option<Type>, CodegenError> {
    use program::Statement::*;
    match statement {
        Declare(ref name, ref typ, ref expr) => {
            let r_type = check_expr(expr, env, struct_env)?;
            if *typ == r_type {
                let typ = Type::Pointer(box r_type);
                env.insert(name.to_string(), typ);
                Ok(None)
            } else {
                Err(TypeCheck(format!("{} has type {}, but initialized by type {}", name, typ, r_type)))
            }
        },
        Assign(ref lhs, ref rhs) => {
            let l_type = check_expr(lhs, env, struct_env)?;
            let r_type = check_expr(rhs, env, struct_env)?;
            if l_type == Type::Pointer(box r_type.clone()) {
                Ok(None)
            } else {
                Err(TypeCheck(format!("can not assign {} into {}", r_type, l_type)))
            }
        },
        Return(ref expr) => Ok(Some(check_expr(expr, env, struct_env)?)),
        ReturnVoid => Ok(Some(Type::Void)),
        Expr(ref expr) => {
            check_expr(expr, env, struct_env)?;
            Ok(None)
        },
        PrintNum(expr) => {
            if Type::Int == check_expr(expr, env, struct_env)? {
                Ok(None)
            } else {
                Err(TypeCheck(format!("can not print non-integer, {:?}", expr)))
            }
        },
    }
}

fn check_expr(expr: &Expr, env: &Env, struct_env: &StructEnv) -> Result<Type, CodegenError> {
    use program::Expr::*;
    match expr {
        Var(ref name) =>
            env.get(name).cloned().ok_or(TypeCheck(format!("unbound variable {}", name))),
        Load(box expr) => {
            match check_expr(expr, env, struct_env)? {
                Type::Pointer(box typ) => Ok(typ),
                typ => Err(TypeCheck(format!("can not load from non-pointer type, that is {}", typ)))
            }
        }
        BinOp(op, box ref lhs, box ref rhs) => {
            let lhs = check_expr(lhs, env, struct_env)?;
            let rhs = check_expr(rhs, env, struct_env)?;
            use program::BinOp::*;
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
            let cond = check_expr(cond, env, struct_env)?;
            let then = check_expr(then, env, struct_env)?;
            let else_ = check_expr(else_, env, struct_env)?;
            if Type::Bool != cond {
                Err(TypeCheck(format!("condition must be bool, but {}", cond)))
            } else if then != else_ {
                Err(TypeCheck(format!("condition branches must have same type. but {} vs {}", then, else_)))
            } else {
                Ok(then)
            }
        },
        ArrayAt(box arr, box idx) => {
            let arr_type = check_expr(arr, env, struct_env)?;
            match arr_type {
                Type::Pointer(box Type::Array(inner_type, _)) => {
                    let idx_type = check_expr(idx, env, struct_env)?;
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
        StructAt(box expr, idx) => {
            if let Type::Pointer(box Type::StructVar(name)) = check_expr(expr, env, struct_env)? {
                let fields = struct_env.get(&name).ok_or(TypeCheck(format!("undefined struct: {}", name)))?;
                let typ = fields.iter().nth(*idx as usize).cloned().ok_or(TypeCheck(format!("invalid field index {} of {}", idx, name)))?;
                Ok(Type::Pointer(box typ))
            } else {
                Err(TypeCheck(format!("can not access non-struct field: {:?}", expr)))
            }
        },
        Call(func, args) => {
            let func_type = check_expr(func, env, struct_env)?;
            let args: Result<_, _> = args.iter().map(|arg| check_expr(arg, env, struct_env)).collect();
            let arg_types: Vec<Type> = args?;
            let func_type = if let Type::Pointer(box typ) = func_type { typ } else { unreachable!() };
            match func_type {
                Type::Func(param_types, box ret_type) => {
                    if arg_types == param_types {
                        Ok(ret_type)
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
        Literal(ref lit) => Ok(type_of_lit(lit, env, struct_env)?),
    }
}

fn type_of_lit(lit: &Literal, env: &Env, struct_env: &StructEnv) -> Result<Type, CodegenError> {
    use program::Literal::*;
    Ok(match lit {
        Bool(_) => Type::Bool,
        Char(_) => Type::Char,
        Int(_) => Type::Int,
        Func(name) => Type::Pointer(box env.get(name).cloned().ok_or(TypeCheck(format!("unknown function {}", name)))?),
        Array(elems, typ) => Type::Array(box typ.clone(), elems.len()),
        Struct(exprs, name) => {
            let fields = struct_env.get(name).ok_or(TypeCheck(format!("undefined struct: {}", name)))?;
            let expr_types: Result<Vec<_>, _> = exprs.iter().map(|expr| check_expr(expr, env, struct_env)).collect();
            let expr_types = expr_types?;
            if fields == &expr_types {
                Type::StructVar(name.clone())
            } else {
                return Err(TypeCheck(format!("not match field types: {:?} vs {:?}", fields, expr_types)))
            }
        }
    })
}

