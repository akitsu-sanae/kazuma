mod base;
mod typ;
mod lit;
mod build;
mod util;

use std::ffi::CString;
use std::collections::HashMap;

use crate::*;

pub use self::base::*;
pub use self::typ::*;
pub use self::lit::*;
pub use self::build::*;
pub use self::util::*;

#[derive(Debug, Clone, Copy)]
enum Value {
    Var(LValue),
    Func,
}

type Env = HashMap<String, Value>;

pub fn generate(module: Module) -> Result<String, CodegenError> {
    i_know_what_i_do::clear_name_counter();
    let base = Base::new(&module);
    apply_module(module, &base)?;
    util::validate_module(base.module)?;
    util::print_module(base.module)
}

fn apply_module(module: Module, base: &Base) -> Result<(), CodegenError> {
    let mut env = Env::new();
    for func in module.funcs.iter() {
        env.insert(func.name.clone(), Value::Func);
    }
    for func in module.funcs {
        apply_func(func, &env, base)?;
    }
    Ok(())
}

fn apply_type(typ: &Type, context: LContext) -> LType {
    use Type::*;
    match typ {
        Void => typ::void(context),
        Bool => typ::bool(context),
        Char => typ::char(context),
        Int => typ::int32(context),
        String => typ::char_ptr(context),
        Func(from, box to) => {
            let mut from = from.iter().map(|typ| apply_type(typ, context)).collect();
            let to = apply_type(to, context);
            typ::func(&mut from, to)
        }
    }
}

fn apply_func(func: Func, env: &Env, base: &Base) -> Result<(), CodegenError> {
    let mut param_types = func.args.iter()
        .map(|&(_, ref ty)| apply_type(ty, base.context))
        .collect();
    let func_typ = typ::func(&mut param_types, apply_type(&func.ret_type, base.context));
    let gen_func = util::add_function(base.module, &func.name, func_typ);
    util::add_entry_block(gen_func, base);
    let mut env = env.clone();
    for (i, arg) in func.args.into_iter().enumerate() {
        let typ = param_types[i];
        let var = build::declare(&arg.0, typ, util::get_func_param(gen_func, i), base.builder);
        env.insert(arg.0, Value::Var(var));
    }

    for statement in func.body {
        apply_statement(statement, &mut env, base)?;
    }
    Ok(())
}

fn apply_statement(statement: Statement, env: &mut Env, base: &Base) -> Result<(), CodegenError> {
    use Statement::*;
    match statement {
        Declare(name, typ, init) => {
            let typ = apply_type(&typ, base.context);
            let init = apply_expr(init, env, base)?;
            let var = build::declare(&name, typ, init, base.builder);
            env.insert(name, Value::Var(var));
            Ok(())
        },
        Assign(name, expr) => {
            let var = env.get(&name)
                .ok_or(CodegenError::ModuleBuilding(format!("unbound variable: {}", name)))?;
            match var {
                Value::Func => Err(CodegenError::ModuleBuilding(format!("can not assign value to function {}", name))),
                Value::Var(var) => {
                    let expr = apply_expr(expr, env, base)?;
                    build::store(*var, expr, base.builder);
                    Ok(())
                }
            }
        },
        Return(expr) => {
            let expr = apply_expr(expr, env, base)?;
            build::ret(expr, base.builder);
            Ok(())
        }
        ReturnVoid => {
            build::ret_void(base.builder);
            Ok(())
        },
        Expr(expr) => {
            apply_expr(expr, env, base)?;
            Ok(())
        },
    }
}

fn apply_expr(expr: Expr, env: &Env, base: &Base) -> Result<LValue, CodegenError> {
    use Expr::*;
    match expr {
        Var(name) => {
            let var = env.get(&name)
                .ok_or(CodegenError::ModuleBuilding(format!("unbound variable: {}", name)))?;
            match var {
                Value::Var(var) =>
                    Ok(build::load(*var, base.builder)),
                Value::Func =>
                    Ok(util::get_func_by_name(cstring(&name), base.module)),
            }
        },
        Literal(lit) => apply_literal(lit, base),
        BinOp(op, box lhs, box rhs) => apply_binop_expr(op, lhs, rhs, env, base),
        If(box cond, box then, box else_) => apply_if_expr(cond, then, else_, env, base),
        Call(box func, args) => {
            let func = apply_expr(func, env, base)?;
            let args: Result<Vec<LValue>, _> = args.into_iter().map(|arg| apply_expr(arg, env, base)).collect();
            let mut args = args?;
            Ok(build::call(func, &mut args, base.builder))
        },
        _ => unimplemented!(),
    }
}

fn apply_binop_expr(op: BinOp, lhs: Expr, rhs: Expr, env: &Env, base: &Base) -> Result<LValue, CodegenError> {
    use BinOp::*;
    let lhs = apply_expr(lhs, env, base)?;
    let rhs = apply_expr(rhs, env, base)?;
    match op {
        Add => Ok(build::add(lhs, rhs, base.builder)),
        Sub => Ok(build::sub(lhs, rhs, base.builder)),
        Mult => Ok(build::mult(lhs, rhs, base.builder)),
        Div => Ok(build::div(lhs, rhs, base.builder)),
        _ => unimplemented!(),
    }
}

fn apply_if_expr(cond: Expr, then: Expr, else_: Expr, env: &Env, base: &Base) -> Result<LValue, CodegenError> {
    let cond = apply_expr(cond, env, base)?;
    let insertion_block = util::insertion_block(base.builder);
    let then_block = append_block("if_then", insertion_block, base);
    let else_block = append_block("if_else", then_block, base);
    let merge_block = append_block("if_merge", else_block, base);

    build::cond_branch(cond, then_block, else_block, base.builder);

    // code generation for then-block
    util::position_at_end(then_block, base.builder);
    let then = apply_expr(then, env, base)?;
    build::branch(merge_block, base.builder);
    let then_block = util::insertion_block(base.builder);

    // code generation for else-block
    util::position_at_end(else_block, base.builder);
    let else_ = apply_expr(else_, env, base)?;
    build::branch(merge_block, base.builder);
    let else_block = util::insertion_block(base.builder);

    // code generation for merge-block
    util::position_at_end(merge_block, base.builder);
    Ok(build::phi(typ::type_of(then), vec!((then, then_block), (else_, else_block)), base.builder))
}

fn apply_literal(lit: Literal, base: &Base) -> Result<LValue, CodegenError> {
    use Literal::*;
    match lit {
        Bool(b) => Ok(lit::bool(b, base.context)),
        Int(n) => Ok(lit::int32(n, base.context)),
        _ => unimplemented!(),
    }
}

