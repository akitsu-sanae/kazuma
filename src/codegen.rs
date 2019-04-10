mod base;
mod typ;
mod lit;
mod build;
mod util;

use std::ffi::CString;
use std::collections::HashMap;
use std::error::Error;
use std::fmt;

use crate::*;

pub use self::base::*;
pub use self::typ::*;
pub use self::lit::*;
pub use self::build::*;
pub use self::util::*;

#[derive(Debug)]
pub enum CodegenError {
    ModuleBuilding(String),
    ModuleValidation(String),
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CodegenError::*;
        match self {
            ModuleBuilding(ref msg) => write!(f, "module building error: {}", msg),
            ModuleValidation(ref msg) => write!(f, "module validation error: {}", msg),
        }
    }
}

impl Error for CodegenError {
    fn description(&self) -> &str {
        "code generation error"
    }
}

pub fn generate(module: Module) -> Result<String, CodegenError> {
    let base = Base::new(&module);
    apply_module(module, &base)?;
    util::validate_module(base.module)?;
    util::print_module(base.module)
}

fn apply_module(module: Module, base: &Base) -> Result<(), CodegenError> {
    for func in module.funcs {
        apply_funcs(func, base)?;
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
    }
}

fn apply_funcs(func: Func, base: &Base) -> Result<(), CodegenError> {
    let mut param_types = func.args.iter()
        .map(|&(_, ref ty)| apply_type(ty, base.context))
        .collect();
    let func_typ = typ::func(&mut param_types, apply_type(&func.ret_type, base.context));
    let gen_func = util::add_function(base.module, &func.name, func_typ);
    let mut env = HashMap::new();
    for (i, arg) in func.args.into_iter().enumerate() {
        util::set_func_param(i, arg.0, gen_func, &mut env);
    }
    util::add_entry_block(gen_func, base);

    for statement in func.body {
        apply_statement(statement, &env, base)?;
    }
    Ok(())
}

fn apply_statement(statement: Statement, env: &HashMap<String, LValue>, base: &Base) -> Result<(), CodegenError> {
    use Statement::*;
    match statement {
        Declare(var, typ, init) => {
            let typ = apply_type(&typ, base.context);
            let init = apply_expr(init, env, base)?;
            build::declare(&var, typ, init, base.builder);
            Ok(())
        },
        Assign(var, expr) => {
            let var = *env.get(&var)
                .ok_or(CodegenError::ModuleBuilding(format!("unbound variable: {}", var)))?;
            let expr = apply_expr(expr, env, base)?;
            build::store(var, expr, base.builder);
            Ok(())
        },
        Return(expr) => {
            let expr = apply_expr(expr, env, base)?;
            build::ret(expr, base.builder);
            Ok(())
        }
        ReturnVoid => {
            build::ret_void(base.builder);
            Ok(())
        }
        _ => unimplemented!(),
    }
}

fn apply_expr(expr: Expr, env: &HashMap<String, LValue>, base: &Base) -> Result<LValue, CodegenError> {
    use Expr::*;
    match expr {
        Var(name) => {
            let var: LValue = *env.get(&name)
                .ok_or(CodegenError::ModuleBuilding(format!("unbound variable: {}", name)))?;
            Ok(build::load(var, base.builder))
        },
        Literal(lit) => apply_literal(lit, base),
        BinOp(op, box lhs, box rhs) => apply_binop_expr(op, lhs, rhs, env, base),
        _ => unimplemented!(),
    }
}

fn apply_binop_expr(op: BinOp, lhs: Expr, rhs: Expr, env: &HashMap<String, LValue>, base: &Base) -> Result<LValue, CodegenError> {
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

fn apply_literal(lit: Literal, base: &Base) -> Result<LValue, CodegenError> {
    use Literal::*;
    match lit {
        Int(n) => Ok(lit::int32(n, base.context)),
        _ => unimplemented!(),
    }
}

