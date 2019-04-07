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

pub fn generate(module: Module) -> String {
    let mut base = Base::new(&module);
    apply_module(module, &mut base);
    unsafe {
        let ir = llvm::core::LLVMPrintModuleToString(base.module);
        let len = libc::strlen(ir);
        String::from_raw_parts(ir as *mut u8, len+1, len+1)
    }
}

fn apply_module(module: Module, base: &mut Base) {
    for func in module.funcs {
        apply_funcs(func, base);
    }
}

fn apply_typ(ty: &Type, context: LContext) -> LType {
    use Type::*;
    match ty {
        Bool => typ::bool(context),
        Char => typ::char(context),
        Int => typ::int32(context),
        String => typ::char_ptr(context),
    }
}

fn apply_funcs(func: Func, base: &mut Base) {
    unsafe {
    let mut param_types = func.args.iter()
        .map(|&(_, ref ty)| apply_typ(ty, base.context))
        .collect();
    let func_typ = typ::func(&mut param_types, apply_typ(&func.ret_typ, base.context));
    let func_ll = llvm::core::LLVMAddFunction(base.module, func.name.as_str().as_ptr() as *const _, func_typ);
    let mut env = HashMap::new();
    for (i, arg) in func.args.into_iter().enumerate() {
        let param = llvm::core::LLVMGetParam(func_ll, i as libc::c_uint);
        env.insert(arg.0.clone(), param);
        let param_name = CString::new(arg.0.as_str()).unwrap();
        llvm::core::LLVMSetValueName(param, param_name.as_ptr());
    }
    let block = llvm::core::LLVMAppendBasicBlockInContext(base.context, func_ll, b"entry\0".as_ptr() as *const _);
    llvm::core::LLVMPositionBuilderAtEnd(base.builder, block);
    for statement in func.body {
        apply_statement(statement, &env, base);
    }
    }
}

fn apply_statement(statement: Statement, env: &HashMap<String, LValue>, base: &mut Base) {
    use Statement::*;
    match statement {
        Declare(var, typ, init) => {
            let typ = apply_typ(&typ, base.context);
            let init = apply_expr(init, env, base);
            build::declare(&var, typ, init, &mut base.builder);
        },
        Assign(var, expr) => {
            let var = *env.get(&var).unwrap();
            let expr = apply_expr(expr, env, base);
            build::store(var, expr, &mut base.builder);
        },
        Return(expr) => {
            let expr = apply_expr(expr, env, base);
            build::ret(expr, &mut base.builder)
        }
        _ => unimplemented!(),
    }
}

fn apply_expr(expr: Expr, env: &HashMap<String, LValue>, base: &mut Base) -> LValue {
    use Expr::*;
    match expr {
        Var(name) => {
            build::load(&name, env, &mut base.builder).unwrap()
        },
        Literal(lit) => apply_literal(lit, base),
        BinOp(op, box lhs, box rhs) => apply_binop_expr(op, lhs, rhs, env, base),
        _ => unimplemented!(),
    }
}

fn apply_binop_expr(op: BinOp, lhs: Expr, rhs: Expr, env: &HashMap<String, LValue>, base: &mut Base) -> LValue {
    use BinOp::*;
    let lhs = apply_expr(lhs, env, base);
    let rhs = apply_expr(rhs, env, base);
    match op {
        Add => build::add(lhs, rhs, &mut base.builder),
        Sub => build::sub(lhs, rhs, &mut base.builder),
        Mult => build::mult(lhs, rhs, &mut base.builder),
        Div => build::div(lhs, rhs, &mut base.builder),
        _ => unimplemented!(),
    }
}

fn apply_literal(lit: Literal, base: &mut Base) -> LValue {
    use Literal::*;
    match lit {
        Int(n) => lit::int32(n, base.context),
        _ => unimplemented!(),
    }
}

