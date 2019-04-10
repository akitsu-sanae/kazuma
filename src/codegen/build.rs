
use super::*;
use llvm::{*, core::*};

pub fn declare(name: &str, typ: LType, init: LValue, builder: &mut LBuilder) -> LValue {
    unsafe {
        let var = LLVMBuildAlloca(*builder, typ, name.as_ptr() as *const _);
        LLVMBuildStore(*builder, init, var)
    }
}

pub fn store(var: LValue, expr: LValue, builder: &mut LBuilder) -> LValue {
    unsafe {
        LLVMBuildStore(*builder, expr, var)
    }
}

pub fn load(name: &String, env: &HashMap<String, LValue>, builder: &mut LBuilder) -> Result<LValue, ()> {
    let var = env.get(name).unwrap();
    let name = CString::new(name.as_str()).unwrap();
    unsafe {
        Ok(LLVMBuildLoad(*builder, *var, name.as_ptr()))
    }
}

pub fn ret(value: LValue, builder: &mut LBuilder) {
    unsafe {
        LLVMBuildRet(*builder, value);
    }
}

pub fn ret_void(builder: &mut LBuilder) {
    unsafe {
        LLVMBuildRetVoid(*builder);
    }
}

pub fn add(lhs: LValue, rhs: LValue, builder: &mut LBuilder) -> LValue {
    let result = fresh_name();
    unsafe {
        LLVMBuildAdd(*builder, lhs, rhs, result.as_ptr() as *const _)
    }
}

pub fn sub(lhs: LValue, rhs: LValue, builder: &mut LBuilder) -> LValue {
    let result = fresh_name();
    unsafe {
        LLVMBuildSub(*builder, lhs, rhs, result.as_ptr() as *const _)
    }
}

pub fn mult(lhs: LValue, rhs: LValue, builder: &mut LBuilder) -> LValue {
    let result = fresh_name();
    unsafe {
        LLVMBuildMul(*builder, lhs, rhs, result.as_ptr() as *const _)
    }
}

pub fn div(lhs: LValue, rhs: LValue, builder: &mut LBuilder) -> LValue {
    let result = fresh_name();
    unsafe {
        LLVMBuildSDiv(*builder, lhs, rhs, result.as_ptr() as *const _)
    }
}

pub fn compare_integer(lhs: LValue, rhs: LValue, op: LLVMIntPredicate, builder: &mut LBuilder) -> LValue {
    let name = fresh_name();
    unsafe {
        LLVMBuildICmp(*builder, op, lhs, rhs, name.as_str().as_ptr() as *const i8)
    }
}

