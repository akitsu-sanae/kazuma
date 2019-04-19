
use super::*;
use llvm::core::*;

pub fn declare(name: &str, typ: LType, init: LValue, builder: LBuilder) -> LValue {
    let name = cstring(name);
     unsafe {
        let var = LLVMBuildAlloca(builder, typ, name.as_ptr());
        self::store(var, init, builder);
        var
     }
}

pub fn store(var: LValue, expr: LValue, builder: LBuilder) -> LValue {
    unsafe {
        LLVMBuildStore(builder, expr, var)
    }
}

pub fn load(var: LValue, builder: LBuilder) -> LValue {
    let name = fresh_name(NameType::Var, "local");
    unsafe {
        LLVMBuildLoad(builder, var, name.as_ptr())
    }
}

pub fn ret(value: LValue, builder: LBuilder) {
    unsafe {
        LLVMBuildRet(builder, value);
    }
}

pub fn ret_void(builder: LBuilder) {
    unsafe {
        LLVMBuildRetVoid(builder);
    }
}

pub fn add(lhs: LValue, rhs: LValue, builder: LBuilder) -> LValue {
    let result = fresh_name(NameType::Var, "add_ret");
    unsafe {
        LLVMBuildAdd(builder, lhs, rhs, result.as_ptr())
    }
}

pub fn sub(lhs: LValue, rhs: LValue, builder: LBuilder) -> LValue {
    let result = fresh_name(NameType::Var, "sub_ret");
    unsafe {
        LLVMBuildSub(builder, lhs, rhs, result.as_ptr())
    }
}

pub fn mult(lhs: LValue, rhs: LValue, builder: LBuilder) -> LValue {
    let result = fresh_name(NameType::Var, "mul_ret");
    unsafe {
        LLVMBuildMul(builder, lhs, rhs, result.as_ptr())
    }
}

pub fn div(lhs: LValue, rhs: LValue, builder: LBuilder) -> LValue {
    let result = fresh_name(NameType::Var, "div_ret");
    unsafe {
        LLVMBuildSDiv(builder, lhs, rhs, result.as_ptr() as *const _)
    }
}

pub fn branch(block: LBasicBlock, builder: LBuilder) {
    unsafe {
        LLVMBuildBr(builder, block);
    }
}

pub fn cond_branch(cond: LValue, then: LBasicBlock, else_: LBasicBlock, builder: LBuilder) -> LValue {
    unsafe {
        LLVMBuildCondBr(builder, cond, then, else_)
    }
}

pub fn phi(typ: LType, incoming: Vec<(LValue, LBasicBlock)>, builder: LBuilder) -> LValue {
    let len = incoming.len();
    let (mut values, mut blocks): (Vec<LValue>, Vec<LBasicBlock>) = incoming.into_iter().unzip();
    let name = fresh_name(NameType::Var, "phi_ret");
    unsafe {
        let phi = LLVMBuildPhi(builder, typ, name.as_ptr());
        LLVMAddIncoming(phi, values.as_mut_ptr(), blocks.as_mut_ptr(), len as libc::c_uint);
        phi
    }
}

pub fn call(func: LValue, args: &mut Vec<LValue>, builder: LBuilder) -> LValue {
    let len = args.len() as libc::c_uint;
    let name = fresh_name(NameType::Var, "cal_ret");
    unsafe {
        LLVMBuildCall(builder, func, args.as_mut_ptr(), len, name.as_ptr())
    }
}

