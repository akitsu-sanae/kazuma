use super::*;
use llvm::core::*;

pub fn bool(b: bool, context: LContext) -> LValue {
    unsafe {
        LLVMConstInt(LLVMInt1TypeInContext(context), b as u64, 0)
    }
}

pub fn char(c: char, context: LContext) -> LValue {
    unsafe {
        LLVMConstInt(typ::char(context), c as u64, 0)
    }
}

pub fn str(str: &str, context: LContext) -> LValue {
    let mut bytes: Vec<_> = str.bytes()
        .map(|c| char(c as char, context))
        .collect();
    unsafe {
        LLVMConstArray(
            typ::char(context),
            bytes.as_mut_ptr(), bytes.len() as libc::c_uint)
    }
}

pub fn int32(n: i32, context: LContext) -> LValue {
    unsafe {
        LLVMConstInt(typ::int32(context), n as u64, 0)
    }
}

