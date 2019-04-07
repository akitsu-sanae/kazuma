use super::*;
use llvm::core::*;

pub fn bool(context: LContext) -> LType {
    unsafe {
        LLVMInt1TypeInContext(context)
    }
}
pub fn int8(context: LContext) -> LType {
    unsafe {
        LLVMInt8TypeInContext(context)
    }
}
pub fn char(context: LContext) -> LType {
    int8(context)
}
pub fn int32(context: LContext) -> LType {
    unsafe {
        LLVMInt32TypeInContext(context)
    }
}
pub fn char_ptr(context: LContext) -> LType {
    unsafe {
        LLVMPointerType(LLVMInt8TypeInContext(context), 0)
    }
}
pub fn func(from: &mut Vec<LType>, to: LType) -> LType {
    unsafe {
        LLVMFunctionType(to, from.as_mut_ptr(), from.len() as libc::c_uint, 0)
    }
}
pub fn variadic_func(from: &mut Vec<LType>, to: LType) -> LType {
    unsafe {
        LLVMFunctionType(to, from.as_mut_ptr(), from.len() as libc::c_uint, 1)
    }
}
pub fn type_of(v: LValue) -> LType {
    unsafe {
        LLVMTypeOf(v)
    }
}

