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

pub fn func(name: CString, module: LModule) -> LValue {
    unsafe {
        LLVMGetNamedFunction(module, name.as_ptr())
    }
}

pub fn array(mut elems: Vec<LValue>, typ: LType, module: LModule) -> LValue {
    let name = fresh_name(NameType::GlobalConst, "array");
    let arr_type = typ::array(typ, elems.len());
    unsafe {
        let arr = LLVMConstArray(typ, elems.as_mut_ptr(), elems.len() as u32);
        let global_var = LLVMAddGlobal(module, arr_type, name.as_ptr());
        LLVMSetInitializer(global_var, arr);
        LLVMSetGlobalConstant(global_var, 1);
        global_var
    }
}

