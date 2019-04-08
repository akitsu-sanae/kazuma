use llvm::core::*;
use std::sync::RwLock;

use super::*;

pub fn print_module(module: LModule) -> String {
    unsafe {
        let ir = LLVMPrintModuleToString(module);
        let len = libc::strlen(ir);
        String::from_raw_parts(ir as *mut u8, len+1, len+1)
    }
}

pub fn add_function(module: LModule, name: &str, typ: LType) -> LValue {
    unsafe {
        LLVMAddFunction(module, name.as_ptr() as *const _, typ)
    }
}

pub fn set_func_param(idx: usize, name: String, func: LValue, env: &mut HashMap<String, LValue>) {
    unsafe {
        let param = LLVMGetParam(func, idx as libc::c_uint);
        let name_c = CString::new(name.as_str()).unwrap();
        env.insert(name, param);
        LLVMSetValueName(param, name_c.as_ptr())
    }
}

pub fn add_entry_block(func: LValue, base: &Base) -> LBasicBlock {
    unsafe {
        let block = LLVMAppendBasicBlockInContext(base.context, func, b"entry\0".as_ptr() as *const _);
        LLVMPositionBuilderAtEnd(base.builder, block);
        block
    }
}

pub fn fresh_name() -> String {
    let mut name_n = NAME_NUMBER.write().unwrap();
    *name_n += 1;
    format!(".generated.name.{}", *name_n)
}

lazy_static! {
    static ref NAME_NUMBER: RwLock<i32> = RwLock::new(0);
}

