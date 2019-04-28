
use super::*;
use llvm::core::*;

pub fn declare_global(name: &CString, typ: LType, value: LValue, module: LModule) -> LValue {
    unsafe {
        let var = LLVMAddGlobal(module, typ, name.as_ptr());
        LLVMSetInitializer(var, value);
        var
    }
}

pub fn declare(name: &str, typ: LType, init: LValue, builder: LBuilder) -> LValue {
    let name = cstring(name);
     unsafe {
        let var = LLVMBuildAlloca(builder, typ, name.as_ptr());
        self::store(var, init, builder);
        var
     }
}

pub fn declare_array(name: &str, typ: LType, init: LValue, base: &Base) -> LValue {
    let name = cstring(name);
    let var_ptr_name = fresh_name(NameType::Var, "var_ptr");
    let init_ptr_name = fresh_name(NameType::Var, "init_ptr");
    let memcpy_name = cstring("memcpy");
    unsafe {
        let var = LLVMBuildAlloca(base.builder, typ, name.as_ptr());

        let var_ptr = LLVMBuildBitCast(base.builder, var, typ::char_ptr(base.context), var_ptr_name.as_ptr());
        let init_ptr = LLVMBuildBitCast(base.builder, init, typ::char_ptr(base.context), init_ptr_name.as_ptr());
        let len = typ::size_of(typ);

        let memcpy = LLVMGetNamedFunction(base.module, memcpy_name.as_ptr());
        let mut args = vec!(var_ptr, init_ptr, len);
        LLVMBuildCall(base.builder, memcpy, args.as_mut_ptr(), args.len() as libc::c_uint, b"\0".as_ptr() as *const _);

        var
    }
}

pub fn declare_struct(name: &str, typ: LType, init: LValue, base: &Base) -> LValue {
    let name = cstring(name);
    let var_ptr_name = fresh_name(NameType::Var, "var_ptr");
    let init_ptr_name = fresh_name(NameType::Var, "init_ptr");
    let memcpy_name = cstring("memcpy");
    unsafe {
        let var = LLVMBuildAlloca(base.builder, typ, name.as_ptr());

        let var_ptr = LLVMBuildBitCast(base.builder, var, typ::char_ptr(base.context), var_ptr_name.as_ptr());
        let init_ptr = LLVMBuildBitCast(base.builder, init, typ::char_ptr(base.context), init_ptr_name.as_ptr());
        let len = typ::size_of(typ);

        let memcpy = LLVMGetNamedFunction(base.module, memcpy_name.as_ptr());
        let mut args = vec!(var_ptr, init_ptr, len);
        LLVMBuildCall(base.builder, memcpy, args.as_mut_ptr(), args.len() as libc::c_uint, b"\0".as_ptr() as *const _);

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

pub fn gep(arr: LValue, idx: LValue, base: &Base) -> LValue {
    let mut indices = vec!(lit::int32(0, base.context), idx);
    let name = fresh_name(NameType::Var, "gep_ret");
    unsafe {
        LLVMBuildGEP(
            base.builder,
            arr,
            indices.as_mut_ptr(),
            indices.len() as libc::c_uint,
            name.as_ptr())
    }
}

pub mod buildin {

    use super::*;
    pub fn print_num(value: LValue, base: &Base) {
        unsafe {
            let printf_name = cstring("printf");
            let printf = LLVMGetNamedFunction(base.module, printf_name.as_ptr());

            let format = LLVMGetNamedGlobal(base.module, cstring(".buildin.format.num").as_ptr());
            let format_ptr_name = cstring("format_ptr");
            let format_ptr = LLVMBuildBitCast(base.builder, format, typ::char_ptr(base.context), format_ptr_name.as_ptr());
            let mut args = vec!(format_ptr, value);
            call(printf, &mut args, base.builder);
        }
    }

}

