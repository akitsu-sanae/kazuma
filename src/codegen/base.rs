use super::*;

pub type LContext = llvm::prelude::LLVMContextRef;
pub type LModule = llvm::prelude::LLVMModuleRef;
pub type LBuilder = llvm::prelude::LLVMBuilderRef;
pub type LType = llvm::prelude::LLVMTypeRef;
pub type LValue = llvm::prelude::LLVMValueRef;
pub type LBasicBlock = llvm::prelude::LLVMBasicBlockRef;

pub struct Base {
    pub context: LContext,
    pub module: LModule,
    pub builder: LBuilder,
}

impl Base {
    pub fn new(module: &Module) -> Base {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let name = util::cstring(&module.name);
            let module = llvm::core::LLVMModuleCreateWithNameInContext(name.as_ptr(), context);
            let builder = llvm::core::LLVMCreateBuilderInContext(context);

            self::add_buildin(context, module);
            Base {
                context, module, builder
            }
        }
    }
}

impl Drop for Base {
    fn drop(&mut self) {
        use llvm::core::*;
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

fn add_buildin(context: LContext, module: LModule) {
    add_printf_function(context, module);
    add_num_format_str(context, module);
}

fn add_printf_function(context: LContext, module: LModule) {
    let name = CString::new("printf").unwrap();
    let typ = typ::variadic_func(
        &mut vec!(typ::char_ptr(context)),
        typ::int32(context));
    unsafe {
        llvm::core::LLVMAddFunction(module, name.as_ptr(), typ);
    }
}

fn add_num_format_str(context: LContext, module: LModule) {
    let num_format_str = CString::new(".buildin.format.num").unwrap();
    let init = lit::str("%d", context);
    unsafe {
        let global_var = llvm::core::LLVMAddGlobal(module, typ::type_of(init), num_format_str.as_ptr());
        llvm::core::LLVMSetInitializer(global_var, init);
    }
}

