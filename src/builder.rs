/*============================================================================
  Copyright (C) 2017 akitsu sanae
  https://github.com/akitsu-sanae/kazuma
  Distributed under the Boost Software License, Version 1.0. (See accompanying
  file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
============================================================================*/

extern crate llvm_sys as llvm;

use std::ffi::CString;
use std::collections::HashMap;

use super::ast;

type LContext = llvm::prelude::LLVMContextRef;
type LModule = llvm::prelude::LLVMModuleRef;
type LBuilder = llvm::prelude::LLVMBuilderRef;
type LType = llvm::prelude::LLVMTypeRef;
type LValue = llvm::prelude::LLVMValueRef;

pub struct Builder {
    context: LContext,
    module: LModule,
    builder: LBuilder,
}

type Env = HashMap<String, LValue>;

impl Builder {

    pub fn new(name: &str) -> Self {
        unsafe {
            let context = llvm::core::LLVMContextCreate();
            let module = llvm::core::LLVMModuleCreateWithName(name.as_ptr() as *const _);
            let builder = llvm::core::LLVMCreateBuilderInContext(context);
            Builder {
                context: context,
                module: module,
                builder: builder,
            }
        }
    }


    pub fn build(&self, module: &ast::Module) -> Result<String, String> {
        self.build_module(module)
    }

    fn build_module(&self, module: &ast::Module) -> Result<String, String> {
        for func in module.functions.iter() {
            self.build_function(func)?;
        }

        unsafe {
            CString::from_raw(llvm::core::LLVMPrintModuleToString(self.module)).into_string().map_err(|e| format!("{}", e))
        }
    }

    fn build_type(&self, typ: &ast::Type) -> Result<LType, String> {
        use ast::Type::*;
        unsafe {
            match *typ {
                Void => Ok(llvm::core::LLVMVoidTypeInContext(self.context)),
                Bool => Ok(llvm::core::LLVMInt1TypeInContext(self.context)),
                Int8 => Ok(llvm::core::LLVMInt8TypeInContext(self.context)),
                Int16 => Ok(llvm::core::LLVMInt16TypeInContext(self.context)),
                Int32 => Ok(llvm::core::LLVMInt32TypeInContext(self.context)),
                Int64 => Ok(llvm::core::LLVMInt64TypeInContext(self.context)),
                Float32 => Ok(llvm::core::LLVMFloatTypeInContext(self.context)),
                Float64 => Ok(llvm::core::LLVMDoubleTypeInContext(self.context)),
                Pointer(box ref inner) => Ok(llvm::core::LLVMPointerType(self.build_type(inner)?, 0)),
                Array(box ref inner, ref n) => Ok(llvm::core::LLVMArrayType(self.build_type(inner)?, *n as u32)),
                Function(ref args, box ref ret) => {
                    let args: Result<Vec<_>, String> = args.iter().map(|arg| self.build_type(arg)).collect();
                    let mut args = args?;
                    let ret = self.build_type(ret)?;
                    Ok(llvm::core::LLVMFunctionType(ret, args.as_mut_ptr(), args.len() as u32, 0))
                },
                Struct(ref name) => Ok(llvm::core::LLVMStructCreateNamed(self.context, name.as_str().as_ptr() as *const _))
            }
        }
    }

    fn build_function(&self, func: &ast::Function) -> Result<LValue, String> {
        unsafe {
            let func_name = func.name.as_str().as_ptr() as *const _;
            let func_type = self.build_type(&func.typ)?;
            let function = llvm::core::LLVMAddFunction(self.module, func_name, func_type);
            let block = llvm::core::LLVMAppendBasicBlockInContext(self.context, function, b"entry\0".as_ptr() as *const _);
            llvm::core::LLVMPositionBuilderAtEnd(self.builder, block);
            let ret_value = self.build_expression(&func.body, &mut Env::new())?;
            Ok(llvm::core::LLVMBuildRet(self.builder, ret_value))
        }
    }

    fn build_expression(&self, expr: &ast::Expression, env: &mut Env) -> Result<LValue, String> {
        use ast::Expression::*;
        let name = CString::new("tmp").unwrap();
        match *expr {
            Let(ref name, box ref init) => {
                let init = self.build_expression(init, env)?;
                env.insert(name.clone(), init);
                let name = name.as_ptr() as *const i8;
                unsafe {
                    let typ = llvm::core::LLVMTypeOf(init);
                    let stack = llvm::core::LLVMBuildAlloca(self.builder, typ, name);
                    Ok(llvm::core::LLVMBuildStore(self.builder, init, stack))
                }
            },
            Variable(ref name) => env.get(name).cloned().ok_or(format!("unbound variable: {}", name)),
            Assign(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildStore(self.builder, rhs, lhs))
                }
            },
            Add(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr()))
                }
            },
            Sub(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr()))
                }
            },
            Mult(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr()))
                }
            },
            Div(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    let type_name = CString::from_raw(llvm::core::LLVMPrintTypeToString(llvm::core::LLVMTypeOf(lhs))).into_string().unwrap();
                    match type_name.as_str() {
                        "double" | "float" =>
                            Ok(llvm::core::LLVMBuildFDiv(self.builder, lhs, rhs, name.as_ptr())),
                        _ =>
                            Ok(llvm::core::LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr())),
                    }
                }
            },
            Equal(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntEQ, lhs, rhs, name.as_ptr()))
                }
            },
            NotEqual(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntNE, lhs, rhs, name.as_ptr()))
                }
            },
            Greater(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSGT, lhs, rhs, name.as_ptr()))
                }
            },
            GreaterEq(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSGE, lhs, rhs, name.as_ptr()))
                }
            },
            Less(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSLT, lhs, rhs, name.as_ptr()))
                }
            },
            LessEq(box ref lhs, box ref rhs) => {
                let lhs = self.build_expression(lhs, env)?;
                let rhs = self.build_expression(rhs, env)?;
                unsafe {
                    Ok(llvm::core::LLVMBuildICmp(self.builder, llvm::LLVMIntPredicate::LLVMIntSLE, lhs, rhs, name.as_ptr()))
                }
            },
            Call(box ref func, ref args) => {
                let func = self.build_expression(func, env)?;
                let args: Result<Vec<_>, String> = args.iter().map(|arg| self.build_expression(arg, env)).collect();
                let mut args = args?;
                unsafe {
                    Ok(llvm::core::LLVMBuildCall(self.builder, func, args.as_mut_ptr(), args.len() as u32, name.as_ptr()))
                }
            },
            Block(ref exprs) => {
                exprs.iter().fold(
                    Err("no statement in block".to_owned()),
                    |_, expr| self.build_expression(expr, env)
                )
            },
            Bool(ref b) => {
                let bool_ty = self.build_type(&ast::Type::Bool)?;
                let val = if *b { 1 } else { 0 };
                unsafe {
                    Ok(llvm::core::LLVMConstInt(bool_ty, val as u64, 0))
                }
            }
            Int(ref n) => {
                let int_ty = self.build_type(&ast::Type::Int32)?;
                unsafe {
                    Ok(llvm::core::LLVMConstInt(int_ty, *n as u64, 0))
                }
            },
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            llvm::core::LLVMDisposeBuilder(self.builder);
            llvm::core::LLVMDisposeModule(self.module);
            llvm::core::LLVMContextDispose(self.context);
        }
    }
}

