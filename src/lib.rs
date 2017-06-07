/*============================================================================
  Copyright (C) 2017 akitsu sanae
  https://github.com/akitsu-sanae/kazuma
  Distributed under the Boost Software License, Version 1.0. (See accompanying
  file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
============================================================================*/

#![feature(box_syntax)]
#![feature(box_patterns)]

extern crate llvm_sys as llvm;

use std::ffi::CString;
use std::ptr;

type LContext = llvm::prelude::LLVMContextRef;
type LModule = llvm::prelude::LLVMModuleRef;
type LBuilder = llvm::prelude::LLVMBuilderRef;
type LValue = llvm::prelude::LLVMValueRef;

pub mod ast;
mod type_checker;

pub struct Builder {
    context: LContext,
    module: LModule,
    builder: LBuilder,
}

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

    pub fn module(&self, module: &ast::Module) -> Result<String, String> {
        type_checker::check(module)?;

        for func in module.functions.iter() {
            self.function(func);
        }
        unsafe {
            CString::from_raw(llvm::core::LLVMPrintModuleToString(self.module)).into_string().map_err(|e| format!("{}", e))
        }
    }

    fn function(&self, func: &ast::Function) {
        unsafe {
            let void_type = llvm::core::LLVMVoidTypeInContext(self.context);
            let func_type  = llvm::core::LLVMFunctionType(void_type, ptr::null_mut(), 0, 0);
            let function = llvm::core::LLVMAddFunction(self.module, func.name.as_str().as_ptr() as *const _, func_type);
            let block = llvm::core::LLVMAppendBasicBlockInContext(self.context, function, b"entry\0".as_ptr() as *const _);
            llvm::core::LLVMPositionBuilderAtEnd(self.builder, block);
            let result = self.expression(&func.body);
            llvm::core::LLVMBuildRet(self.builder, result);
        }
    }

    fn expression(&self, expr: &ast::Expression) -> LValue {
        use ast::Expression::*;
        match *expr {
            Add(box ref lhs, box ref rhs) => {
                let lhs = self.expression(lhs);
                let rhs = self.expression(rhs);
                let name = CString::new("addtmp").unwrap();
                unsafe {
                    llvm::core::LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr())
                }
            },
            Sub(box ref lhs, box ref rhs) => {
                let lhs = self.expression(lhs);
                let rhs = self.expression(rhs);
                let name = CString::new("subtmp").unwrap();
                unsafe {
                    llvm::core::LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr())
                }
            },
            Mult(box ref lhs, box ref rhs) => {
                let lhs = self.expression(lhs);
                let rhs = self.expression(rhs);
                let name = CString::new("subtmp").unwrap();
                unsafe {
                    llvm::core::LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr())
                }
            },
            Div(box ref lhs, box ref rhs) => {
                let lhs = self.expression(lhs);
                let rhs = self.expression(rhs);
                let name = CString::new("subtmp").unwrap();
                unsafe {
                    llvm::core::LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr())
                }
            },
            Literal(ref lit) => self.literal(lit),
        }
    }

    fn literal(&self, lit: &ast::Literal) -> LValue {
        use ast::Literal::*;
        match *lit {
            Int1(ref n) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt1TypeInContext(self.context);
                    llvm::core::LLVMConstInt(int_type, n.clone() as u64, 0)
                }
            },
            Int8(ref n) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt8TypeInContext(self.context);
                    llvm::core::LLVMConstInt(int_type, n.clone() as u64, 0)
                }
            },
            Int16(ref n) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt16TypeInContext(self.context);
                    llvm::core::LLVMConstInt(int_type, n.clone() as u64, 0)
                }
            },
            Int32(ref n) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt32TypeInContext(self.context);
                    llvm::core::LLVMConstInt(int_type, n.clone() as u64, 0)
                }
            },
            Int64(ref n) => {
                unsafe {
                    let int_type = llvm::core::LLVMInt64TypeInContext(self.context);
                    llvm::core::LLVMConstInt(int_type, n.clone() as u64, 0)
                }
            },

            UInt8(ref n) => {
                unsafe {
                    let uint_type = llvm::core::LLVMInt8TypeInContext(self.context);
                    llvm::core::LLVMConstInt(uint_type, n.clone() as u64, 1)
                }
            },
            UInt16(ref n) => {
                unsafe {
                    let uint_type = llvm::core::LLVMInt16TypeInContext(self.context);
                    llvm::core::LLVMConstInt(uint_type, n.clone() as u64, 1)
                }
            },
            UInt32(ref n) => {
                unsafe {
                    let uint_type = llvm::core::LLVMInt32TypeInContext(self.context);
                    llvm::core::LLVMConstInt(uint_type, n.clone() as u64, 1)
                }
            },
            UInt64(ref n) => {
                unsafe {
                    let uint_type = llvm::core::LLVMInt64TypeInContext(self.context);
                    llvm::core::LLVMConstInt(uint_type, n.clone() as u64, 1)
                }
            },

            Float(ref n) => {
                unsafe {
                    let float_type = llvm::core::LLVMFloatTypeInContext(self.context);
                    llvm::core::LLVMConstReal(float_type, n.clone())
                }
            },
            Double(ref n) => {
                unsafe {
                    let double_type = llvm::core::LLVMDoubleTypeInContext(self.context);
                    llvm::core::LLVMConstReal(double_type, n.clone())
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

#[test]
fn test() {
    let builder = Builder::new("test");
    builder.module(&ast::Module{
        functions: vec![
            ast::Function {
                name: "main".to_string(),
                body: ast::Expression::Add(
                    box ast::Expression::Literal(ast::Literal::Int32(4)),
                    box ast::Expression::Add(
                        box ast::Expression::Literal(ast::Literal::Int32(3)),
                        box ast::Expression::Literal(ast::Literal::Int32(5))))
            }
        ]
    }).unwrap();
}

