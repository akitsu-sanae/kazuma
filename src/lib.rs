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

type LContext = llvm::prelude::LLVMContextRef;
type LModule = llvm::prelude::LLVMModuleRef;
type LBuilder = llvm::prelude::LLVMBuilderRef;
type LType = llvm::prelude::LLVMTypeRef;
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

    fn type_(&self, ty: &ast::Type) -> LType {
        use ast::Type::*;
        match *ty {
            Int1 => unsafe {
                llvm::core::LLVMInt1TypeInContext(self.context)
            },
            Int8 | UInt8 => unsafe {
                llvm::core::LLVMInt8TypeInContext(self.context)
            },
            Int16 | UInt16 => unsafe {
                llvm::core::LLVMInt16TypeInContext(self.context)
            },
            Int32 | UInt32 => unsafe {
                llvm::core::LLVMInt32TypeInContext(self.context)
            },
            Int64 | UInt64 => unsafe {
                llvm::core::LLVMInt64TypeInContext(self.context)
            },
            Float => unsafe {
                llvm::core::LLVMFloatTypeInContext(self.context)
            },
            Double => unsafe {
                llvm::core::LLVMDoubleTypeInContext(self.context)
            },
        }
    }

    fn arguments_type(&self, args: &Vec<(String, ast::Type)>) -> Vec<LType> {
        args.iter().map(|&(_, ref ty)| self.type_(ty)).collect()
    }

    fn function(&self, func: &ast::Function) {
        unsafe {
            let mut arg_types = self.arguments_type(&func.arguments);
            let ret_type = self.type_(&func.return_type);
            let func_type  = llvm::core::LLVMFunctionType(ret_type, arg_types.as_mut_ptr(), func.arguments.len() as u32, 0);
            let function = llvm::core::LLVMAddFunction(self.module, func.name.as_str().as_ptr() as *const _, func_type);
            let block = llvm::core::LLVMAppendBasicBlockInContext(self.context, function, b"entry\0".as_ptr() as *const _);
            llvm::core::LLVMPositionBuilderAtEnd(self.builder, block);
            for statement in func.body.iter() {
                self.statement(statement);
            }
        }
    }

    fn statement(&self, statement: &ast::Statement) {
        use ast::Statement::*;
        match *statement {
            Return(ref expr) => unsafe {
                let expr = self.expression(expr);
                llvm::core::LLVMBuildRet(self.builder, expr);
            },
            ReturnVoid => unsafe {
                llvm::core::LLVMBuildRetVoid(self.builder);
            },
            Expression(ref expr) => {
                self.expression(expr);
            }
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
            Equal(box ref lhs, box ref rhs) | NotEqual(box ref lhs, box ref rhs) |
            Greater(box ref lhs, box ref rhs) | GreaterEqual(box ref lhs, box ref rhs) |
            Less(box ref lhs, box ref rhs) | LessEqual(box ref lhs, box ref rhs) => {
                let lhs = self.expression(lhs);
                let rhs = self.expression(rhs);
                let name = CString::new("cmptmp").unwrap();
                unsafe {
                    llvm::core::LLVMBuildICmp(self.builder, self.comp_pred(expr).unwrap(), lhs, rhs, name.as_ptr())
                }
            },
            Literal(ref lit) => self.literal(lit),
        }
    }

    fn comp_pred(&self, expr: &ast::Expression) -> Result<llvm::LLVMIntPredicate, String> {
        use ast::Expression::*;
        Ok(match *expr {
            Equal(_, _) => llvm::LLVMIntPredicate::LLVMIntEQ,
            NotEqual(_, _) => llvm::LLVMIntPredicate::LLVMIntNE,
            Greater(_, _) => llvm::LLVMIntPredicate::LLVMIntSGT,
            GreaterEqual(_, _) => llvm::LLVMIntPredicate::LLVMIntSGE,
            Less(_, _) => llvm::LLVMIntPredicate::LLVMIntSLT,
            LessEqual(_, _) => llvm::LLVMIntPredicate::LLVMIntSLE,
            _ => Err(format!("non compare expr: {:?}", expr))?,
        })
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
                arguments: vec![("a".to_string(), ast::Type::Int8)],
                return_type: ast::Type::Int16,
                body: vec![
                    ast::Statement::Return(
                        ast::Expression::Add(
                            box ast::Expression::Literal(ast::Literal::Int32(4)),
                            box ast::Expression::Add(
                                box ast::Expression::Literal(ast::Literal::Int32(3)),
                                box ast::Expression::Literal(ast::Literal::Int32(5)))))
                ]
            }
        ]
    }).unwrap();
}

