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
use std::collections::HashMap;

type LContext = llvm::prelude::LLVMContextRef;
type LModule = llvm::prelude::LLVMModuleRef;
type LBuilder = llvm::prelude::LLVMBuilderRef;
type LType = llvm::prelude::LLVMTypeRef;
type LValue = llvm::prelude::LLVMValueRef;

pub mod ast;

pub struct Builder {
    context: LContext,
    module: LModule,
    builder: LBuilder,
}

fn int_literal(n: i32, context: &LContext) -> LValue {
    unsafe {
        let int_ty = llvm::core::LLVMInt32TypeInContext(*context);
        llvm::core::LLVMConstInt(int_ty, n as u64, 0)
    }
}

fn char_literal(ch: u8, context: &LContext) -> LValue {
    unsafe {
        let char_ty = llvm::core::LLVMInt8TypeInContext(*context);
        llvm::core::LLVMConstInt(char_ty, ch as u64, 0)
    }
}

fn str_literal(str: &str, context: &LContext) -> LValue {
    let mut bytes: Vec<_> = str.bytes()
        .map(|ch| char_literal(ch, context))
        .collect();
    bytes.push(char_literal(0, context));
    unsafe {
        let char_ty = llvm::core::LLVMInt8TypeInContext(*context);
        llvm::core::LLVMConstArray(char_ty, bytes.as_mut_ptr(), bytes.len() as std::os::raw::c_uint)
    }
}

fn setup(context: &mut LContext, module: &mut LModule, builder: &mut LBuilder) {
    // printf
    unsafe {
        let print_name = CString::new("printf").unwrap();
        let int_ty = llvm::core::LLVMInt32TypeInContext(*context);
        let mut args = vec![
            llvm::core::LLVMPointerType(
                llvm::core::LLVMInt8TypeInContext(*context), 0)
        ];
        let printf_ty = llvm::core::LLVMFunctionType(int_ty, args.as_mut_ptr(), 1, 1);
        llvm::core::LLVMAddFunction(*module, print_name.as_ptr(), printf_ty);
    }

    // "%d\n" as format string
    let printf_format_name = CString::new(".buildin.printf.format.num").unwrap();
    let init = str_literal("%d\n", context);
    unsafe {
        let global = llvm::core::LLVMAddGlobal(
            *module,
            llvm::core::LLVMTypeOf(init),
            printf_format_name.as_ptr());
        llvm::core::LLVMSetInitializer(global, init);
    }
}


impl Builder {
    pub fn new(name: &str) -> Self {
        unsafe {
            let mut context = llvm::core::LLVMContextCreate();
            let mut module = llvm::core::LLVMModuleCreateWithName(name.as_ptr() as *const _);
            let mut builder = llvm::core::LLVMCreateBuilderInContext(context);
            setup(&mut context, &mut module, &mut builder);
            Builder {
                context: context,
                module: module,
                builder: builder,
            }
        }
    }

    pub fn module(&self, module: &ast::Module) -> Result<String, String> {
        for func in module.functions.iter() {
            self.function(func);
        }
        unsafe {
            CString::from_raw(llvm::core::LLVMPrintModuleToString(self.module)).into_string().map_err(|e| format!("{}", e))
        }
    }

    fn type_(&self, ty: &ast::Type) -> LType {
        match *ty {
            ast::Type::Boolean => unsafe {
                llvm::core::LLVMInt1TypeInContext(self.context)
            },
            ast::Type::Integer => unsafe {
                llvm::core::LLVMInt32TypeInContext(self.context)
            },
            ast::Type::String => panic!("unimplemented"),
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
            let ret_value = self.expression(&func.body, &mut HashMap::new());
            llvm::core::LLVMBuildRet(self.builder, ret_value);
        }
    }

    fn expression(&self, expr: &ast::Expression, env: &mut HashMap<String, LValue>) -> LValue {
        use ast::Expression::*;
        match *expr {
            Variable(ref name) => {
                let var = *env.get(name).unwrap();
                unsafe {
                    let name = CString::new("loaded").unwrap();
                    llvm::core::LLVMBuildLoad(self.builder, var, name.as_ptr())
                }
            },
            BinOp(ref op, box ref lhs, box ref rhs) => self.binop(op, lhs, rhs, env),
            Print(box ref e) => {
                let value = self.expression(e, env);
                unsafe {
                    let printf_name = CString::new("printf").unwrap();
                    let printf = llvm::core::LLVMGetNamedFunction(self.module, printf_name.as_ptr());
                    let format_name = CString::new(".buildin.printf.format.num").unwrap();
                    let format = llvm::core::LLVMGetNamedGlobal(self.module, format_name.as_ptr());
                    let format_ptr_name = CString::new("gep").unwrap();
                    let mut indices = vec![int_literal(0, &self.context), int_literal(0, &self.context)];
                    let format_ptr = llvm::core::LLVMBuildGEP(
                        self.builder,
                        format,
                        indices.as_mut_ptr(),
                        indices.len() as std::os::raw::c_uint,
                        format_ptr_name.as_ptr());
                    let mut args = vec![format_ptr, value];
                    let ret_name = CString::new("ret").unwrap();
                    llvm::core::LLVMBuildCall(
                        self.builder,
                        printf,
                        args.as_mut_ptr(),
                        args.len() as std::os::raw::c_uint,
                        ret_name.as_ptr())
                }
            },
            Let(ref name, ref typ, box ref init) => {
                let ty = self.type_(typ);
                let init = self.expression(init, env);
                unsafe {
                    let stack = llvm::core::LLVMBuildAlloca(self.builder, ty, name.as_ptr() as *const i8);
                    llvm::core::LLVMBuildStore(self.builder, init, stack)
                }
            },
            Assign(ref name, box ref rhs) => {
                let var = *env.get(name).unwrap();
                let rhs = self.expression(rhs, env);
                unsafe {
                    llvm::core::LLVMBuildStore(self.builder, rhs, var)
                }
            }
            Literal(ref lit) => self.literal(lit)
        }
    }

    fn binop(&self, op: &ast::BinaryOperator, lhs: &ast::Expression, rhs: &ast::Expression, env: &mut HashMap<String, LValue>) -> LValue {
        use ast::Expression::*;
        use ast::BinaryOperator::*;
        let lhs = self.expression(lhs, env);
        let rhs = self.expression(rhs, env);
        match *op {
            Add => unsafe {
                let name = CString::new("addtmp").unwrap();
                llvm::core::LLVMBuildAdd(self.builder, lhs, rhs, name.as_ptr())
            },
            Sub => unsafe {
                let name = CString::new("subtmp").unwrap();
                llvm::core::LLVMBuildSub(self.builder, lhs, rhs, name.as_ptr())
            },
            Mult => unsafe {
                let name = CString::new("multmp").unwrap();
                llvm::core::LLVMBuildMul(self.builder, lhs, rhs, name.as_ptr())
            },
            Div => unsafe {
                let name = CString::new("divtmp").unwrap();
                llvm::core::LLVMBuildSDiv(self.builder, lhs, rhs, name.as_ptr())
            },
            Equal | NotEqual | Greater | GreaterEqual | Less | LessEqual => unsafe {
                let name = CString::new("cmptmp").unwrap();
                llvm::core::LLVMBuildICmp(self.builder, convert_to_llvm(op).unwrap(), lhs, rhs, name.as_ptr())
            },

            Call => panic!("unimplemented"),
            Sequent => rhs,
        }
    }

    fn literal(&self, lit: &ast::Literal) -> LValue {
        use ast::Literal::*;
        match *lit {
            Boolean(ref b) => {
                unsafe {
                    let int_type = self.type_(&ast::Type::Boolean);
                    llvm::core::LLVMConstInt(int_type, if *b { 1 } else { 0 }, 0)
                }
            }
            Integer(ref n) => {
                unsafe {
                    let int_type = self.type_(&ast::Type::Integer);
                    llvm::core::LLVMConstInt(int_type, *n as u64, 0)
                }
            }
            String(ref str) => {
                panic!("unimplemented!")
            }
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

fn convert_to_llvm(op: &ast::BinaryOperator) -> Result<llvm::LLVMIntPredicate, String> {
    use ast::BinaryOperator::*;
    Ok(match *op {
        Equal => llvm::LLVMIntPredicate::LLVMIntEQ,
        NotEqual => llvm::LLVMIntPredicate::LLVMIntNE,
        Greater => llvm::LLVMIntPredicate::LLVMIntSGT,
        GreaterEqual => llvm::LLVMIntPredicate::LLVMIntSGE,
        Less => llvm::LLVMIntPredicate::LLVMIntSLT,
        LessEqual => llvm::LLVMIntPredicate::LLVMIntSLE,
        _ => Err(format!("non compare expr: {:?}", *op))?,
    })
}

