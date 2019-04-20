#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(slice_patterns)]

#[macro_use]
extern crate lazy_static;

extern crate llvm_sys as llvm;
extern crate libc;

mod program;
mod typ;
mod error;
mod codegen;
mod typecheck;
mod test;


pub fn generate(module: program::Module) -> Result<String, error::CodegenError> {
    typecheck::check(&module)?;
    codegen::generate(module)
}

