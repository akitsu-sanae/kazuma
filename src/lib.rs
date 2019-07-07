#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(slice_patterns)]

#[macro_use]
extern crate lazy_static;

extern crate libc;
extern crate llvm_sys as llvm;

mod codegen;
mod error;
mod program;
mod test;
mod typ;
mod typecheck;

pub fn generate(module: program::Module) -> Result<String, error::CodegenError> {
    typecheck::check(&module)?;
    codegen::generate(module)
}
