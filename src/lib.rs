#![feature(box_syntax)]
#![feature(box_patterns)]

#[macro_use]
extern crate lazy_static;

extern crate libc;
extern crate llvm_sys as llvm;

pub mod codegen;
pub mod error;
pub mod program;
pub mod typ;
pub mod typecheck;

#[cfg(test)]
mod test;

pub fn generate(module: program::Module) -> Result<String, error::CodegenError> {
    typecheck::check(&module)?;
    codegen::generate(module)
}
