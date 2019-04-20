use std::fmt;
use std::error::Error;

#[derive(Debug)]
pub enum CodegenError {
    TypeCheck(String),
    ModuleBuilding(String),
    ModuleValidation(String),
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CodegenError::*;
        match self {
            TypeCheck(ref msg) => write!(f, "type error: {}", msg),
            ModuleBuilding(ref msg) => write!(f, "module building error: {}", msg),
            ModuleValidation(ref msg) => write!(f, "module validation error: {}", msg),
        }
    }
}

impl Error for CodegenError {
    fn description(&self) -> &str {
        "code generation error"
    }
}

