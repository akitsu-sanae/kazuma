
use std::fs;
use std::io::Write;
use super::*;

#[cfg(test)]
fn output(module: Module, filename: &str) {
    let mut f = fs::File::create(&format!("./test/{}", filename)).unwrap();
    match gencode(module) {
        Ok(code) => write!(f, "{}", code).unwrap(),
        Err(err) => panic!("{}", err),
    }
}

#[test]
fn build_module_test() {
    let module = Module {
        name: "test".to_string(),
        funcs: vec!(),
    };
    output(module, "module_test.ll");
}

#[test]
fn build_function_test() {
    let module = Module {
        name: "test".to_string(),
        funcs: vec!(Func {
            name: "main".to_string(),
            args: vec!(),
            ret_type: Type::Void,
            body: vec!(Statement::ReturnVoid),
        }),
    };
    output(module, "function_test.ll")
}

#[test]
fn build_ret_function_test() {
    let module = Module {
        name: "test".to_string(),
        funcs: vec!(Func {
            name: "main".to_string(),
            args: vec!(),
            ret_type: Type::Int,
            body: vec!(Statement::Return(Expr::Literal(Literal::Int(42)))),
        }),
    };
    output(module, "ret_function_test.ll")
}

#[test]
fn build_binop_expr_test() {
    let module = Module {
        name: "test".to_string(),
        funcs: vec!(Func {
            name: "main".to_string(),
            args: vec!(),
            ret_type: Type::Int,
            body: vec!(Statement::Return(Expr::BinOp(
                        BinOp::Add,
                        box Expr::Literal(Literal::Int(114)),
                        box Expr::Literal(Literal::Int(514))))),
        }),
    };
    output(module, "binop_expr_test.ll")
}

#[test]
fn build_if_expr_test() {
    let module = Module {
        name: "test".to_string(),
        funcs: vec!(Func {
            name: "main".to_string(),
            args: vec!(),
            ret_type: Type::Int,
            body: vec!(Statement::Return(Expr::If(
                        box Expr::Literal(Literal::Bool(true)),
                        box Expr::Literal(Literal::Int(114)),
                        box Expr::Literal(Literal::Int(514))))),
        }),
    };
    output(module, "if_expr_test.ll")
}

