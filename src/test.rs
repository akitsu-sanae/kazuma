
use std::fs;
use std::io::Write;
use super::*;

#[cfg(test)]
fn output(module: Module, filename: &str) {
    let mut f = fs::File::create(&format!("./test/{}", filename)).unwrap();
    write!(f, "{}", gencode(module)).unwrap();
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
            ret_typ: Type::Int,
            body: vec!()
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
            ret_typ: Type::Int,
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
            ret_typ: Type::Int,
            body: vec!(Statement::Return(Expr::BinOp(
                        BinOp::Add,
                        box Expr::Literal(Literal::Int(114)),
                        box Expr::Literal(Literal::Int(514))))),
        }),
    };
    output(module, "binop_expr_test.ll")
}
