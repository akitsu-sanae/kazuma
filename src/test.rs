
use std::fs;
use std::io::Write;
use super::*;

#[cfg(test)]
fn output(module: Module, filename: &str) {
    let mut f = fs::File::create(&format!("./test/{}", filename)).unwrap();
    match generate(module) {
        Ok(code) => write!(f, "{}", code).unwrap(),
        Err(err) => panic!("{}", err),
    }
}

#[test]
fn build_module_test() {
    let module = Module {
        name: "module".to_string(),
        funcs: vec!(),
    };
    output(module, "module_test.ll");
}

#[test]
fn build_function_test() {
    let module = Module {
        name: "empty function".to_string(),
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
        name: "ret function".to_string(),
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
        name: "binop expr".to_string(),
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
        name: "if expr".to_string(),
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

#[test]
fn build_var_test() {
    let module = Module {
        name: "var".to_string(),
        funcs: vec!(Func {
            name: "main".to_string(),
            args: vec!(),
            ret_type: Type::Int,
            body: vec!(
                Statement::Declare(
                    "a".to_string(),
                    Type::Int,
                    Expr::Literal(Literal::Int(42))),
                Statement::Return(Expr::Load(box Expr::Var("a".to_string())))),
        }),
    };
    output(module, "var_expr_test.ll")
}

#[test]
fn build_args_test() {
    let module = Module {
        name: "args".to_string(),
        funcs: vec!(
            Func {
                name: "add".to_string(),
                args: vec!(
                    ("a".to_string(), Type::Int),
                    ("b".to_string(), Type::Int)),
                ret_type: Type::Int,
                body: vec!(Statement::Return(Expr::BinOp(
                            BinOp::Add,
                            box Expr::Load(box Expr::Var("a".to_string())),
                            box Expr::Load(box Expr::Var("b".to_string()))))),
            }),
    };
    output(module, "args_test.ll")
}

#[test]
fn build_call_expr_test() {
    let module = Module{
        name: "call_expr".to_string(),
        funcs: vec!(
            Func {
                name: "add".to_string(),
                args: vec!(
                    ("a".to_string(), Type::Int),
                    ("b".to_string(), Type::Int)),
                ret_type: Type::Int,
                body: vec!(Statement::Return(Expr::BinOp(
                            BinOp::Add,
                            box Expr::Load(box Expr::Var("a".to_string())),
                            box Expr::Load(box Expr::Var("b".to_string()))))),
            },
            Func {
                name : "main".to_string(),
                args: vec!(),
                ret_type: Type::Int,
                body: vec!(
                    Statement::Return(Expr::Call(
                            box Expr::Literal(Literal::Func("add".to_string())),
                            vec!(
                                Expr::Literal(Literal::Int(114)),
                                Expr::Literal(Literal::Int(514)))))),
            }),
    };
    output(module, "call_expr_test.ll")
}

#[test]
fn build_func_ptr_test() {
    let module = Module{
        name: "func_ptr".to_string(),
        funcs: vec!(
            Func {
                name: "add".to_string(),
                args: vec!(
                    ("a".to_string(), Type::Int),
                    ("b".to_string(), Type::Int)),
                ret_type: Type::Int,
                body: vec!(Statement::Return(Expr::BinOp(
                            BinOp::Add,
                            box Expr::Load(box Expr::Var("a".to_string())),
                            box Expr::Load(box Expr::Var("b".to_string()))))),
            },
            Func {
                name : "main".to_string(),
                args: vec!(),
                ret_type: Type::Int,
                body: vec!(
                    Statement::Declare(
                        "f".to_string(),
                        Type::Func(vec!(Type::Int, Type::Int), box Type::Int),
                        Expr::Literal(Literal::Func("add".to_string()))),
                    Statement::Return(Expr::Call(
                            box Expr::Load(box Expr::Var("f".to_string())),
                            vec!(
                                Expr::Literal(Literal::Int(114)),
                                Expr::Literal(Literal::Int(514)))))),
            }),
    };
    output(module, "func_ptr_test.ll")
}


