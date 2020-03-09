use crate::program::*;
use crate::typ::*;

macro_rules! hashmap(
    { $($key:expr => $value:expr),+} => {
        {
            let mut map = ::std::collections::HashMap::new();
            $(
                map.insert($key, $value);
            )+
            map
        }
    };
    {} => {
        {
            ::std::collections::HashMap::new()
        }
    };
);

fn check(module: Module, expected: &str, filename: &str) {
    use std::fs;
    use std::io::Write;
    use std::process::Command;
    use std::str;
    let filename = format!("./test/{}", filename);
    {
        // write llvm-ir
        let mut f = fs::File::create(&filename).unwrap();
        match crate::generate(module) {
            Ok(code) => write!(f, "{}", code).unwrap(),
            Err(err) => panic!("{}", err),
        }
    }
    {
        // run & check
        let result = Command::new("lli")
            .arg(&filename)
            .output()
            .expect("failed to execute lli");
        let output = str::from_utf8(&result.stdout).expect("unrecognazed outut");
        assert_eq!(output, expected);
        assert!(result.status.success());
    }
}

#[test]
fn build_function_test() {
    // int main() { return 0; }
    let module = Module {
        name: "empty function".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![Func {
            name: "main".to_string(),
            args: vec![],
            ret_type: Type::Int,
            body: vec![Statement::Return(Expr::Literal(Literal::Int(0)))],
        }],
    };
    check(module, "", "function_test.ll")
}

#[test]
fn build_int_literal_test() {
    // int main() {
    //   printf("%d\n", 42);
    //   return 0;
    // }
    let module = Module {
        name: "int literal".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![Func {
            name: "main".to_string(),
            args: vec![],
            ret_type: Type::Int,
            body: vec![
                Statement::PrintNum(Expr::Literal(Literal::Int(42))),
                Statement::Return(Expr::Literal(Literal::Int(0))),
            ],
        }],
    };
    check(module, "42\n", "int_literal_test.ll")
}

#[test]
fn build_binop_expr_test() {
    // int main() {
    //   printf("%d\n", 114 + 514);
    //   return 0;
    // }
    let module = Module {
        name: "binop expr".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![Func {
            name: "main".to_string(),
            args: vec![],
            ret_type: Type::Int,
            body: vec![
                Statement::PrintNum(Expr::BinOp(
                    BinOp::Add,
                    box Expr::Literal(Literal::Int(114)),
                    box Expr::Literal(Literal::Int(514)),
                )),
                Statement::Return(Expr::Literal(Literal::Int(0))),
            ],
        }],
    };
    check(module, "628\n", "binop_expr_test.ll")
}

#[test]
fn build_if_expr_test() {
    // int main() {
    //   printf("%d\n", if true {114} else {514});
    // }
    let module = Module {
        name: "if expr".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![Func {
            name: "main".to_string(),
            args: vec![],
            ret_type: Type::Int,
            body: vec![
                Statement::PrintNum(Expr::If(
                    box Expr::Literal(Literal::Bool(true)),
                    box Expr::Literal(Literal::Int(114)),
                    box Expr::Literal(Literal::Int(514)),
                )),
                Statement::Return(Expr::Literal(Literal::Int(0))),
            ],
        }],
    };
    check(module, "114\n", "if_expr_test.ll")
}

#[test]
fn build_var_test() {
    // int main() {
    //   int a = 42;
    //   printf("%d\n", a);
    //   return 0;
    // }
    let module = Module {
        name: "var".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![Func {
            name: "main".to_string(),
            args: vec![],
            ret_type: Type::Int,
            body: vec![
                Statement::Declare("a".to_string(), Type::Int, Expr::Literal(Literal::Int(42))),
                Statement::PrintNum(Expr::Load(box Expr::Var("a".to_string()))),
                Statement::Return(Expr::Literal(Literal::Int(0))),
            ],
        }],
    };
    check(module, "42\n", "var_expr_test.ll")
}

#[test]
fn build_args_test() {
    // int add(int a, int b) {
    //   return a + b;
    // }
    // int main() {
    //   return 0;
    // }
    let module = Module {
        name: "args".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![
            Func {
                name: "add".to_string(),
                args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                ret_type: Type::Int,
                body: vec![Statement::Return(Expr::BinOp(
                    BinOp::Add,
                    box Expr::Load(box Expr::Var("a".to_string())),
                    box Expr::Load(box Expr::Var("b".to_string())),
                ))],
            },
            Func {
                name: "main".to_string(),
                args: vec![],
                ret_type: Type::Int,
                body: vec![Statement::Return(Expr::Literal(Literal::Int(0)))],
            },
        ],
    };
    check(module, "", "args_test.ll")
}

#[test]
fn build_call_expr_test() {
    // int add(int a, int b) {
    //   return a + b;
    // }
    // int main() {
    //   printf("%d\n", add(114, 514));
    //   return 0;
    // }
    let module = Module {
        name: "call_expr".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![
            Func {
                name: "add".to_string(),
                args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                ret_type: Type::Int,
                body: vec![Statement::Return(Expr::BinOp(
                    BinOp::Add,
                    box Expr::Load(box Expr::Var("a".to_string())),
                    box Expr::Load(box Expr::Var("b".to_string())),
                ))],
            },
            Func {
                name: "main".to_string(),
                args: vec![],
                ret_type: Type::Int,
                body: vec![
                    Statement::PrintNum(Expr::Call(
                        box Expr::Literal(Literal::Func("add".to_string())),
                        vec![
                            Expr::Literal(Literal::Int(114)),
                            Expr::Literal(Literal::Int(514)),
                        ],
                    )),
                    Statement::Return(Expr::Literal(Literal::Int(0))),
                ],
            },
        ],
    };
    check(module, "628\n", "call_expr_test.ll")
}

#[test]
fn build_func_ptr_test() {
    // int add(int a, int b) {
    //   return a + b;
    // }
    // int main() {
    //   int (*f)(int, int) = add;
    //   printf("%d\n", f(114, 514));
    //   return 0;
    // }
    let module = Module {
        name: "func_ptr".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![
            Func {
                name: "add".to_string(),
                args: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                ret_type: Type::Int,
                body: vec![Statement::Return(Expr::BinOp(
                    BinOp::Add,
                    box Expr::Load(box Expr::Var("a".to_string())),
                    box Expr::Load(box Expr::Var("b".to_string())),
                ))],
            },
            Func {
                name: "main".to_string(),
                args: vec![],
                ret_type: Type::Int,
                body: vec![
                    Statement::Declare(
                        "f".to_string(),
                        Type::Pointer(box Type::Func(vec![Type::Int, Type::Int], box Type::Int)),
                        Expr::Literal(Literal::Func("add".to_string())),
                    ),
                    Statement::PrintNum(Expr::Call(
                        box Expr::Load(box Expr::Var("f".to_string())),
                        vec![
                            Expr::Literal(Literal::Int(114)),
                            Expr::Literal(Literal::Int(514)),
                        ],
                    )),
                    Statement::Return(Expr::Literal(Literal::Int(0))),
                ],
            },
        ],
    };
    check(module, "628\n", "func_ptr_test.ll")
}

#[test]
fn build_array_test() {
    // int main() {
    //   int arr[2] = {114, 514};
    //   arr[1] = 42;
    //   printf("%d\n", arr[1]);
    //   return 0;
    // }
    let module = Module {
        name: "array".to_string(),
        struct_types: vec![],
        global_var: hashmap! {},
        funcs: vec![Func {
            name: "main".to_string(),
            args: vec![],
            ret_type: Type::Int,
            body: vec![
                Statement::Declare(
                    "arr".to_string(),
                    Type::Array(box Type::Int, 2),
                    Expr::Literal(Literal::Array(
                        vec![
                            Expr::Literal(Literal::Int(114)),
                            Expr::Literal(Literal::Int(514)),
                        ],
                        Type::Int,
                    )),
                ),
                Statement::Assign(
                    Expr::ArrayAt(
                        box Expr::Var("arr".to_string()),
                        box Expr::Literal(Literal::Int(1)),
                    ),
                    Expr::Literal(Literal::Int(42)),
                ),
                Statement::PrintNum(Expr::Load(box Expr::ArrayAt(
                    box Expr::Var("arr".to_string()),
                    box Expr::Literal(Literal::Int(1)),
                ))),
                Statement::Return(Expr::Literal(Literal::Int(0))),
            ],
        }],
    };
    check(module, "42\n", "array_test.ll");
}

#[test]
fn build_struct_test() {
    // struct foo_t = {int, int};
    //
    // int main() {
    //   foo_t f = {114, 514};
    //   f.0 = 42;
    //   printf("%d\n", f.0 + f.1);
    //   return 0;
    // }
    let module = Module {
        name: "struct".to_string(),
        struct_types: vec![StructDef {
            name: "foo_t".to_string(),
            fields: vec![Type::Int, Type::Int],
        }],
        global_var: hashmap! {},
        funcs: vec![Func {
            name: "main".to_string(),
            args: vec![],
            ret_type: Type::Int,
            body: vec![
                Statement::Declare(
                    "f".to_string(),
                    Type::StructVar("foo_t".to_string()),
                    Expr::Literal(Literal::Struct(
                        vec![
                            Expr::Literal(Literal::Int(114)),
                            Expr::Literal(Literal::Int(514)),
                        ],
                        "foo_t".to_string(),
                    )),
                ),
                Statement::Assign(
                    Expr::StructAt(box Expr::Var("f".to_string()), 0),
                    Expr::Literal(Literal::Int(42)),
                ),
                Statement::PrintNum(Expr::BinOp(
                    BinOp::Add,
                    box Expr::Load(box Expr::StructAt(box Expr::Var("f".to_string()), 0)),
                    box Expr::Load(box Expr::StructAt(box Expr::Var("f".to_string()), 1)),
                )),
                Statement::Return(Expr::Literal(Literal::Int(0))),
            ],
        }],
    };
    check(module, "556\n", "struct_test.ll");
}

#[test]
fn build_global_var_test() {
    // int counter = 0;
    // void incr() { counter += 1; }
    // int main() {
    //   incr();
    //   incr();
    //   printf("%d\n", counter);
    //   return 0;
    // }
    let module = Module {
        name: "global_var".to_string(),
        struct_types: vec![],
        global_var: hashmap! {"counter".to_string() => (Type::Int, Expr::Literal(Literal::Int(0)))},
        funcs: vec![
            Func {
                name: "incr".to_string(),
                args: vec![],
                ret_type: Type::Void,
                body: vec![
                    Statement::Assign(
                        Expr::Var("counter".to_string()),
                        Expr::BinOp(
                            BinOp::Add,
                            box Expr::Load(box Expr::Var("counter".to_string())),
                            box Expr::Literal(Literal::Int(1)),
                        ),
                    ),
                    Statement::ReturnVoid,
                ],
            },
            Func {
                name: "main".to_string(),
                args: vec![],
                ret_type: Type::Int,
                body: vec![
                    Statement::Expr(Expr::Call(
                        box Expr::Literal(Literal::Func("incr".to_string())),
                        vec![],
                    )),
                    Statement::Expr(Expr::Call(
                        box Expr::Literal(Literal::Func("incr".to_string())),
                        vec![],
                    )),
                    Statement::Expr(Expr::Call(
                        box Expr::Literal(Literal::Func("incr".to_string())),
                        vec![],
                    )),
                    Statement::PrintNum(Expr::Load(box Expr::Var("counter".to_string()))),
                    Statement::Return(Expr::Literal(Literal::Int(0))),
                ],
            },
        ],
    };
    check(module, "3\n", "global_var_test.ll");
}
