#  Kazuma

Kazuma is a LLVM wrapper library for generating LLVM IR.

# Example

Rust Code:
```
use std::collections::HashMap;
use kazuma::{program::*, typ::*, generate};

let module = Module {
    name: "print int literal".to_string(),
    struct_types: vec![],
    global_var: HashMap::new(),
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

generate(module) // -> Result<String, error::CodegenError>
```

Generated LLVM IR:
```
; ModuleID = 'print int literal'
source_filename = "print int literal"

@.builtin.format.num = global [3 x i8] c"%d\0A"

declare i32 @printf(i8*, ...)

declare void @memcpy(i8*, i8*, ...)

define i32 @main() {
entry:
  %0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.builtin.format.num, i32 0, i32 0), i32 42)
  ret i32 0
}
```

See more examples in `src/test.rs`

# Copyright
Copyright (C) 2015-2019 akitsu sanae.  
Distributed under the Boost Software License, Version 1.0. 
(See accompanying file LICENSE or copy at http://www.boost/org/LICENSE_1_0.txt)  


