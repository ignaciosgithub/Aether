use aether_codegen::CodeGenerator;
use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen;

fn module_nested() -> Module {
    let inner = StructDef {
        name: "Inner".to_string(),
        fields: vec![
            StructField { name: "x".to_string(), ty: Type::I64 },
            StructField { name: "y".to_string(), ty: Type::I32 },
        ],
        parent: None,
    };
    let outer = StructDef {
        name: "Outer".to_string(),
        fields: vec![
            StructField { name: "a".to_string(), ty: Type::I32 },
            StructField { name: "inner".to_string(), ty: Type::User("Inner".to_string()) },
            StructField { name: "s".to_string(), ty: Type::String },
        ],
        parent: None,
    };
    let g = StaticVar {
        name: "G".to_string(),
        ty: Type::User("Outer".to_string()),
        init: Expr::StructLit("Outer".to_string(), vec![
            ("a".to_string(), Expr::Lit(Value::Int(1))),
            ("inner".to_string(), Expr::StructLit("Inner".to_string(), vec![
                ("x".to_string(), Expr::Lit(Value::Int(9))),
                ("y".to_string(), Expr::Lit(Value::Int(7))),
            ])),
            ("s".to_string(), Expr::Lit(Value::String("Hi".to_string()))),
        ]),
    };
    let usei = Function {
        name: "usei".to_string(),
        params: vec![Param { name: "v".to_string(), ty: Type::I32 }],
        ret: Type::I32,
        body: vec![Stmt::Return(Expr::Var("v".to_string()))],
        is_pub: false,
        is_threaded: false,
    };
    let mainf = Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("usei".to_string(), vec![
                Expr::Field(Box::new(Expr::Field(Box::new(Expr::Var("G".to_string())), "inner".to_string())), "y".to_string())
            ])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    };
    Module { items: vec![Item::Struct(inner), Item::Struct(outer), Item::Static(g), Item::Function(mainf), Item::Function(usei)] }
}

#[test]
fn linux_field_nested_offset_and_arg_reg() {
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module_nested()).expect("codegen ok");
    assert!(asm.contains("leaq G(%rip"), "self/static address should be materialized for base");
    assert!(asm.contains("mov "), "should move field into arg reg");
    assert!(asm.contains("call usei"), "should call usei with field value");
}

#[test]
fn linux_static_string_field_emitted() {
    let mut cg = X86_64LinuxCodegen::new_linux();
    let asm = cg.generate(&module_nested()).expect("codegen ok");
    assert!(asm.contains("\nG:\n"), "G static should be emitted");
}
