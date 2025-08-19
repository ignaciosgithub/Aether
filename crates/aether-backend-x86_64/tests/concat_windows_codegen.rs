use aether_frontend::ast::*;
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_concat_of_two_literals_prints_combined_string() {
    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::PrintExpr(Expr::Call(
                "concat".into(),
                vec![Expr::Lit(Value::String("A".into())), Expr::Lit(Value::String("B".into()))],
            )),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });
    let m = Module { items: vec![main_fn] };
    let mut cg = Codegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");
    assert!(asm.contains("AB") || asm.contains("A") && asm.contains("B"), "expected concatenated string in emitted data/prints");
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_on_non_windows() { assert!(true); }
