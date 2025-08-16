use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value};
use aether_backend_x86_64::X86_64LinuxCodegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_labels_dedup_for_in_order_main() {
    let main_fn = Item::Function(Function {
        name: "main".to_string(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Println("A".to_string()),
            Stmt::Println("B".to_string()),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![main_fn] };
    let mut cg = X86_64LinuxCodegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    let ls0_defs = asm.matches("\nLS0:\n").count();
    let ls1_defs = asm.matches("\nLS1:\n").count();

    assert_eq!(ls0_defs, 1, "expected exactly one LS0 definition, found {}", ls0_defs);
    assert_eq!(ls1_defs, 1, "expected exactly one LS1 definition, found {}", ls1_defs);
}

#[cfg(not(target_os = "windows"))]
#[test]
fn windows_labels_dedup_for_in_order_main_skipped() {
    assert!(true);
}
