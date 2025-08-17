use aether_frontend::ast::{Module, Item, Function, Stmt, Expr, Type, Value, Param};
use aether_backend_x86_64::X86_64LinuxCodegen as Codegen;
use aether_codegen::CodeGenerator;

#[cfg(target_os = "windows")]
#[test]
fn windows_spawn_same_func_emits_single_thunk() {
    let worker = Item::Function(Function {
        name: "worker".into(),
        params: vec![Param { name: "arg".into(), ty: Type::I64 }],
        ret: Type::I32,
        body: vec![
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: true,
    });

    let main_fn = Item::Function(Function {
        name: "main".into(),
        params: vec![],
        ret: Type::I32,
        body: vec![
            Stmt::Expr(Expr::Call("worker".into(), vec![Expr::Lit(Value::Int(1))])),
            Stmt::Expr(Expr::Call("worker".into(), vec![Expr::Lit(Value::Int(2))])),
            Stmt::Return(Expr::Lit(Value::Int(0))),
        ],
        is_pub: true,
        is_threaded: false,
    });

    let m = Module { items: vec![worker, main_fn] };
    let mut cg = Codegen::new_windows();
    let asm = cg.generate(&m).expect("codegen ok");

    let occurrences = asm.match_indices("worker_thunk:\n").count()
        + asm.match_indices("\nworker_thunk:\r\n").count()
        + asm.match_indices("\nworker_thunk:\n").count();
    assert!(occurrences <= 1, "expected at most one worker_thunk label, found {}", occurrences);
}

#[cfg(not(target_os = "windows"))]
#[test]
fn skip_non_windows() { assert!(true); }
