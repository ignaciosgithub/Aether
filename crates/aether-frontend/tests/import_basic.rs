use aether_frontend::{parse_file_with_imports};
use std::fs;
use std::path::Path;

#[test]
fn resolves_simple_import() {
    let dir = tempfile::tempdir().unwrap();
    let lib = r#"
        pub func foo() -> i32 { return 1; }
    "#;
    let main_src = r#"
        import "lib.ae";
        pub func main() -> i32 { return foo(); }
    "#;
    fs::write(dir.path().join("lib.ae"), lib).unwrap();
    fs::write(dir.path().join("main.ae"), main_src).unwrap();

    let m = parse_file_with_imports(dir.path().join("main.ae")).expect("resolve ok");
    assert!(m.items.iter().any(|it| matches!(it, aether_frontend::ast::Item::Function(f) if f.name=="foo")));
    assert!(m.items.iter().any(|it| matches!(it, aether_frontend::ast::Item::Function(f) if f.name=="main")));
}

#[test]
fn detects_cycle() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("a.ae"), r#"import "b.ae"; pub func a()->i32{ return 0; }"#).unwrap();
    fs::write(dir.path().join("b.ae"), r#"import "a.ae"; pub func b()->i32{ return 0; }"#).unwrap();
    let err = parse_file_with_imports(dir.path().join("a.ae")).err().unwrap();
    let msg = format!("{}", err);
    assert!(msg.contains("circular import"));
}
