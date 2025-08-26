use aether_frontend::parse_file_with_imports;
use std::fs;

#[test]
fn not_found_import_errors() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("main.ae"), r#"import "missing.ae"; pub func main()->i32 { return 0; }"#).unwrap();
    let err = parse_file_with_imports(dir.path().join("main.ae")).err().unwrap();
    let msg = format!("{}", err);
    assert!(msg.contains("No such file") || msg.contains("no such file") || msg.contains("not found") || msg.contains("failed"));
}

#[test]
fn duplicate_function_across_imports_errors() {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("a.ae"), r#"pub func foo()->i32 { return 1; }"#).unwrap();
    fs::write(dir.path().join("b.ae"), r#"pub func foo()->i32 { return 2; }"#).unwrap();
    fs::write(dir.path().join("main.ae"), r#"import "a.ae"; import "b.ae"; pub func main()->i32 { return foo(); }"#).unwrap();
    let err = parse_file_with_imports(dir.path().join("main.ae")).err().unwrap();
    let msg = format!("{}", err);
    assert!(msg.contains("duplicate function name"));
}
