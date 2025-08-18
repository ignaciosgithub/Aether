use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;

#[test]
fn parse_addr_of_and_deref() {
    let src = r#"
        func main() -> i32 {
            let x: i64 = 1;
            let p: &i64 = &x;
            let y: i64 = *p;
            return 0;
        }
    "#;
    let toks = Lexer::tokenize(src).unwrap();
    let module = Parser::parse(&toks);
    assert!(module.is_ok());
}

#[test]
fn parse_indexing_with_pointers_arrays() {
    let src = r#"
        func main() -> i32 {
            let a: [i32; 4] = [1,2,3,4];
            let p: &i32 = &a[2];
            let v: i32 = *p;
            return 0;
        }
    "#;
    let toks = Lexer::tokenize(src).unwrap();
    let module = Parser::parse(&toks);
    assert!(module.is_ok());
}
