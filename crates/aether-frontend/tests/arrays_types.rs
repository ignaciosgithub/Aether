use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;

#[test]
fn parse_array_and_vector_and_hlist_types() {
    let src = r#"
        func f(a: [i64; 8], v: vec[i32]) -> i32 {
            return 0;
        }
        func g(h: hlist) -> i32 { return 0; }
    "#;
    let toks = Lexer::tokenize(src).unwrap();
    let module = Parser::parse(&toks);
    assert!(module.is_ok());
}
