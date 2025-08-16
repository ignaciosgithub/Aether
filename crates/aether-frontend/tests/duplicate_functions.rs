use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;

#[test]
fn parser_errors_on_duplicate_function_names() {
    let src = r#"
pub func worker() -> i32 { return 0; }
func worker() -> i32 { return 1; }
"#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    let err = Parser::parse(&toks).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("duplicate function name 'worker'"));
}

#[test]
fn parser_accepts_unique_function_names() {
    let src = r#"
pub func worker() -> i32 { return 0; }
func worker2() -> i32 { return 1; }
"#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    let module = Parser::parse(&toks).expect("parse ok");
    assert_eq!(module.items.len(), 2);
}
