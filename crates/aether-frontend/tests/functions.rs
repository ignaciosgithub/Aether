use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;
use aether_frontend::ast::*;

#[test]
fn parse_multiple_functions_and_call() {
    let src = r#"
        func foo() -> i32 { return 42; }
        pub func main() -> i32 {
            foo();
            return 0;
        }
    "#;
    let toks = Lexer::tokenize(src).expect("lex");
    let module = Parser::parse(&toks).expect("parse");
    assert!(module.items.len() == 2);
    let mut saw_foo = false;
    let mut saw_main = false;
    for item in module.items {
        if let Item::Function(f) = item {
            if f.name == "foo" { saw_foo = true; }
            if f.name == "main" { saw_main = true; }
        }
    }
    assert!(saw_foo && saw_main);
}
