use aether_frontend::lexer::Lexer;
use aether_frontend::parser::Parser;

#[test]
fn parse_struct_lit_with_trailing_comma() {
    let src = r#"
        pub struct Parent { s: String }
        pub struct Child : Parent { b: I32 }
        static C: Child = Child { s: "Hi", b: 1, };
    "#;
    let toks = Lexer::tokenize(src).expect("lex");
    let m = Parser::parse(&toks).expect("parse");
    let mut found = false;
    for it in m.items {
        if let aether_frontend::ast::Item::Static(st) = it {
            if st.name == "C" {
                if let aether_frontend::ast::Expr::StructLit(name, fields) = st.init {
                    assert_eq!(name, "Child");
                    assert_eq!(fields.len(), 2);
                    found = true;
                }
            }
        }
    }
    assert!(found);
}
