use aether_frontend::lexer::{Lexer, TokenKind};

fn has_ident(idents: &[&str], toks: &[aether_frontend::lexer::Token]) -> bool {
    toks.iter().any(|t| {
        if let TokenKind::Ident(name) = &t.kind {
            idents.iter().any(|s| name == s)
        } else {
            false
        }
    })
}

#[test]
fn skips_slash_slash_line_comments() {
    let src = r#"
pub func main() -> i32 { return 0; }
"#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    assert!(!has_ident(&["foo", "bar", "baz", "another", "comment", "with", "tokens", "like"], &toks));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Func)));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Ident(ref s) if s == "main")));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::I32)));
}

#[test]
fn skips_hash_line_comments() {
    let src = r#"
# foo
# func should not be seen here
pub func main() -> i32 { return 0; }
"#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    assert!(!has_ident(&["foo"], &toks));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Func)));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Ident(ref s) if s == "main")));
}

#[test]
fn skips_block_comments_non_nested() {
    let src = r#"
pub /* func hidden() -> i32 { return 1; } */ func main() -> i32 { /* return 5; */ return 0; }
/* block start
   func also_hidden() -> i32 { return 2; }
end */
"#;
    let toks = Lexer::tokenize(src).expect("lex ok");
    assert!(!has_ident(&["hidden", "also_hidden"], &toks));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Func)));
    assert!(toks.iter().any(|t| matches!(t.kind, TokenKind::Ident(ref s) if s == "main")));
}

#[test]
fn unterminated_block_comment_errors() {
    let src = "/* hello";
    let err = Lexer::tokenize(src).unwrap_err();
    let msg = format!("{err:#}");
    assert!(msg.contains("unterminated block comment"));
}
