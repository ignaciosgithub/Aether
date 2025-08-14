use anyhow::{anyhow, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Int(String),
    Float(String),
    String(String),
    Bool(bool),
    Arrow,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Colon,
    Semicolon,
    Plus,
    Minus,
    Star,
    Slash,
    Return,
    Func,
    Pub,
    Println,
    True,
    False,
    Void,
    I32,
    I64,
    F32,
    F64,
    Any,
    List,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
}

pub struct Lexer;

impl Lexer {
    pub fn tokenize(src: &str) -> Result<Vec<Token>> {
        let mut toks = Vec::new();
        let bytes = src.as_bytes();
        let mut i = 0usize;

        while i < bytes.len() {
            let c = bytes[i] as char;
            if c.is_ascii_whitespace() {
                i += 1;
                continue;
            }
            if c == '/' && i + 1 < bytes.len() && bytes[i + 1] as char == '/' {
                while i < bytes.len() && bytes[i] as char != '\n' {
                    i += 1;
                }
                continue;
            }
            if c.is_ascii_alphabetic() || c == '_' {
                let start = i;
                i += 1;
                while i < bytes.len() {
                    let ch = bytes[i] as char;
                    if ch.is_ascii_alphanumeric() || ch == '_' {
                        i += 1;
                    } else {
                        break;
                    }
                }
                let s = &src[start..i];
                let kind = match s {
                    "return" => TokenKind::Return,
                    "func" => TokenKind::Func,
                    "pub" => TokenKind::Pub,
                    "println" => TokenKind::Println,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "void" => TokenKind::Void,
                    "i32" => TokenKind::I32,
                    "i64" => TokenKind::I64,
                    "f32" => TokenKind::F32,
                    "f64" => TokenKind::F64,
                    "any" => TokenKind::Any,
                    "list" => TokenKind::List,
                    _ => TokenKind::Ident(s.to_string()),
                };
                toks.push(Token { kind });
                continue;
            }
            if c.is_ascii_digit() {
                let start = i;
                i += 1;
                while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                    i += 1;
                }
                let mut is_float = false;
                if i < bytes.len() && bytes[i] as char == '.' {
                    is_float = true;
                    i += 1;
                    while i < bytes.len() && (bytes[i] as char).is_ascii_digit() {
                        i += 1;
                    }
                }
                let s = &src[start..i];
                toks.push(Token {
                    kind: if is_float {
                        TokenKind::Float(s.to_string())
                    } else {
                        TokenKind::Int(s.to_string())
                    },
                });
                continue;
            }
            if c == '\"' {
                i += 1;
                let mut s = String::new();
                while i < bytes.len() {
                    let ch = bytes[i] as char;
                    if ch == '\\' {
                        i += 1;
                        if i >= bytes.len() { return Err(anyhow!("unterminated string")); }
                        let esc = bytes[i] as char;
                        match esc {
                            'n' => s.push('\n'),
                            't' => s.push('\t'),
                            'r' => s.push('\r'),
                            '\\' => s.push('\\'),
                            '"' => s.push('"'),
                            _ => s.push(esc),
                        }
                        i += 1;
                        continue;
                    }
                    if ch == '\"' {
                        i += 1;
                        break;
                    }
                    s.push(ch);
                    i += 1;
                }
                toks.push(Token { kind: TokenKind::String(s) });
                continue;
            }
            let kind = match c {
                '-' if i + 1 < bytes.len() && bytes[i + 1] as char == '>' => {
                    i += 2;
                    TokenKind::Arrow
                }
                '+' => { i += 1; TokenKind::Plus }
                '-' => { i += 1; TokenKind::Minus }
                '*' => { i += 1; TokenKind::Star }
                '/' => { i += 1; TokenKind::Slash }
                '(' => {
                    i += 1;
                    TokenKind::LParen
                }
                ')' => {
                    i += 1;
                    TokenKind::RParen
                }
                '{' => {
                    i += 1;
                    TokenKind::LBrace
                }
                '}' => {
                    i += 1;
                    TokenKind::RBrace
                }
                '[' => {
                    i += 1;
                    TokenKind::LBracket
                }
                ']' => {
                    i += 1;
                    TokenKind::RBracket
                }
                ',' => {
                    i += 1;
                    TokenKind::Comma
                }
                ':' => {
                    i += 1;
                    TokenKind::Colon
                }
                ';' => {
                    i += 1;
                    TokenKind::Semicolon
                }
                _ => return Err(anyhow!("unexpected character: {}", c)),
            };
            toks.push(Token { kind });
        }
        toks.push(Token { kind: TokenKind::Eof });
        Ok(toks)
    }
}
