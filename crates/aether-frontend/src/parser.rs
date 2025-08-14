use anyhow::{anyhow, Result};

use crate::ast::*;
use crate::lexer::{Token, TokenKind};

pub struct Parser<'a> {
    toks: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn parse(toks: &'a [Token]) -> Result<Module> {
        let mut p = Parser { toks, pos: 0 };
        let mut items = Vec::new();
        if let Some(func) = p.parse_function()? {
            items.push(Item::Function(func));
        }
        Ok(Module { items })
    }

    fn parse_function(&mut self) -> Result<Option<Function>> {
        let mut is_pub = false;
        if self.eat_kind(&TokenKind::Pub) {
            is_pub = true;
        }
        if !self.eat_kind(&TokenKind::Func) {
            return Ok(None);
        }
        let name = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
            self.pos += 1;
            s
        } else {
            return Err(anyhow!("expected function name"));
        };
        self.expect(&TokenKind::LParen)?;
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        self.expect(&TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.eat_kind(&TokenKind::RBrace) {
            body.push(self.parse_stmt()?);
        }
        Ok(Some(Function {
            name,
            params: vec![],
            ret,
            body,
            is_pub,
            is_threaded: false,
        }))
    }

    fn parse_type(&mut self) -> Result<Type> {
        let kind = self.peek().ok_or_else(|| anyhow!("unexpected eof"))?.kind.clone();
        let ty = match kind {
            TokenKind::Void => Type::Void,
            TokenKind::I32 => Type::I32,
            TokenKind::I64 => Type::I64,
            TokenKind::F32 => Type::F32,
            TokenKind::F64 => Type::F64,
            TokenKind::Any => Type::Any,
            TokenKind::List => {
                self.pos += 1;
                self.expect(&TokenKind::LBracket)?;
                let inner = self.parse_type()?;
                self.expect(&TokenKind::RBracket)?;
                return Ok(Type::List(Box::new(inner)));
            }
            _ => return Err(anyhow!("unexpected token in type")),
        };
        self.pos += 1;
        Ok(ty)
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        if self.eat_kind(&TokenKind::Return) {
            let expr = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;
            return Ok(Stmt::Return(expr));
        }
        let expr = self.parse_expr()?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_add_sub()
    }

    fn parse_add_sub(&mut self) -> Result<Expr> {
        let mut node = self.parse_mul_div()?;
        loop {
            if self.eat_kind(&TokenKind::Plus) {
                let rhs = self.parse_mul_div()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Add, Box::new(rhs));
            } else if self.eat_kind(&TokenKind::Minus) {
                let rhs = self.parse_mul_div()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Sub, Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_mul_div(&mut self) -> Result<Expr> {
        let mut node = self.parse_primary()?;
        loop {
            if self.eat_kind(&TokenKind::Star) {
                let rhs = self.parse_primary()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Mul, Box::new(rhs));
            } else if self.eat_kind(&TokenKind::Slash) {
                let rhs = self.parse_primary()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Div, Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        if self.eat_kind(&TokenKind::LParen) {
            let e = self.parse_expr()?;
            self.expect(&TokenKind::RParen)?;
            return Ok(e);
        }
        if self.eat_kind(&TokenKind::LBracket) {
            let mut elems = Vec::new();
            if !self.eat_kind(&TokenKind::RBracket) {
                loop {
                    let e = self.parse_expr()?;
                    match e {
                        Expr::Lit(v) => elems.push(v),
                        _ => return Err(anyhow!("only literal elements in list for now")),
                    }
                    if self.eat_kind(&TokenKind::RBracket) {
                        break;
                    }
                    self.expect(&TokenKind::Comma)?;
                }
            }
            return Ok(Expr::Lit(Value::List(elems)));
        }
        let kind = self.peek().ok_or_else(|| anyhow!("unexpected eof"))?.kind.clone();
        match kind {
            TokenKind::Int(s) => {
                self.pos += 1;
                let v = s.parse::<i64>()?;
                Ok(Expr::Lit(Value::Int(v)))
            }
            TokenKind::Float(s) => {
                self.pos += 1;
                let v = s.parse::<f64>()?;
                Ok(Expr::Lit(Value::Float64(v)))
            }
            TokenKind::True => {
                self.pos += 1;
                Ok(Expr::Lit(Value::Bool(true)))
            }
            TokenKind::False => {
                self.pos += 1;
                Ok(Expr::Lit(Value::Bool(false)))
            }
            _ => Err(anyhow!("unexpected token in expr")),
        }
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<()> {
        if let Some(t) = self.toks.get(self.pos) {
            if &t.kind == kind {
                self.pos += 1;
                return Ok(());
            }
        }
        Err(anyhow!("expected {:?}", kind))
    }

    fn eat_kind(&mut self, kind: &TokenKind) -> bool {
        if let Some(t) = self.toks.get(self.pos) {
            if &t.kind == kind {
                self.pos += 1;
                return true;
            }
        }
        false
    }

    fn peek(&self) -> Option<&Token> {
        self.toks.get(self.pos)
    }
}
