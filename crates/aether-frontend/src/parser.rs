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
        loop {
            if p.peek().map(|t| &t.kind) == Some(&TokenKind::Eof) {
                break;
            }
            match p.parse_item()? {
                Some(it) => items.push(it),
                None => break,
            }
        }
        use std::collections::HashSet;
        let mut seen = HashSet::new();
        for it in &items {
            if let Item::Function(f) = it {
                if !seen.insert(f.name.clone()) {
                    return Err(anyhow!("duplicate function name '{}'", f.name));
                }
            }
        }
        Ok(Module { items })
    }

    fn parse_item(&mut self) -> Result<Option<Item>> {
        let mut is_pub = false;
        if self.eat_kind(&TokenKind::Pub) {
            is_pub = true;
        }
        if self.eat_kind(&TokenKind::Func) {
            let f = self.parse_function(is_pub)?;
            return Ok(Some(Item::Function(f)));
        }
        if self.eat_kind(&TokenKind::Struct) {
            let s = self.parse_struct_def()?;
            return Ok(Some(Item::Struct(s)));
        }
        if self.eat_kind(&TokenKind::Static) {
            let s = self.parse_static_var()?;
            return Ok(Some(Item::Static(s)));
        }
        if is_pub {
            return Err(anyhow!("expected Func/Struct/Static after 'pub'"));
        }
        Ok(None)
    }

    fn parse_struct_def(&mut self) -> Result<StructDef> {
        let name = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
            self.pos += 1;
            s
        } else {
            return Err(anyhow!("expected struct name"));
        };
        let mut parent: Option<String> = None;
        if self.eat_kind(&TokenKind::Colon) {
            if let Some(TokenKind::Ident(pn)) = self.peek().map(|t| t.kind.clone()) {
                self.pos += 1;
                parent = Some(pn);
            } else {
                return Err(anyhow!("expected parent name after ':'"));
            }
        }
        self.expect(&TokenKind::LBrace)?;
        let mut fields = Vec::new();
        if !self.eat_kind(&TokenKind::RBrace) {
            loop {
                let fname = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
                    self.pos += 1;
                    s
                } else {
                    return Err(anyhow!("expected field name"));
                };
                self.expect(&TokenKind::Colon)?;
                let fty = self.parse_type()?;
                fields.push(StructField { name: fname, ty: fty });
                if self.eat_kind(&TokenKind::RBrace) {
                    break;
                }
                self.expect(&TokenKind::Comma)?;
            }
        }
        Ok(StructDef { name, fields, parent })
    }

    fn parse_static_var(&mut self) -> Result<StaticVar> {
        let name = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
            self.pos += 1;
            s
        } else {
            return Err(anyhow!("expected static name"));
        };
        self.expect(&TokenKind::Colon)?;
        let ty = self.parse_type()?;
        self.expect(&TokenKind::Assign)?;
        let init = self.parse_expr()?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(StaticVar { name, ty, init })
    }

    fn parse_function(&mut self, is_pub: bool) -> Result<Function> {
        let name = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
            self.pos += 1;
            s
        } else {
            return Err(anyhow!("expected function name"));
        };
        self.expect(&TokenKind::LParen)?;
        let mut params = Vec::new();
        if !self.eat_kind(&TokenKind::RParen) {
            loop {
                let name = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
                    self.pos += 1;
                    s
                } else {
                    return Err(anyhow!("expected parameter name"));
                };
                self.expect(&TokenKind::Colon)?;
                let ty = self.parse_type()?;
                params.push(Param { name, ty });
                if self.eat_kind(&TokenKind::RParen) {
                    break;
                }
                self.expect(&TokenKind::Comma)?;
            }
        }
        self.expect(&TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        self.expect(&TokenKind::LBrace)?;
        let mut body = Vec::new();
        while !self.eat_kind(&TokenKind::RBrace) {
            body.push(self.parse_stmt()?);
        }
        Ok(Function {
            name,
            params,
            ret,
            body,
            is_pub,
            is_threaded: false,
        })
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
            TokenKind::StringType => Type::String,
            TokenKind::Ident(s) => {
                self.pos += 1;
                return Ok(Type::User(s));
            }
            _ => return Err(anyhow!("unexpected token in type")),
        };
        self.pos += 1;
        Ok(ty)
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        if self.eat_kind(&TokenKind::Let) {
            let name = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
                self.pos += 1;
                s
            } else {
                return Err(anyhow!("expected identifier after let"));
            };
            self.expect(&TokenKind::Colon)?;
            let ty = self.parse_type()?;
            self.expect(&TokenKind::Assign)?;
            let init = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;
            return Ok(Stmt::Let { name, ty, init });
        }
        if self.eat_kind(&TokenKind::Println) {
            self.expect(&TokenKind::LParen)?;
            if let Some(TokenKind::String(val)) = self.peek().map(|t| t.kind.clone()) {
                self.pos += 1;
                self.expect(&TokenKind::RParen)?;
                self.expect(&TokenKind::Semicolon)?;
                return Ok(Stmt::Println(val));
            } else {
                let expr = self.parse_expr()?;
                self.expect(&TokenKind::RParen)?;
                self.expect(&TokenKind::Semicolon)?;
                return Ok(Stmt::PrintExpr(expr));
            }
        }
        if self.eat_kind(&TokenKind::While) {
            self.expect(&TokenKind::LParen)?;
            let cond = self.parse_expr()?;
            self.expect(&TokenKind::RParen)?;
            self.expect(&TokenKind::LBrace)?;
            let mut body = Vec::new();
            while !self.eat_kind(&TokenKind::RBrace) {
                body.push(self.parse_stmt()?);
            }
            return Ok(Stmt::While { cond, body });
        }
        if self.eat_kind(&TokenKind::Break) {
            self.expect(&TokenKind::Semicolon)?;
            return Ok(Stmt::Break);
        }
        if self.eat_kind(&TokenKind::Continue) {
            self.expect(&TokenKind::Semicolon)?;
            return Ok(Stmt::Continue);
        }
        if self.eat_kind(&TokenKind::Return) {
            let expr = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;
            return Ok(Stmt::Return(expr));
        }
        let save = self.pos;
        let lval = self.parse_postfix()?;
        if self.eat_kind(&TokenKind::Assign) {
            let value = self.parse_expr()?;
            self.expect(&TokenKind::Semicolon)?;
            return Ok(Stmt::Assign { target: lval, value });
        } else {
            self.pos = save;
        }
        let expr = self.parse_expr()?;
        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Expr(expr))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_cmp()
    }

    fn parse_cmp(&mut self) -> Result<Expr> {
        let mut node = self.parse_add_sub()?;
        loop {
            if self.eat_kind(&TokenKind::Le) {
                let rhs = self.parse_add_sub()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Le, Box::new(rhs));
            } else if self.eat_kind(&TokenKind::Lt) {
                let rhs = self.parse_add_sub()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Lt, Box::new(rhs));
            } else if self.eat_kind(&TokenKind::Eq) {
                let rhs = self.parse_add_sub()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Eq, Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_unary(&mut self) -> Result<Expr> {
        if self.eat_kind(&TokenKind::LParen) {
            let save = self.pos;
            let ty_res = self.parse_type();
            if ty_res.is_ok() && self.eat_kind(&TokenKind::RParen) {
                let inner = self.parse_unary()?;
                return Ok(Expr::Cast(Box::new(inner), ty_res.unwrap()));
            } else {
                self.pos = save;
                let e = self.parse_expr()?;
                self.expect(&TokenKind::RParen)?;
                return Ok(e);
            }
        }
        if self.eat_kind(&TokenKind::Minus) {
            let rhs = self.parse_unary()?;
            return Ok(Expr::BinOp(Box::new(Expr::Lit(Value::Int(0))), BinOpKind::Sub, Box::new(rhs)));
        }
        if self.eat_kind(&TokenKind::Plus) {
            let rhs = self.parse_unary()?;
            return Ok(rhs);
        }
        self.parse_postfix()
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
        let mut node = self.parse_unary()?;
        loop {
            if self.eat_kind(&TokenKind::Star) {
                let rhs = self.parse_postfix()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Mul, Box::new(rhs));
            } else if self.eat_kind(&TokenKind::Slash) {
                let rhs = self.parse_postfix()?;
                node = Expr::BinOp(Box::new(node), BinOpKind::Div, Box::new(rhs));
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_postfix(&mut self) -> Result<Expr> {
        let mut node = self.parse_primary()?;
        loop {
            if self.eat_kind(&TokenKind::Dot) {
                let mname = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
                    self.pos += 1;
                    s
                } else {
                    return Err(anyhow!("expected identifier after '.'"));
                };
                if self.eat_kind(&TokenKind::LParen) {
                    let mut args = Vec::new();
                    if !self.eat_kind(&TokenKind::RParen) {
                        loop {
                            let e = self.parse_expr()?;
                            args.push(e);
                            if self.eat_kind(&TokenKind::RParen) {
                                break;
                            }
                            self.expect(&TokenKind::Comma)?;
                        }
                    }
                    node = Expr::MethodCall(Box::new(node), mname, args);
                } else {
                    node = Expr::Field(Box::new(node), mname);
                }
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_primary(&mut self) -> Result<Expr> {
        if self.eat_kind(&TokenKind::If) {
            let cond = self.parse_expr()?;
            self.expect(&TokenKind::LBrace)?;
            let then_e = self.parse_expr()?;
            self.expect(&TokenKind::RBrace)?;
            self.expect(&TokenKind::Else)?;
            self.expect(&TokenKind::LBrace)?;
            let else_e = self.parse_expr()?;
            self.expect(&TokenKind::RBrace)?;
            return Ok(Expr::IfElse { cond: Box::new(cond), then_expr: Box::new(then_e), else_expr: Box::new(else_e) });
        }
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
        if let Some(TokenKind::Ident(name)) = self.peek().map(|t| t.kind.clone()) {
            if self.toks.get(self.pos + 1).map(|t| t.kind.clone()) == Some(TokenKind::LParen) {
                self.pos += 2;
                let mut args = Vec::new();
                if !self.eat_kind(&TokenKind::RParen) {
                    loop {
                        let e = self.parse_expr()?;
                        args.push(e);
                        if self.eat_kind(&TokenKind::RParen) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                    }
                }
                return Ok(Expr::Call(name, args));
            } else if self.toks.get(self.pos + 1).map(|t| t.kind.clone()) == Some(TokenKind::LBrace) && name.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
                self.pos += 2;
                let mut fields = Vec::new();
                if !self.eat_kind(&TokenKind::RBrace) {
                    loop {
                        let fname = if let Some(TokenKind::Ident(s)) = self.peek().map(|t| t.kind.clone()) {
                            self.pos += 1;
                            s
                        } else {
                            return Err(anyhow!("expected field name in struct literal"));
                        };
                        self.expect(&TokenKind::Colon)?;
                        let fexpr = self.parse_expr()?;
                        fields.push((fname, fexpr));
                        if self.eat_kind(&TokenKind::RBrace) {
                            break;
                        }
                        self.expect(&TokenKind::Comma)?;
                    }
                }
                return Ok(Expr::StructLit(name, fields));
            } else {
                self.pos += 1;
                return Ok(Expr::Var(name));
            }
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
            TokenKind::String(s) => {
                self.pos += 1;
                Ok(Expr::Lit(Value::String(s)))
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
