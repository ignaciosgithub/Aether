#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Param>,
    pub ret: Type,
    pub body: Vec<Stmt>,
    pub is_pub: bool,
    pub is_threaded: bool,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub enum Type {
    Void,
    Bool,
    I32,
    I64,
    F32,
    F64,
    Any,
    List(Box<Type>),
    String,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Expr),
    Expr(Expr),
    Println(String),
    PrintExpr(Expr),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float64(f64),
    Float32(f32),
    Bool(bool),
    List(Vec<Value>),
    String(String),
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Lt,
    Le,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Value),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
    Call(String, Vec<Expr>),
    Var(String),
    Cast(Box<Expr>, Type),
    IfElse { cond: Box<Expr>, then_expr: Box<Expr>, else_expr: Box<Expr> },
}
