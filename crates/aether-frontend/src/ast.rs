#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub parent: Option<String>,
}

#[derive(Debug, Clone)]
pub struct StaticVar {
    pub name: String,
    pub ty: Type,
    pub init: Expr,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    Struct(StructDef),
    Static(StaticVar),
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
    User(String),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Expr),
    Expr(Expr),
    Println(String),
    PrintExpr(Expr),
    While { cond: Expr, body: Vec<Stmt> },
    Break,
    Continue,
    Let { name: String, ty: Type, init: Expr },
    Assign { target: Expr, value: Expr },
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
    Field(Box<Expr>, String),
    StructLit(String, Vec<(String, Expr)>),
    MethodCall(Box<Expr>, String, Vec<Expr>),
}
