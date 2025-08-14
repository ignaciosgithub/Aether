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
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Expr),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float64(f64),
    Float32(f32),
    Bool(bool),
    List(Vec<Value>),
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Lit(Value),
    BinOp(Box<Expr>, BinOpKind, Box<Expr>),
}
