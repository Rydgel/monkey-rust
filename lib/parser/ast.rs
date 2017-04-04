pub type Program = BlockStmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Stmt {
    LetStmt(Ident, Expr),
    ReturnStmt(Expr),
    ExprStmt(Expr),
}

pub type BlockStmt = Vec<Stmt>;

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    IdentExpr(Ident),
    LitExpr(Literal),
    PrefixExpr(Prefix, Box<Expr>),
    InfixExpr(Infix, Box<Expr>, Box<Expr>),
    IfExpr { cond: Box<Expr>, consequence: BlockStmt, alternative: Option<BlockStmt> },
    FnExpr { params: Vec<Ident>, body: BlockStmt },
    CallExpr { function: Box<Expr>, arguments: Vec<Box<Expr>> },
    ArrayExpr(Vec<Box<Expr>>),
    HashExpr(Vec<(Literal, Box<Expr>)>),
    IndexExpr { array: Box<Expr>, index: Box<Expr> },
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    IntLiteral(usize),
    BoolLiteral(bool),
    StringLiteral(String),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, Debug, Clone)]
pub enum Prefix {
    PrefixPlus,
    PrefixMinus,
    Not,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Infix {
    Plus,
    Minus,
    Divide,
    Multiply,
    Equal,
    NotEqual,
    GreaterThanEqual,
    LessThanEqual,
    GreaterThan,
    LessThan,
}

#[derive(PartialEq, PartialOrd, Debug, Clone)]
pub enum Precedence {
    PLowest,
    PEquals,
    PLessGreater,
    PSum,
    PProduct,
    PCall,
    PIndex,
}
