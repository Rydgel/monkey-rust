use nom::*;

pub mod ast;
use lexer::token::*;
use parser::ast::*;
use parser::ast::Expr::*;
use parser::ast::Literal::*;


macro_rules! tag_token (
  ($i: expr, $tag: expr) => (
    {
        let (i1, t1) = try_parse!($i, take!(1));
        if t1.tok.len() == 0 {
            IResult::Incomplete::<_,_,u32>(Needed::Size(1))
        } else {
            if t1.tok[0] == $tag {
                IResult::Done(i1, t1)
            } else {
                IResult::Error(error_position!(ErrorKind::Tag, $i))
            }
        }
    }
  );
);

macro_rules! parse_ident (
  ($i: expr,) => (
    {
        let (i1, t1) = try_parse!($i, take!(1));
        if t1.tok.len() == 0 {
            IResult::Error(error_position!(ErrorKind::Tag, $i))
        } else {
            match t1.tok[0].clone() {
                Token::Ident(name) => IResult::Done(i1, Ident(name)),
                _ => IResult::Error(error_position!(ErrorKind::Tag, $i)),
            }
        }
    }
  );
);

macro_rules! parse_literal (
  ($i: expr,) => (
    {
        let (i1, t1) = try_parse!($i, take!(1));
        if t1.tok.len() == 0 {
            IResult::Error(error_position!(ErrorKind::Tag, $i))
        } else {
            match t1.tok[0].clone() {
                Token::IntLiteral(i) => IResult::Done(i1, IntLiteral(i)),
                Token::BoolLiteral(b) => IResult::Done(i1, BoolLiteral(b)),
                Token::StringLiteral(s) => IResult::Done(i1, StringLiteral(s)),
                _ => IResult::Error(error_position!(ErrorKind::Tag, $i)),
            }
        }
    }
  );
);

fn infix_op(t: Token) -> (Precedence, Option<Infix>) {
    match t {
        Token::Equal => (Precedence::PEquals, Some(Infix::Equal)),
        Token::NotEqual => (Precedence::PEquals, Some(Infix::NotEqual)),
        Token::LessThan => (Precedence::PLessGreater, Some(Infix::LessThan)),
        Token::GreaterThan => (Precedence::PLessGreater, Some(Infix::GreaterThan)),
        Token::Plus => (Precedence::PSum, Some(Infix::Plus)),
        Token::Minus => (Precedence::PSum, Some(Infix::Minus)),
        Token::Multiply => (Precedence::PProduct, Some(Infix::Multiply)),
        Token::Divide => (Precedence::PProduct, Some(Infix::Divide)),
        Token::LParen => (Precedence::PCall, None),
        Token::LBracket => (Precedence::PIndex, None),
        _ => (Precedence::PLowest, None),
    }
}

named!(parse_program<Tokens, Program>,
    do_parse!(
        prog: many0!(parse_stmt) >>
        tag_token!(Token::EOF) >>
        (prog)
    )
);

// todo
named!(parse_expr<Tokens, Expr>,
    do_parse!(
        parse_literal!() >>
        (LitExpr(IntLiteral(1)))
    )
);

named!(parse_stmt<Tokens, Stmt>, alt_complete!(
    parse_let_stmt |
    parse_return_stmt |
    parse_expr_stmt
));

named!(parse_let_stmt<Tokens, Stmt>,
    do_parse!(
        tag_token!(Token::Let) >>
        ident: parse_ident!() >>
        tag_token!(Token::Assign) >>
        expr: parse_expr >>
        opt!(tag_token!(Token::SemiColon)) >>
        (Stmt::LetStmt(ident, expr))
    )
);

named!(parse_return_stmt<Tokens, Stmt>,
    do_parse!(
        tag_token!(Token::Return) >>
        expr: parse_expr >>
        opt!(tag_token!(Token::SemiColon)) >>
        (Stmt::ReturnStmt(expr))
    )
);

named!(parse_expr_stmt<Tokens, Stmt>,
    do_parse!(
        expr: parse_expr >>
        opt!(tag_token!(Token::SemiColon)) >>
        (Stmt::ExprStmt(expr))
    )
);

named!(parse_block_stmt<Tokens, BlockStmt>,
    do_parse!(
        tag_token!(Token::LBrace) >>
        ss: many0!(parse_stmt) >>
        tag_token!(Token::RBrace) >>
        (ss)
    )
);

named!(parse_atom_expr<Tokens, Expr>, alt_complete!(
    parse_lit_expr |
    parse_ident_expr |
    parse_prefix_expr |
    parse_paren_expr |
    parse_array_expr |
    parse_hash_expr |
    parse_if_expr |
    parse_fn_expr
));

named!(parse_paren_expr<Tokens, Expr>,
    do_parse!(
        tag_token!(Token::LParen) >>
        expr: parse_expr >>
        tag_token!(Token::RParen) >>
        (expr)
    )
);

named!(parse_lit_expr<Tokens, Expr>,
    do_parse!(
        lit: parse_literal!() >>
        (Expr::LitExpr(lit))
    )
);

named!(parse_ident_expr<Tokens, Expr>,
    do_parse!(
        ident: parse_ident!() >>
        (Expr::IdentExpr(ident))
    )
);

named!(parse_comma_exprs<Tokens, Box<Expr>>,
    do_parse!(
        tag_token!(Token::Comma) >>
        e: parse_expr >>
        (Box::new(e))
    )
);

named!(parse_exprs<Tokens, Vec<Box<Expr>>>,
    do_parse!(
        e: map!(parse_expr, Box::new) >>
        es: many0!(parse_comma_exprs) >>
        ([&vec!(e)[..], &es[..]].concat())
    )
);

fn empty_boxed_vec(i: Tokens) -> IResult<Tokens, Vec<Box<Expr>>> {
    IResult::Done(i, vec!())
}

named!(parse_array_expr<Tokens, Expr>,
    do_parse!(
        tag_token!(Token::LBracket) >>
        exprs: alt_complete!(parse_exprs | empty_boxed_vec) >>
        tag_token!(Token::RBracket) >>
        (Expr::ArrayExpr(exprs))
    )
);

named!(parse_hash_pair<Tokens, (Literal, Box<Expr>)>,
    do_parse!(
        l: parse_literal!() >>
        tag_token!(Token::Colon) >>
        e: parse_expr >>
        (l, Box::new(e))
    )
);

named!(parse_hash_comma_expr<Tokens, (Literal, Box<Expr>)>,
    do_parse!(
        tag_token!(Token::Comma) >>
        pair: parse_hash_pair >>
        (pair)
    )
);

named!(parse_hash_pairs<Tokens, Vec<(Literal, Box<Expr>)>>,
    do_parse!(
        pair: parse_hash_pair >>
        pairs: many0!(parse_hash_comma_expr) >>
        ([&vec!(pair)[..], &pairs[..]].concat())
    )
);

fn empty_pairs(i: Tokens) -> IResult<Tokens, Vec<(Literal, Box<Expr>)>> {
    IResult::Done(i, vec!())
}

named!(parse_hash_expr<Tokens, Expr>,
    do_parse!(
        tag_token!(Token::LBrace) >>
        pairs: alt_complete!(parse_hash_pairs | empty_pairs) >>
        tag_token!(Token::RBrace) >>
        (Expr::HashExpr(pairs))
    )
);

fn parse_prefix_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (i1, t1) = try_parse!(input, alt_complete!(
        tag_token!(Token::Plus) |
        tag_token!(Token::Minus) |
        tag_token!(Token::Not)
    ));

    if t1.tok.len() == 0 {
        IResult::Error(error_position!(ErrorKind::Tag, input))
    } else {
        let (i2, e) = try_parse!(i1, parse_atom_expr);

        match t1.tok[0].clone() {
            Token::Plus => IResult::Done(i2, Expr::PrefixExpr(Prefix::PrefixPlus, Box::new(e))),
            Token::Minus => IResult::Done(i2, Expr::PrefixExpr(Prefix::PrefixMinus, Box::new(e))),
            Token::Not => IResult::Done(i2, Expr::PrefixExpr(Prefix::Not, Box::new(e))),
            _ => IResult::Error(ErrorKind::Custom(66))
        }
    }
}

named!(parse_if_expr<Tokens, Expr>,
    do_parse!(
        tag_token!(Token::If) >>
        tag_token!(Token::LParen) >>
        expr: parse_expr >>
        tag_token!(Token::RParen) >>
        c: parse_block_stmt >>
        a: parse_else_expr >>
        (Expr::IfExpr { cond: Box::new(expr), consequence: c, alternative: a })
    )
);

named!(parse_else_expr<Tokens, Option<BlockStmt>>,
    opt!(do_parse!(tag_token!(Token::Else) >> b: parse_block_stmt >> (b)))
);

fn empty_params(i: Tokens) -> IResult<Tokens, Vec<Ident>> {
    IResult::Done(i, vec!())
}

named!(parse_fn_expr<Tokens, Expr>,
    do_parse!(
        tag_token!(Token::Function) >>
        tag_token!(Token::LParen) >>
        p: alt_complete!(parse_params | empty_params) >>
        tag_token!(Token::RParen) >>
        b: parse_block_stmt >>
        (Expr::FnExpr { params: p, body: b })
    )
);

named!(parse_params<Tokens, Vec<Ident>>,
    do_parse!(
        p: parse_ident!() >>
        ps: many0!(do_parse!(
            tag_token!(Token::Function) >>
            i: parse_ident!()
            >> (i))
        ) >>
        ([&vec!(p)[..], &ps[..]].concat())
    )
);

pub struct Parser;

impl Parser {
    pub fn parse_tokens(tokens: Tokens) -> Program {
        let parsing = parse_program(tokens);
        println!("{:?}", parsing);
        parsing.to_result().unwrap()
    }
}
