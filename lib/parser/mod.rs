use nom::*;

pub mod ast;
use lexer::token::*;
use parser::ast::*;
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
                IResult::Error(error_position!(ErrorKind::Count, $i))
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

named!(parse_expr<Tokens, Expr>,
    apply!(parse_pratt_expr, Precedence::PLowest)
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
        o: opt!(tag_token!(Token::SemiColon)) >>
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

fn parse_pratt_expr(input: Tokens, precedence: Precedence) -> IResult<Tokens, Expr> {
    do_parse!(input,
        left: parse_atom_expr >>
        i: apply!(go_parse_pratt_expr, precedence, left) >>
        (i)
    )
}

fn go_parse_pratt_expr(input: Tokens, precedence: Precedence, left: Expr) -> IResult<Tokens, Expr> {
    let (i1, t1) = try_parse!(input, take!(1));
    if t1.tok.len() == 0 {
        IResult::Done(i1, left)
    } else {
        let preview = t1.tok[0].clone();
        match infix_op(preview) {
            (Precedence::PCall, _) if precedence < Precedence::PCall => {
                let (i2, left2) = try_parse!(input, apply!(parse_call_expr, left));
                go_parse_pratt_expr(i2, precedence, left2)
            },
            (Precedence::PIndex, _) if precedence < Precedence::PIndex => {
                let (i2, left2) = try_parse!(input, apply!(parse_index_expr, left));
                go_parse_pratt_expr(i2, precedence, left2)
            },
            (ref peek_precedence, _) if precedence < *peek_precedence => {
                let (i2, left2) = try_parse!(input, apply!(parse_infix_expr, left));
                go_parse_pratt_expr(i2, precedence, left2)
            },
            _ => IResult::Done(input, left)
        }
    }
}

fn parse_infix_expr(input: Tokens, left: Expr) -> IResult<Tokens, Expr> {
    let (i1, t1) = try_parse!(input, take!(1));
    if t1.tok.len() == 0 {
        IResult::Error(error_position!(ErrorKind::Tag, input))
    } else {
        let next = t1.tok[0].clone();
        let (precedence, maybe_op) = infix_op(next);
        match maybe_op {
            None => IResult::Error(error_position!(ErrorKind::Tag, input)),
            Some(op) => {
                let (i2, right) = try_parse!(i1, apply!(parse_pratt_expr, precedence));
                IResult::Done(i2, Expr::InfixExpr(op, Box::new(left), Box::new(right)))
            },
        }
    }
}

fn parse_call_expr(input: Tokens, fn_handle: Expr) -> IResult<Tokens, Expr> {
    do_parse!(input,
        tag_token!(Token::LParen) >>
        args: parse_exprs >>
        tag_token!(Token::RParen) >>
        (Expr::CallExpr { function: Box::new(fn_handle), arguments: args })
    )
}

fn parse_index_expr(input: Tokens, arr: Expr) -> IResult<Tokens, Expr> {
    do_parse!(input,
        tag_token!(Token::LBracket) >>
        idx: parse_expr >>
        tag_token!(Token::RBracket) >>
        (Expr::IndexExpr { array: Box::new(arr), index: Box::new(idx) })
    )
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
            tag_token!(Token::Comma) >>
            i: parse_ident!()
            >> (i))
        ) >>
        ([&vec!(p)[..], &ps[..]].concat())
    )
);

pub struct Parser;

impl Parser {
    pub fn parse_tokens(tokens: Tokens) -> IResult<Tokens, Program> {
        parse_program(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::token::*;
    use lexer::*;

    #[test]
    fn empty() {
        let input = &b""[..];
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!();
        assert_eq!(result, expected_results);
    }

    #[test]
    fn let_statements() {
        let input =
            "let x = 5;\
             let y = 10;\
             let foobar = 838383;\
             let boo = true;\
            "
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::LetStmt(Ident("x".to_owned()), Expr::LitExpr(Literal::IntLiteral(5))),
            Stmt::LetStmt(Ident("y".to_owned()), Expr::LitExpr(Literal::IntLiteral(10))),
            Stmt::LetStmt(Ident("foobar".to_owned()), Expr::LitExpr(Literal::IntLiteral(838383))),
            Stmt::LetStmt(Ident("boo".to_owned()), Expr::LitExpr(Literal::BoolLiteral(true))),
        );
        assert_eq!(result, expected_results);
    }

    #[test]
    fn return_statements() {
        let input =
            "return 5;\
             return 10;\
             return 838383;\
             return true;\
            "
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(5))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(10))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(838383))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::BoolLiteral(true))),
        );
        assert_eq!(result, expected_results);
    }

    #[test]
    fn some_statements() {
        let input =
            "let x = 5;\
             return 10;\
             15;\
             let y = 20;\
             return false;\
            "
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::LetStmt(Ident("x".to_owned()), Expr::LitExpr(Literal::IntLiteral(5))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(10))),
            Stmt::ExprStmt(Expr::LitExpr(Literal::IntLiteral(15))),
            Stmt::LetStmt(Ident("y".to_owned()), Expr::LitExpr(Literal::IntLiteral(20))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::BoolLiteral(false))),
        );
        assert_eq!(result, expected_results);
    }

    #[test]
    fn identifier() {
        let input =
            "foobar;\
             foobar\
            "
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::ExprStmt(Expr::IdentExpr(Ident("foobar".to_owned()))),
            Stmt::ExprStmt(Expr::IdentExpr(Ident("foobar".to_owned()))),
        );
        assert_eq!(result, expected_results);
    }

    #[test]
    fn prefix_expr() {
        let input =
            "-foobar;\
             +10\
             !true\
            "
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixMinus, Box::new(Expr::IdentExpr(Ident("foobar".to_owned()))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixPlus, Box::new(Expr::LitExpr(Literal::IntLiteral(10))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::Not, Box::new(Expr::LitExpr(Literal::BoolLiteral(true))))),
        );
        assert_eq!(result, expected_results);
    }

    #[test]
    fn prefix_expr2() {
        let input =
            "-(foobar);\
             (+(10));\
             (((!true)));\
            "
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixMinus, Box::new(Expr::IdentExpr(Ident("foobar".to_owned()))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixPlus, Box::new(Expr::LitExpr(Literal::IntLiteral(10))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::Not, Box::new(Expr::LitExpr(Literal::BoolLiteral(true))))),
        );
        assert_eq!(result, expected_results);
    }

    #[test]
    fn infix_expr() {
        let input =
            "10 + 20"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::ExprStmt(
                Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(10))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(20))),
                )
            ),
        );
        assert_eq!(result, expected_results);

        let input =
            "10 * 20"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::ExprStmt(
                Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(10))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(20))),
                )
            ),
        );
        assert_eq!(result, expected_results);


        let input =
            "10 + 5 / -20 - (x + x)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "10 + (5 / (-20)) - (x + x)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "10 + 5 / -20 - (x + x)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        let expected_results: Program = vec!(
            Stmt::ExprStmt(
                Expr::InfixExpr(
                    Infix::Minus,
                    Box::new(Expr::InfixExpr(
                        Infix::Plus,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(10))),
                        Box::new(Expr::InfixExpr(
                            Infix::Divide,
                            Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                            Box::new(Expr::PrefixExpr(
                                Prefix::PrefixMinus,
                                Box::new(Expr::LitExpr(Literal::IntLiteral(20)))
                            ))
                        )),
                    )),
                    Box::new(Expr::InfixExpr(
                        Infix::Plus,
                        Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                        Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                    )),
                )
            ),
        );
        assert_eq!(result, expected_results);
    }

    #[test]
    fn op_precedence() {
        let input =
            "!-a"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "(!(-a))"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);

        let input =
            "a + b + c"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "((a + b) + c)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "a + b - c"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "((a + b) - c)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);



        let input =
            "a * b * c"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "((a * b) * c)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "a * b / c"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "((a * b) / c)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "a + b / c"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "(a + (b / c))"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "a + b * c + d / e - f"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "(((a + (b * c)) + (d / e)) - f)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "3 + 4; -5 * 5"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "(3 + 4);((-5) * 5)"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "5 > 4 == 3 < 4"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "((5 > 4) == (3 < 4))"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "5 < 4 != 3 > 4"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "((5 < 4) != (3 > 4))"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);


        let input =
            "3 + 4 * 5 == 3 * 1 + 4 * 5"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let input =
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
            .as_bytes();
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);
    }
}
