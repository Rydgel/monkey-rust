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
        Token::LessThanEqual => (Precedence::PLessGreater, Some(Infix::LessThanEqual)),
        Token::GreaterThanEqual => (Precedence::PLessGreater, Some(Infix::GreaterThanEqual)),
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

named!(parse_comma_exprs<Tokens, Expr>,
    do_parse!(
        tag_token!(Token::Comma) >>
        e: parse_expr >>
        (e)
    )
);

named!(parse_exprs<Tokens, Vec<Expr>>,
    do_parse!(
        e: parse_expr >>
        es: many0!(parse_comma_exprs) >>
        ([&vec!(e)[..], &es[..]].concat())
    )
);

fn empty_boxed_vec(i: Tokens) -> IResult<Tokens, Vec<Expr>> {
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

named!(parse_hash_pair<Tokens, (Literal, Expr)>,
    do_parse!(
        l: parse_literal!() >>
        tag_token!(Token::Colon) >>
        e: parse_expr >>
        (l, e)
    )
);

named!(parse_hash_comma_expr<Tokens, (Literal, Expr)>,
    do_parse!(
        tag_token!(Token::Comma) >>
        pair: parse_hash_pair >>
        (pair)
    )
);

named!(parse_hash_pairs<Tokens, Vec<(Literal, Expr)>>,
    do_parse!(
        pair: parse_hash_pair >>
        pairs: many0!(parse_hash_comma_expr) >>
        ([&vec!(pair)[..], &pairs[..]].concat())
    )
);

fn empty_pairs(i: Tokens) -> IResult<Tokens, Vec<(Literal, Expr)>> {
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
        args: alt_complete!(parse_exprs | empty_boxed_vec) >>
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
    use lexer::*;

    fn assert_input_with_program(input: &[u8], expected_results: Program) {
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();
        assert_eq!(result, expected_results);
    }

    fn compare_inputs(input: &[u8], input2: &[u8]) {
        let r = Lexer::lex_tokens(input).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let result = Parser::parse_tokens(tokens).to_result().unwrap();

        let r = Lexer::lex_tokens(input2).to_result().unwrap();
        let tokens = Tokens::new(&r);
        let expected_results = Parser::parse_tokens(tokens).to_result().unwrap();

        assert_eq!(result, expected_results);
    }

    #[test]
    fn empty() {
        assert_input_with_program(&b""[..], vec!());
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

        let program: Program = vec!(
            Stmt::LetStmt(Ident("x".to_owned()), Expr::LitExpr(Literal::IntLiteral(5))),
            Stmt::LetStmt(Ident("y".to_owned()), Expr::LitExpr(Literal::IntLiteral(10))),
            Stmt::LetStmt(Ident("foobar".to_owned()), Expr::LitExpr(Literal::IntLiteral(838383))),
            Stmt::LetStmt(Ident("boo".to_owned()), Expr::LitExpr(Literal::BoolLiteral(true))),
        );

        assert_input_with_program(input, program);
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

        let program: Program = vec!(
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(5))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(10))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(838383))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::BoolLiteral(true))),
        );

        assert_input_with_program(input, program);
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

        let program: Program = vec!(
            Stmt::LetStmt(Ident("x".to_owned()), Expr::LitExpr(Literal::IntLiteral(5))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::IntLiteral(10))),
            Stmt::ExprStmt(Expr::LitExpr(Literal::IntLiteral(15))),
            Stmt::LetStmt(Ident("y".to_owned()), Expr::LitExpr(Literal::IntLiteral(20))),
            Stmt::ReturnStmt(Expr::LitExpr(Literal::BoolLiteral(false))),
        );

        assert_input_with_program(input, program);
    }

    #[test]
    fn identifier() {
        let input =
            "foobar;\
             foobar\
            "
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(Expr::IdentExpr(Ident("foobar".to_owned()))),
            Stmt::ExprStmt(Expr::IdentExpr(Ident("foobar".to_owned()))),
        );

        assert_input_with_program(input, program);
    }

    #[test]
    fn prefix_expr() {
        let input =
            "-foobar;\
             +10\
             !true\
            "
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixMinus, Box::new(Expr::IdentExpr(Ident("foobar".to_owned()))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixPlus, Box::new(Expr::LitExpr(Literal::IntLiteral(10))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::Not, Box::new(Expr::LitExpr(Literal::BoolLiteral(true))))),
        );

        assert_input_with_program(input, program);
    }

    #[test]
    fn prefix_expr2() {
        let input =
            "-(foobar);\
             (+(10));\
             (((!true)));\
            "
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixMinus, Box::new(Expr::IdentExpr(Ident("foobar".to_owned()))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::PrefixPlus, Box::new(Expr::LitExpr(Literal::IntLiteral(10))))),
            Stmt::ExprStmt(Expr::PrefixExpr(Prefix::Not, Box::new(Expr::LitExpr(Literal::BoolLiteral(true))))),
        );

        assert_input_with_program(input, program);
    }

    #[test]
    fn infix_expr() {
        let input =
            "10 + 20"
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::InfixExpr(
                    Infix::Plus,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(10))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(20))),
                )
            ),
        );

        assert_input_with_program(input, program);

        let input =
            "10 * 20"
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::InfixExpr(
                    Infix::Multiply,
                    Box::new(Expr::LitExpr(Literal::IntLiteral(10))),
                    Box::new(Expr::LitExpr(Literal::IntLiteral(20))),
                )
            ),
        );

        assert_input_with_program(input, program);


        let input =
            "10 + 5 / -20 - (x + x)"
            .as_bytes();

        let input2 =
            "10 + (5 / (-20)) - (x + x)"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "10 + 5 / -20 - (x + x)"
            .as_bytes();

        let program: Program = vec!(
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

        assert_input_with_program(input, program);
    }

    #[test]
    fn op_precedence() {
        let input =
            "!-a"
            .as_bytes();

        let input2 =
            "(!(-a))"
            .as_bytes();

        compare_inputs(input, input2);

        let input =
            "a + b + c"
            .as_bytes();

        let input2 =
            "((a + b) + c)"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "a + b - c"
            .as_bytes();

        let input2 =
            "((a + b) - c)"
            .as_bytes();

        compare_inputs(input, input2);



        let input =
            "a * b * c"
            .as_bytes();

        let input2 =
            "((a * b) * c)"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "a * b / c"
            .as_bytes();

        let input2 =
            "((a * b) / c)"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "a + b / c"
            .as_bytes();

        let input2 =
            "(a + (b / c))"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "a + b * c + d / e - f"
            .as_bytes();

        let input2 =
            "(((a + (b * c)) + (d / e)) - f)"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "3 + 4; -5 * 5"
            .as_bytes();

        let input2 =
            "(3 + 4);((-5) * 5)"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "5 > 4 == 3 < 4"
            .as_bytes();

        let input2 =
            "((5 > 4) == (3 < 4))"
            .as_bytes();

        compare_inputs(input, input2);

        let input =
            "5 < 4 != 3 > 4"
            .as_bytes();

        let input2 =
            "((5 < 4) != (3 > 4))"
            .as_bytes();

        compare_inputs(input, input2);

        let input =
            "3 + 4 * 5 == 3 * 1 + 4 * 5"
            .as_bytes();

        let input2 =
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"
            .as_bytes();

        compare_inputs(input, input2);
    }

    #[test]
    fn if_expr() {
        let input =
            "if (x < y) { x }"
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::IfExpr {
                    cond: Box::new(Expr::InfixExpr(
                        Infix::LessThan,
                        Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                        Box::new(Expr::IdentExpr(Ident("y".to_owned()))),
                    )),
                    consequence: vec!(
                        Stmt::ExprStmt(Expr::IdentExpr(Ident("x".to_owned()))),
                    ),
                    alternative: None,
                }
            ),
        );

        assert_input_with_program(input, program);

        let input =
            "if (x < y) { x } else { y }"
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::IfExpr {
                    cond: Box::new(Expr::InfixExpr(
                        Infix::LessThan,
                        Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                        Box::new(Expr::IdentExpr(Ident("y".to_owned()))),
                    )),
                    consequence: vec!(
                        Stmt::ExprStmt(Expr::IdentExpr(Ident("x".to_owned()))),
                    ),
                    alternative: Some(
                        vec!(
                            Stmt::ExprStmt(Expr::IdentExpr(Ident("y".to_owned()))),
                        ),
                    ),
                }
            ),
        );

        assert_input_with_program(input, program);
    }


    #[test]
    fn function_expr() {
        let input =
            "fn() {\
                return foobar + barfoo;\
            }\
            "
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::FnExpr {
                    params: vec!(),
                    body: vec!(
                        Stmt::ReturnStmt(
                            Expr::InfixExpr(
                                Infix::Plus,
                                Box::new(Expr::IdentExpr(Ident("foobar".to_owned()))),
                                Box::new(Expr::IdentExpr(Ident("barfoo".to_owned()))),
                            )
                        ),
                    ),
                }
            ),
        );

        assert_input_with_program(input, program);


        let input =
            "fn(x, y) {\
                return x + y;\
            }\
            "
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::FnExpr {
                    params: vec!(
                        Ident("x".to_owned()),
                        Ident("y".to_owned()),
                    ),
                    body: vec!(
                        Stmt::ReturnStmt(
                            Expr::InfixExpr(
                                Infix::Plus,
                                Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                                Box::new(Expr::IdentExpr(Ident("y".to_owned()))),
                            )
                        ),
                    ),
                }
            ),
        );

        assert_input_with_program(input, program);


        let input =
            "fn() {
                return fn (x, y, z, zz) { return x >= y; };
             }
            "
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::FnExpr {
                    params: vec!(),
                    body: vec!(
                        Stmt::ReturnStmt(
                            Expr::FnExpr {
                                params: vec!(
                                    Ident("x".to_owned()),
                                    Ident("y".to_owned()),
                                    Ident("z".to_owned()),
                                    Ident("zz".to_owned()),
                                ),
                                body: vec!(
                                    Stmt::ReturnStmt(
                                        Expr::InfixExpr(
                                            Infix::GreaterThanEqual,
                                            Box::new(Expr::IdentExpr(Ident("x".to_owned()))),
                                            Box::new(Expr::IdentExpr(Ident("y".to_owned()))),
                                        )
                                    )
                                )
                            }
                        ),
                    ),
                }
            ),
        );

        assert_input_with_program(input, program);
    }

    #[test]
    fn function_call_expr() {
        let input =
            "add(2, 3);\
             add(a, b, 1, 2 * 3, other(4 + 5), add(6, 7 * 8));\
             fn(a, b) { return a + b; }(1, 2);\
            "
            .as_bytes();

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::CallExpr {
                    function: Box::new(Expr::IdentExpr(Ident("add".to_owned()))),
                    arguments: vec!(
                        Expr::LitExpr(Literal::IntLiteral(2)),
                        Expr::LitExpr(Literal::IntLiteral(3)),
                    ),
                }
            ),
            Stmt::ExprStmt(
                Expr::CallExpr {
                    function: Box::new(Expr::IdentExpr(Ident("add".to_owned()))),
                    arguments: vec!(
                        Expr::IdentExpr(Ident("a".to_owned())),
                        Expr::IdentExpr(Ident("b".to_owned())),
                        Expr::LitExpr(Literal::IntLiteral(1)),
                        Expr::InfixExpr(
                            Infix::Multiply,
                            Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                            Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                        ),
                        Expr::CallExpr {
                            function: Box::new(Expr::IdentExpr(Ident("other".to_owned()))),
                            arguments: vec!(
                                Expr::InfixExpr(
                                    Infix::Plus,
                                    Box::new(Expr::LitExpr(Literal::IntLiteral(4))),
                                    Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                                )
                            ),
                        },
                        Expr::CallExpr {
                            function: Box::new(Expr::IdentExpr(Ident("add".to_owned()))),
                            arguments: vec!(
                                Expr::LitExpr(Literal::IntLiteral(6)),
                                Expr::InfixExpr(
                                    Infix::Multiply,
                                    Box::new(Expr::LitExpr(Literal::IntLiteral(7))),
                                    Box::new(Expr::LitExpr(Literal::IntLiteral(8))),
                                ),
                            )
                        },
                    ),
                }
            ),
            Stmt::ExprStmt(
                Expr::CallExpr {
                    function: Box::new(
                        Expr::FnExpr {
                            params: vec!(
                                Ident("a".to_owned()),
                                Ident("b".to_owned()),
                            ),
                            body: vec!(
                                Stmt::ReturnStmt(
                                    Expr::InfixExpr(
                                        Infix::Plus,
                                        Box::new(Expr::IdentExpr(Ident("a".to_owned()))),
                                        Box::new(Expr::IdentExpr(Ident("b".to_owned()))),
                                    )
                                )
                            )
                        }
                    ),
                    arguments: vec!(
                        Expr::LitExpr(Literal::IntLiteral(1)),
                        Expr::LitExpr(Literal::IntLiteral(2)),
                    )
                }
            ),
        );

        assert_input_with_program(input, program);
    }

    #[test]
    fn strings() {
        let input = &b"\"foobar\""[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::LitExpr(
                    Literal::StringLiteral("foobar".to_owned())
                )
            ),
        );

        assert_input_with_program(input, program);

        let input = &b"\"foo bar\""[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::LitExpr(
                    Literal::StringLiteral("foo bar".to_owned())
                )
            ),
        );

        assert_input_with_program(input, program);

        let input = &b"\"foo\nbar\""[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::LitExpr(
                    Literal::StringLiteral("foo\nbar".to_owned())
                )
            ),
        );

        assert_input_with_program(input, program);

        let input = &b"\"foo\tbar\""[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::LitExpr(
                    Literal::StringLiteral("foo\tbar".to_owned())
                )
            ),
        );

        assert_input_with_program(input, program);

        let input = &b"\"foo\\\"bar\""[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::LitExpr(
                    Literal::StringLiteral("foo\"bar".to_owned())
                )
            ),
        );

        assert_input_with_program(input, program);
    }

    #[test]
    fn arrays() {
        let input = &b"[1, 2 * 2, 3 + 3]"[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::ArrayExpr(vec!(
                    Expr::LitExpr(Literal::IntLiteral(1)),
                    Expr::InfixExpr(
                        Infix::Multiply,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(2))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(2)))
                    ),
                    Expr::InfixExpr(
                        Infix::Plus,
                        Box::new(Expr::LitExpr(Literal::IntLiteral(3))),
                        Box::new(Expr::LitExpr(Literal::IntLiteral(3)))
                    ),
                ))
            ),
        );

        assert_input_with_program(input, program);


        let input = &b"myArray[1 + 1]"[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::IndexExpr {
                    array: Box::new(Expr::IdentExpr(Ident("myArray".to_owned()))),
                    index: Box::new(
                        Expr::InfixExpr(
                            Infix::Plus,
                            Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                            Box::new(Expr::LitExpr(Literal::IntLiteral(1)))
                        )
                    ),
                }
            ),
        );

        assert_input_with_program(input, program);
    }


    #[test]
    fn array_precedence() {
        let input =
            "a * [1, 2, 3, 4][b * c] * d"
            .as_bytes();

        let input2 =
            "((a * ([1, 2, 3, 4][b * c])) * d)"
            .as_bytes();

        compare_inputs(input, input2);


        let input =
            "add(a * b[2], b[1], 2 * [1, 2][1])"
            .as_bytes();

        let input2 =
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))"
            .as_bytes();

        compare_inputs(input, input2);
    }


    #[test]
    fn hash() {
        let input = &b"{}"[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::HashExpr(
                    vec!()
                )
            ),
        );

        assert_input_with_program(input, program);


        let input = &b"{\"one\": 1, \"two\": 2, \"three\": 3}"[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::HashExpr(vec!(
                    (
                        Literal::StringLiteral("one".to_owned()),
                        Expr::LitExpr(Literal::IntLiteral(1))
                    ),
                    (
                        Literal::StringLiteral("two".to_owned()),
                        Expr::LitExpr(Literal::IntLiteral(2))
                    ),
                    (
                        Literal::StringLiteral("three".to_owned()),
                        Expr::LitExpr(Literal::IntLiteral(3))
                    ),
                ))
            ),
        );

        assert_input_with_program(input, program);


        let input = &b"{4: 1, 5: 2, 6: 3}"[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::HashExpr(vec!(
                    (
                        Literal::IntLiteral(4),
                        Expr::LitExpr(Literal::IntLiteral(1))
                    ),
                    (
                        Literal::IntLiteral(5),
                        Expr::LitExpr(Literal::IntLiteral(2))
                    ),
                    (
                        Literal::IntLiteral(6),
                        Expr::LitExpr(Literal::IntLiteral(3))
                    ),
                ))
            ),
        );

        assert_input_with_program(input, program);



        let input = &b"{true: 1, false: 2}"[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::HashExpr(vec!(
                    (
                        Literal::BoolLiteral(true),
                        Expr::LitExpr(Literal::IntLiteral(1))
                    ),
                    (
                        Literal::BoolLiteral(false),
                        Expr::LitExpr(Literal::IntLiteral(2))
                    ),
                ))
            ),
        );

        assert_input_with_program(input, program);


        let input = &b"{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15/5}"[..];

        let program: Program = vec!(
            Stmt::ExprStmt(
                Expr::HashExpr(vec!(
                    (
                        Literal::StringLiteral("one".to_owned()),
                        Expr::InfixExpr(
                            Infix::Plus,
                            Box::new(Expr::LitExpr(Literal::IntLiteral(0))),
                            Box::new(Expr::LitExpr(Literal::IntLiteral(1))),
                        )
                    ),
                    (
                        Literal::StringLiteral("two".to_owned()),
                        Expr::InfixExpr(
                            Infix::Minus,
                            Box::new(Expr::LitExpr(Literal::IntLiteral(10))),
                            Box::new(Expr::LitExpr(Literal::IntLiteral(8))),
                        )
                    ),
                    (
                        Literal::StringLiteral("three".to_owned()),
                        Expr::InfixExpr(
                            Infix::Divide,
                            Box::new(Expr::LitExpr(Literal::IntLiteral(15))),
                            Box::new(Expr::LitExpr(Literal::IntLiteral(5))),
                        )
                    ),
                ))
            ),
        );

        assert_input_with_program(input, program);
    }
}
