use nom::*;
use nom::types::*;
use std::str;
use std::str::FromStr;
use std::str::Utf8Error;

pub mod token;
use lexer::token::*;


// operators
named!(equal_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("==") >> (Token::Equal))
);

named!(not_equal_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("!=") >> (Token::NotEqual))
);

named!(assign_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("=") >> (Token::Assign))
);

named!(plus_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("+") >> (Token::Plus))
);

named!(minus_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("-") >> (Token::Minus))
);

named!(multiply_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("*") >> (Token::Multiply))
);

named!(divide_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("/") >> (Token::Divide))
);

named!(not_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("!") >> (Token::Not))
);

named!(greater_operator_equal<CompleteByteSlice, Token>,
  do_parse!(tag!(">=") >> (Token::GreaterThanEqual))
);

named!(lesser_operator_equal<CompleteByteSlice, Token>,
  do_parse!(tag!("<=") >> (Token::LessThanEqual))
);

named!(greater_operator<CompleteByteSlice, Token>,
  do_parse!(tag!(">") >> (Token::GreaterThan))
);

named!(lesser_operator<CompleteByteSlice, Token>,
  do_parse!(tag!("<") >> (Token::LessThan))
);

named!(lex_operator<CompleteByteSlice, Token>, alt!(
    equal_operator |
    not_equal_operator |
    assign_operator |
    plus_operator |
    minus_operator |
    multiply_operator |
    divide_operator |
    not_operator |
    greater_operator_equal |
    lesser_operator_equal |
    greater_operator |
    lesser_operator
));


// punctuations
named!(comma_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(",") >> (Token::Comma))
);

named!(semicolon_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(";") >> (Token::SemiColon))
);

named!(colon_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(":") >> (Token::Colon))
);

named!(lparen_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("(") >> (Token::LParen))
);

named!(rparen_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(")") >> (Token::RParen))
);

named!(lbrace_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("{") >> (Token::LBrace))
);

named!(rbrace_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("}") >> (Token::RBrace))
);

named!(lbracket_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("[") >> (Token::LBracket))
);

named!(rbracket_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("]") >> (Token::RBracket))
);

named!(lex_punctuations<CompleteByteSlice, Token>, alt!(
    comma_punctuation |
    semicolon_punctuation |
    colon_punctuation |
    lparen_punctuation |
    rparen_punctuation |
    lbrace_punctuation |
    rbrace_punctuation |
    lbracket_punctuation |
    rbracket_punctuation
));

// Strings
fn pis(input: CompleteByteSlice) -> IResult<CompleteByteSlice, Vec<u8>> {
    use std::result::Result::*;

    let (i1, c1) = try_parse!(input, take!(1));
    match c1.as_bytes() {
        b"\"" => Ok((input, vec![])),
        b"\\" => {
            let (i2, c2) = try_parse!(i1, take!(1));
            pis(i2).map(|(slice, done)| (slice, concat_slice_vec(c2.0, done)))
        }
        c => {
            pis(i1).map(|(slice, done)| (slice, concat_slice_vec(c, done)))
        },
    }
}

fn concat_slice_vec(c: &[u8], done: Vec<u8>) -> Vec<u8> {
    let mut new_vec = c.to_vec();
    new_vec.extend(&done);
    new_vec
}

fn convert_vec_utf8(v: Vec<u8>) -> Result<String, Utf8Error> {
    let slice = v.as_slice();
    str::from_utf8(slice).map(|s| s.to_owned())
}

named!(string<CompleteByteSlice, String>,
  delimited!(
    tag!("\""),
    map_res!(pis, convert_vec_utf8),
    tag!("\"")
  )
);

named!(lex_string<CompleteByteSlice, Token>,
    do_parse!(
        s: string >>
        (Token::StringLiteral(s))
    )
);

macro_rules! check(
  ($input:expr, $submac:ident!( $($args:tt)* )) => (
    {
      use std::result::Result::*;
      use nom::{Err,ErrorKind};

      let mut failed = false;
      for &idx in $input.0 {
        if !$submac!(idx, $($args)*) {
            failed = true;
            break;
        }
      }
      if failed {
        let e: ErrorKind<u32> = ErrorKind::Tag;
        Err(Err::Error(error_position!($input, e)))
      } else {
        Ok((&b""[..], $input))
      }
    }
  );
  ($input:expr, $f:expr) => (
    check!($input, call!($f));
  );
);

// Reserved or ident
fn parse_reserved(c: CompleteStr, rest: Option<CompleteStr>) -> Token {
    let mut string = c.0.to_owned();
    string.push_str(rest.unwrap_or(CompleteStr("")).0);
    match string.as_ref() {
        "let" => Token::Let,
        "fn" => Token::Function,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "true" => Token::BoolLiteral(true),
        "false" => Token::BoolLiteral(false),
        _ => Token::Ident(string),
    }
}

fn complete_byte_slice_str_from_utf8(c: CompleteByteSlice) -> Result<CompleteStr, Utf8Error> {
    str::from_utf8(c.0).map(|s| CompleteStr(s))
}

named!(take_1_char<CompleteByteSlice, CompleteByteSlice>,
    flat_map!(take!(1), check!(is_alphabetic))
);

named!(lex_reserved_ident<CompleteByteSlice, Token>,
    do_parse!(
        c: map_res!(call!(take_1_char), complete_byte_slice_str_from_utf8) >>
        rest: opt!(complete!(map_res!(alphanumeric, complete_byte_slice_str_from_utf8))) >>
        (parse_reserved(c, rest))
    )
);

fn complete_str_from_str<F: FromStr>(c: CompleteStr) -> Result<F, F::Err> {
    FromStr::from_str(c.0)
}

// Integers parsing
named!(lex_integer<CompleteByteSlice, Token>,
    do_parse!(
        i: map_res!(map_res!(digit, complete_byte_slice_str_from_utf8), complete_str_from_str) >>
        (Token::IntLiteral(i))
    )
);

// Illegal tokens
named!(lex_illegal<CompleteByteSlice, Token>,
    do_parse!(take!(1) >> (Token::Illegal))
);

named!(lex_token<CompleteByteSlice, Token>, alt_complete!(
    lex_operator |
    lex_punctuations |
    lex_string |
    lex_reserved_ident |
    lex_integer |
    lex_illegal
));

named!(lex_tokens<CompleteByteSlice, Vec<Token>>, ws!(many0!(lex_token)));


pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(bytes: &[u8]) -> IResult<CompleteByteSlice, Vec<Token>> {
        lex_tokens(CompleteByteSlice(bytes)).map(|(slice, result)|
            (slice, [&result[..], &vec![Token::EOF][..]].concat())
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer1() {
        let input = &b"=+(){},;"[..];
        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::Assign,
            Token::Plus,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::SemiColon,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn test_lexer2() {
        let input = "let five = 5;\
             let ten = 10;\

             let add = fn(x, y) {\
                 x + y;\
             };\

             let result = add(five, ten);"
            .as_bytes();

        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::Let,
            Token::Ident("five".to_owned()),
            Token::Assign,
            Token::IntLiteral(5),
            Token::SemiColon,
            Token::Let,
            Token::Ident("ten".to_owned()),
            Token::Assign,
            Token::IntLiteral(10),
            Token::SemiColon,
            Token::Let,
            Token::Ident("add".to_owned()),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident("x".to_owned()),
            Token::Comma,
            Token::Ident("y".to_owned()),
            Token::RParen,
            Token::LBrace,
            Token::Ident("x".to_owned()),
            Token::Plus,
            Token::Ident("y".to_owned()),
            Token::SemiColon,
            Token::RBrace,
            Token::SemiColon,
            Token::Let,
            Token::Ident("result".to_owned()),
            Token::Assign,
            Token::Ident("add".to_owned()),
            Token::LParen,
            Token::Ident("five".to_owned()),
            Token::Comma,
            Token::Ident("ten".to_owned()),
            Token::RParen,
            Token::SemiColon,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn test_lexer3() {
        let input = "if (a == 10) {\
                return a;\
             } else if (a != 20) {\
                return !a;\
            } else if (a > 20) {\
                return -30 / 40 * 50;\
            } else if (a < 30) {\
                return true;\
            }\
            return false;\
            "
            .as_bytes();

        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::Equal,
            Token::IntLiteral(10),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Ident("a".to_owned()),
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::NotEqual,
            Token::IntLiteral(20),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Not,
            Token::Ident("a".to_owned()),
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::GreaterThan,
            Token::IntLiteral(20),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::Minus,
            Token::IntLiteral(30),
            Token::Divide,
            Token::IntLiteral(40),
            Token::Multiply,
            Token::IntLiteral(50),
            Token::SemiColon,
            Token::RBrace,
            Token::Else,
            Token::If,
            Token::LParen,
            Token::Ident("a".to_owned()),
            Token::LessThan,
            Token::IntLiteral(30),
            Token::RParen,
            Token::LBrace,
            Token::Return,
            Token::BoolLiteral(true),
            Token::SemiColon,
            Token::RBrace,
            Token::Return,
            Token::BoolLiteral(false),
            Token::SemiColon,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn string_literals() {
        let (_, result) = Lexer::lex_tokens(&b"\"foobar\""[..]).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foobar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(&b"\"foo bar\""[..]).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo bar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(&b"\"foo\nbar\""[..]).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\nbar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(&b"\"foo\tbar\""[..]).unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\tbar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(&b"\"foo\\\"bar\""[..])
            .unwrap();
        assert_eq!(result, vec![Token::StringLiteral("foo\"bar".to_owned()), Token::EOF]);

        let (_, result) = Lexer::lex_tokens(&b"\"foo\\\"bar with \xf0\x9f\x92\x96 emojis\""[..])
            .unwrap();
        assert_eq!(result,
            vec![Token::StringLiteral("foo\"bar with 💖 emojis".to_owned()), Token::EOF]);
    }

    #[test]
    fn id_with_numbers() {
        let (_, result) = Lexer::lex_tokens(&b"hello2 hel301oo120"[..])
            .unwrap();
        let expected = vec![
            Token::Ident("hello2".to_owned()),
            Token::Ident("hel301oo120".to_owned()),
            Token::EOF,
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn array_tokens() {
        let (_, result) = Lexer::lex_tokens(&b"[1, 2];"[..]).unwrap();
        let expected = vec![
            Token::LBracket,
            Token::IntLiteral(1),
            Token::Comma,
            Token::IntLiteral(2),
            Token::RBracket,
            Token::SemiColon,
            Token::EOF
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn hash_tokens() {
        let (_, result) = Lexer::lex_tokens(&b"{\"hello\": \"world\"}"[..])
            .unwrap();
        let expected = vec![
            Token::LBrace,
            Token::StringLiteral("hello".to_owned()),
            Token::Colon,
            Token::StringLiteral("world".to_owned()),
            Token::RBrace,
            Token::EOF,
        ];
        assert_eq!(result, expected);
    }
}
