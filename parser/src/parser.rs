use nom::bytes::complete::take;
use nom::character::{is_alphanumeric, is_space};
use nom::number::complete::be_u16;
use nom::IResult;

pub fn length_value(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (input, length) = be_u16(input)?;
    take(length)(input)
}

named!(method, take_while1!(is_alphanumeric));
named!(space, take_while1!(|c| c == b' '));
named!(url, take_while1!(|c| c != b' '));
named!(http, tag!("HTTP/"));
named!(
    version,
    take_while1!(|c| c >= b'0' && c <= b'9' || c == b'.')
);
named!(crlf, tag!("\r\n"));
named!(http_version, preceded!(http, version));

// Primitives
fn is_token_char(i: u8) -> bool {
    is_alphanumeric(i) || b"!#$%&'*+-.^_`|~".contains(&i)
}

named!(token, take_while!(is_token_char));
named!(
    message_header<Header>,
    do_parse!(
        name: token
            >> tag!(":")
            >> opt!(take_while!(is_space))
            >> value: take_while!(is_header_value_char)
            >> crlf
            >> (Header {
                name: name,
                value: value
            })
    )
);
named!(
    headers<Vec<Header>>,
    terminated!(many0!(message_header), opt!(crlf))
);
named!(
    http_req<&[u8], (&[u8], &[u8],&[u8], &[u8],&[u8], &[u8], Vec<Header>) >,
    tuple!(method, space, url, space, http_version, crlf, headers)
);

fn is_header_value_char(i: u8) -> bool {
    i == 9 || (i >= 32 && i <= 126) || i >= 160
}

#[derive(PartialEq, Debug, Default)]
pub struct Header<'a> {
    pub name: &'a [u8],
    pub value: &'a [u8],
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_request_line() {
        let req_line = b"GET /pub/WWW/TheProject.html HTTP/1.1\r\nHost: www.w3.org\r\nForwarded: proto:https;for=27.0.0.1:1234;by:proxy\r\n\r\n";
        let request = http_req(req_line);
        let (_, (method, _, url, _, http_version, _, header)) = request.unwrap();
        assert_eq!(b"GET", method);
        assert_eq!(b"/pub/WWW/TheProject.html", url);
        assert_eq!(b"1.1", http_version);
        assert_eq!(vec![
            Header{
                name: b"Host",
                value: b"www.w3.org",
            },
            Header{
                name: b"Forwarded",
                value: b"proto:https;for=27.0.0.1:1234;by:proxy",
            },
        ], header);
    }
}
