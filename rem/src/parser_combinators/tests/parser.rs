use crate::parser_combinators::{
    parser::{Eof, EofError, Single, SingleError},
    ParseError, Parser,
};

#[test]
fn describe_eof_it_works() {
    assert_eq!(Eof.parse(""), Ok(((), "")));
}

#[test]
fn describe_eof_it_errors_on_not_eof() {
    assert_eq!(
        Eof.parse("a"),
        Err(ParseError {
            recoverable: true,
            inner_error: EofError::NotEof
        })
    );
}

#[test]
fn describe_single_it_works() {
    assert_eq!(Single('a').parse("a"), Ok(('a', "")));
}

#[test]
fn describe_single_it_errors_on_mismatch() {
    assert_eq!(
        Single('a').parse("b"),
        Err(ParseError {
            recoverable: true,
            inner_error: SingleError::Mismatch
        })
    );
}

#[test]
fn describe_single_it_errors_on_eof() {
    assert_eq!(
        Single('a').parse(""),
        Err(ParseError {
            recoverable: true,
            inner_error: SingleError::Eof
        })
    );
}
