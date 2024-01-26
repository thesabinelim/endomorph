use crate::parser_combinators::{
    parser::{Eof, EofError, Just, JustError},
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
fn describe_just_it_works() {
    assert_eq!(Just('a').parse("a"), Ok(('a', "")));
}

#[test]
fn describe_just_it_errors_on_mismatch() {
    assert_eq!(
        Just('a').parse("b"),
        Err(ParseError {
            recoverable: true,
            inner_error: JustError::Mismatch
        })
    );
}

#[test]
fn describe_just_it_errors_on_eof() {
    assert_eq!(
        Just('a').parse(""),
        Err(ParseError {
            recoverable: true,
            inner_error: JustError::Eof
        })
    );
}
