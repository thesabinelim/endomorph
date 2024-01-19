use crate::parser_combinators::{
    parser::{eof, single, EofError, SingleError},
    ParseError, ParseSuccess, Parser,
};

#[test]
fn describe_eof_it_works() {
    assert_eq!(
        eof().parse(""),
        Ok(ParseSuccess {
            result: (),
            rest: ""
        })
    )
}

#[test]
fn describe_eof_it_errors_on_not_eof() {
    assert_eq!(
        eof().parse("a"),
        Err(ParseError {
            expected: "EOF".to_string(),
            recoverable: true,
            inner_error: EofError::NotEof
        })
    )
}

#[test]
fn describe_single_it_works() {
    assert_eq!(
        single('a').parse("a"),
        Ok(ParseSuccess {
            result: 'a',
            rest: ""
        })
    );
}

#[test]
fn describe_single_it_errors_on_mismatch() {
    assert_eq!(
        single('a').parse("b"),
        Err(ParseError {
            expected: 'a'.to_string(),
            recoverable: true,
            inner_error: SingleError::Mismatch
        })
    );
}

#[test]
fn describe_single_it_errors_on_eof() {
    assert_eq!(
        single('a').parse(""),
        Err(ParseError {
            expected: 'a'.to_string(),
            recoverable: true,
            inner_error: SingleError::Eof
        })
    );
}
