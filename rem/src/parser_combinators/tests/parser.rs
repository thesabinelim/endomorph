use crate::parser_combinators::parser::single;

use super::{TestToken, TestTokenStream};

#[test]
fn describe_single_it_works() {
    let parser = single(TestToken::A);
    let tokens = vec![TestToken::A];
    let result = parser(Box::new(TestTokenStream::new(tokens)));
    assert!(result.is_ok_and(|produced| produced == TestToken::A));
}

#[test]
fn describe_single_it_errors_on_mismatch() {
    let parser = single(TestToken::A);
    let tokens = vec![TestToken::B];
    let result = parser(Box::new(TestTokenStream::new(tokens)));
    assert!(result.is_err());
}

#[test]
fn describe_single_it_errors_on_eof() {
    let parser = single(TestToken::A);
    let tokens = vec![];
    let result = parser(Box::new(TestTokenStream::new(tokens)));
    assert!(result.is_err());
}
