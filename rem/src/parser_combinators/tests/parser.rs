use crate::parser_combinators::parser::single;

use super::{TestToken, TestTokenStream};

#[test]
fn describe_single_it_works() {
    let tokens = vec![TestToken::A];
    let result = single(TestToken::A)(TestTokenStream::newBox(tokens));
    assert!(result.is_ok_and(|produced| produced == TestToken::A));
}

#[test]
fn describe_single_it_errors_on_mismatch() {
    let tokens = vec![TestToken::A];
    let result = single(TestToken::B)(TestTokenStream::newBox(tokens));
    assert!(result.is_err());
}

#[test]
fn describe_single_it_errors_on_eof() {
    let tokens = vec![];
    let result = single(TestToken::A)(TestTokenStream::newBox(tokens));
    assert!(result.is_err());
}
