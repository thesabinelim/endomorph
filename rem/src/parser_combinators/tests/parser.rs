use crate::{
    parser_combinators::parser::{eof, single},
    test_tokens,
};

use super::TestToken;

#[test]
fn describe_eof_it_works() {
    let result = eof(test_tokens![]);
    assert!(result.is_ok())
}

#[test]
fn describe_eof_it_errors_on_not_eof() {
    let result = eof(test_tokens![TestToken::A]);
    assert!(result.is_err())
}

#[test]
fn describe_single_it_works() {
    let result = single(TestToken::A)(test_tokens![TestToken::A]);
    assert!(result.is_ok_and(|produced| produced == TestToken::A));
}

#[test]
fn describe_single_it_errors_on_mismatch() {
    let result = single(TestToken::A)(test_tokens![TestToken::B]);
    assert!(result.is_err());
}

#[test]
fn describe_single_it_errors_on_eof() {
    let result = single(TestToken::A)(test_tokens![]);
    assert!(result.is_err());
}
