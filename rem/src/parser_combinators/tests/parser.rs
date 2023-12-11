use crate::{
    parser_combinators::parser::{eof, single},
    test_tokens,
};

use super::TestToken;

#[test]
fn describe_eof_it_works() {
    let parser = eof();
    let stream = test_tokens![];
    assert!(parser(stream).is_ok())
}

#[test]
fn describe_eof_it_errors_on_not_eof() {
    let parser = eof();
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_err())
}

#[test]
fn describe_single_it_works() {
    let parser = single(TestToken::A);
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_ok_and(|production| production == TestToken::A));
}

#[test]
fn describe_single_it_errors_on_mismatch() {
    let parser = single(TestToken::A);
    let stream = test_tokens![TestToken::B];
    assert!(parser(stream).is_err());
}

#[test]
fn describe_single_it_errors_on_eof() {
    let parser = single(TestToken::A);
    let stream = test_tokens![];
    assert!(parser(stream).is_err());
}
