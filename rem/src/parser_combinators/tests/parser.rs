use crate::parser_combinators::parser::{eof, single};

use super::{TestToken, TestTokenStream};

fn run_eof(tokens: Vec<TestToken>) -> Result<(), ()> {
    eof(TestTokenStream::new(tokens))
}

#[test]
fn describe_eof_it_works() {
    let result = run_eof(vec![]);
    assert!(result.is_ok())
}

#[test]
fn describe_eof_it_errors_on_not_eof() {
    let result = run_eof(vec![TestToken::A]);
    assert!(result.is_err())
}

fn run_single(expected: TestToken, tokens: Vec<TestToken>) -> Result<TestToken, ()> {
    single(expected)(TestTokenStream::new(tokens))
}

#[test]
fn describe_single_it_works() {
    let result = run_single(TestToken::A, vec![TestToken::A]);
    assert!(result.is_ok_and(|produced| produced == TestToken::A));
}

#[test]
fn describe_single_it_errors_on_mismatch() {
    let result = run_single(TestToken::A, vec![TestToken::B]);
    assert!(result.is_err());
}

#[test]
fn describe_single_it_errors_on_eof() {
    let result = run_single(TestToken::A, vec![]);
    assert!(result.is_err());
}
