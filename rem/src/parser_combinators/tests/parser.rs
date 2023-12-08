use crate::parser_combinators::parser::single;

use super::{TestToken, TestTokenStream};

fn run_single(expected: TestToken, tokens: Vec<TestToken>) -> Result<TestToken, ()> {
    single(expected)(Box::new(TestTokenStream::new(tokens)))
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
