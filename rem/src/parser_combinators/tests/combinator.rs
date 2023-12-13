use crate::parser_combinators::{
    combinator::{choice, produce, sequence},
    tests::{test_token, TestToken, TestTokenStream},
};

#[test]
fn describe_choice_it_works() {
    let inner_parsers = vec![
        test_token(TestToken::A),
        test_token(TestToken::B),
        test_token(TestToken::C),
    ];
    let parser = choice(inner_parsers);
    assert!(
        parser(&mut TestTokenStream::from("a")).is_ok_and(|production| production == TestToken::A)
    );
    assert!(
        parser(&mut TestTokenStream::from("b")).is_ok_and(|production| production == TestToken::B)
    );
    assert!(
        parser(&mut TestTokenStream::from("c")).is_ok_and(|production| production == TestToken::C)
    );
}

#[test]
fn describe_choice_it_errors_on_all_inner_parser_error() {
    let inner_parsers = vec![
        test_token(TestToken::A),
        test_token(TestToken::B),
        test_token(TestToken::C),
    ];
    let parser = choice(inner_parsers);
    assert!(parser(&mut TestTokenStream::from("a")).is_err());
}

#[test]
fn describe_produce_it_works() {
    let inner_parser = test_token(TestToken::A);
    let parser = produce(TestToken::B, inner_parser);
    assert!(
        parser(&mut TestTokenStream::from("a")).is_ok_and(|production| production == TestToken::B)
    );
}

#[test]
fn describe_produce_it_errors_on_inner_parser_error() {
    let inner_parser = test_token(TestToken::A);
    let parser = produce(TestToken::A, inner_parser);
    assert!(parser(&mut TestTokenStream::from("b")).is_err());
}

#[test]
fn describe_sequence_it_works() {
    let inner_parsers = vec![
        test_token(TestToken::A),
        test_token(TestToken::B),
        test_token(TestToken::C),
    ];
    let parser = sequence(inner_parsers);
    assert!(parser(&mut TestTokenStream::from("abc"))
        .is_ok_and(|productions| productions == vec![TestToken::A, TestToken::B, TestToken::C]));
}

#[test]
fn describe_sequence_it_errors_on_inner_parser_error() {
    let inner_parsers = vec![test_token(TestToken::A), test_token(TestToken::B)];
    let parser = sequence(inner_parsers);
    assert!(parser(&mut TestTokenStream::from("aa")).is_err());
}
