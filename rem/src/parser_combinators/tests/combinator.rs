use crate::{
    parser_combinators::{
        combinator::{choice, produce, sequence, some},
        tests::{test_token, TestToken},
    },
    test_tokens,
};

#[test]
fn describe_choice_it_works() {
    let inner_parsers = vec![
        test_token(TestToken::A),
        test_token(TestToken::B),
        test_token(TestToken::C),
    ];
    let parser = choice(inner_parsers);
    assert!(parser(test_tokens![TestToken::A]).is_ok_and(|production| production == TestToken::A));
    assert!(parser(test_tokens![TestToken::B]).is_ok_and(|production| production == TestToken::B));
    assert!(parser(test_tokens![TestToken::C]).is_ok_and(|production| production == TestToken::C));
}

#[test]
fn describe_choice_it_errors_on_all_inner_parser_error() {
    let inner_parsers = vec![
        test_token(TestToken::A),
        test_token(TestToken::B),
        test_token(TestToken::C),
    ];
    let parser = choice(inner_parsers);
    assert!(parser(test_tokens![TestToken::D]).is_err());
}

#[test]
fn describe_produce_it_works() {
    let inner_parser = test_token(TestToken::A);
    let parser = produce(TestToken::B, inner_parser);
    assert!(parser(test_tokens![TestToken::A]).is_ok_and(|production| production == TestToken::B));
}

#[test]
fn describe_produce_it_errors_on_inner_parser_error() {
    let inner_parser = test_token(TestToken::A);
    let parser = produce(TestToken::A, inner_parser);
    assert!(parser(test_tokens![]).is_err());
}

#[test]
fn describe_sequence_it_works() {
    let inner_parsers = vec![
        test_token(TestToken::A),
        test_token(TestToken::B),
        test_token(TestToken::C),
    ];
    let parser = sequence(inner_parsers);
    assert!(
        parser(test_tokens![TestToken::A, TestToken::B, TestToken::C])
            .is_ok_and(|productions| productions == vec![TestToken::A, TestToken::B, TestToken::C])
    );
}

#[test]
fn describe_sequence_it_errors_on_inner_parser_error() {
    let inner_parsers = vec![test_token(TestToken::A), test_token(TestToken::B)];
    let parser = sequence(inner_parsers);
    assert!(parser(test_tokens![TestToken::A, TestToken::A]).is_err());
}
