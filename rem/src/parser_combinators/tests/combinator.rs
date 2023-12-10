use crate::{
    parser_combinators::{
        combinator::{choice, produce},
        parser::single,
        tests::TestToken,
    },
    test_tokens,
};

#[test]
fn describe_choice_it_works() {
    let parser = choice(vec![
        single(TestToken::A),
        single(TestToken::B),
        single(TestToken::C),
    ]);
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_ok_and(|production| production == TestToken::A));
}

#[test]
fn describe_choice_it_errors_on_mismatch() {
    let parser = choice(vec![
        single(TestToken::A),
        single(TestToken::B),
        single(TestToken::C),
    ]);
    let stream = test_tokens![TestToken::D];
    assert!(parser(stream).is_err());
}

#[test]
fn describe_choice_it_errors_on_eof() {
    let parser = choice(vec![single(TestToken::A)]);
    let stream = test_tokens![];
    assert!(parser(stream).is_err());
}

#[test]
fn describe_produce_it_works() {
    let inner_parser = single(TestToken::A);
    let parser = produce(TestToken::B, inner_parser);
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_ok_and(|production| production == TestToken::B));
}

#[test]
fn describe_produce_it_errors_on_inner_parser_error() {
    let inner_parser = single(TestToken::A);
    let parser = produce(TestToken::B, inner_parser);
    let stream = test_tokens![TestToken::B];
    assert!(parser(stream).is_err());
}
