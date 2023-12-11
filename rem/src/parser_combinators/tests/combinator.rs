use crate::{
    parser_combinators::{
        combinator::{choice, produce, sequence},
        tests::{error, succeed, TestToken},
    },
    test_tokens,
};

#[test]
fn describe_choice_it_works() {
    let inner_parsers = vec![error(()), succeed(TestToken::A)];
    let parser = choice(inner_parsers);
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_ok_and(|production| production == TestToken::A));
}

#[test]
fn describe_choice_it_errors_on_all_inner_parser_error() {
    let inner_parsers = vec![error::<TestToken, (), ()>(())];
    let parser = choice(inner_parsers);
    let stream = test_tokens![TestToken::D];
    assert!(parser(stream).is_err());
}

#[test]
fn describe_produce_it_works() {
    let inner_parser = succeed::<TestToken, TestToken, ()>(TestToken::A);
    let parser = produce(TestToken::B, inner_parser);
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_ok_and(|production| production == TestToken::B));
}

#[test]
fn describe_produce_it_errors_on_inner_parser_error() {
    let inner_parser = error::<TestToken, (), ()>(());
    let parser = produce(TestToken::A, inner_parser);
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_err());
}

#[test]
fn describe_sequence_it_works() {
    let inner_parsers = vec![
        succeed::<TestToken, TestToken, ()>(TestToken::A),
        succeed(TestToken::B),
        succeed(TestToken::C),
    ];
    let parser = sequence(inner_parsers);
    let stream = test_tokens![TestToken::A, TestToken::A, TestToken::A];
    assert!(parser(stream)
        .is_ok_and(|productions| productions == vec![TestToken::A, TestToken::B, TestToken::C]));
}

#[test]
fn describe_sequence_it_errors_on_inner_parser_error() {
    let inner_parsers = vec![succeed::<TestToken, TestToken, ()>(TestToken::A), error(())];
    let parser = sequence(inner_parsers);
    let stream = test_tokens![TestToken::A];
    assert!(parser(stream).is_err());
}
