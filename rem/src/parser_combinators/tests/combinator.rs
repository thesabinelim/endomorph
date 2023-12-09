use crate::{
    parser_combinators::{combinator::choice, parser::single, tests::TestToken},
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
