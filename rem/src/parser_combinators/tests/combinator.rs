use crate::parser_combinators::{
    combinator::{choice, produce, sequence, some},
    tests::{test_match, test_matches, TestToken, TestTokenStream},
};

#[test]
fn describe_choice_it_works() {
    let inner_parsers = test_matches("abc");
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
    let inner_parsers = test_matches("abc");
    let parser = choice(inner_parsers);
    assert!(parser(&mut TestTokenStream::from("d")).is_err());
}

#[test]
fn describe_some_it_works_with_no_matches() {
    let inner_parser = test_match('a');
    let parser = some(inner_parser);
    assert!(parser(&mut TestTokenStream::from("b")).is_ok_and(|productions| productions == vec![]));
}

#[test]
fn describe_some_it_works_with_one_match() {
    let inner_parser = test_match('a');
    let parser = some(inner_parser);
    assert!(parser(&mut TestTokenStream::from("aba"))
        .is_ok_and(|productions| productions == vec![TestToken::A]));
}

#[test]
fn describe_some_it_works_with_several_matches() {
    let inner_parser = test_match('a');
    let parser = some(inner_parser);
    assert!(parser(&mut TestTokenStream::from("aaaba"))
        .is_ok_and(|productions| productions == vec![TestToken::A, TestToken::A, TestToken::A]));
}

#[test]
fn describe_produce_it_works() {
    let inner_parser = test_match('a');
    let parser = produce(TestToken::B, inner_parser);
    assert!(
        parser(&mut TestTokenStream::from("a")).is_ok_and(|production| production == TestToken::B)
    );
}

#[test]
fn describe_produce_it_errors_on_inner_parser_error() {
    let inner_parser = test_match('a');
    let parser = produce(TestToken::A, inner_parser);
    assert!(parser(&mut TestTokenStream::from("b")).is_err());
}

#[test]
fn describe_sequence_it_works() {
    let inner_parsers = test_matches("abc");
    let parser = sequence(inner_parsers);
    assert!(parser(&mut TestTokenStream::from("abc"))
        .is_ok_and(|productions| productions == vec![TestToken::A, TestToken::B, TestToken::C]));
}

#[test]
fn describe_sequence_it_errors_on_inner_parser_error() {
    let inner_parsers = test_matches("ab");
    let parser = sequence(inner_parsers);
    assert!(parser(&mut TestTokenStream::from("aa")).is_err());
}
