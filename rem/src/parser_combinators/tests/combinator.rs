use crate::{
    parser_combinators::{
        combinator::{Choice, ChoiceError, Unrecoverable},
        parser::Single,
        ParseError, Parser,
    },
    types::list::list,
};

#[test]
fn describe_choice_it_works() {
    let parser = Choice::of(list![Single('a'), Single('b'), Single('c')]);
    assert_eq!(parser.parse("a"), Ok(('a', "")));
    assert_eq!(parser.parse("b"), Ok(('b', "")));
    assert_eq!(parser.parse("c"), Ok(('c', "")));
}

#[test]
fn describe_choice_it_errors_on_all_inner_parser_error() {
    let parser = Choice::of(list![Single('a'), Single('b'), Single('c')]);
    assert_eq!(
        parser.parse("d"),
        Err(ParseError {
            expected: "a or b or c".to_string(),
            recoverable: true,
            inner_error: ChoiceError::AllFailed
        })
    );
}

#[test]
fn describe_choice_it_errors_on_inner_parser_unrecoverable_error() {
    let parser = Choice::of(list![
        Single('a'),
        Unrecoverable::of(Single('b')),
        Single('c')
    ]);
    assert_eq!(
        parser.parse("c"),
        Err(ParseError {
            expected: "b".to_string(),
            recoverable: false,
            inner_error: ChoiceError::Unrecoverable
        })
    );
}

// #[test]
// fn describe_some_it_works_with_no_matches() {
//     let inner_parser = test_match('a');
//     let parser = some(inner_parser);
//     assert!(parser("b").is_ok_and(|ParseSuccess { result, rest }| result == vec![]));
// }

// #[test]
// fn describe_some_it_works_with_one_match() {
//     let inner_parser = test_match('a');
//     let parser = some(inner_parser);
//     assert!(parser(TestTokenStream::from("aba"))
//         .is_ok_and(|(_, productions)| productions == vec![TestToken::A]));
// }

// #[test]
// fn describe_some_it_works_with_several_matches() {
//     let inner_parser = test_match('a');
//     let parser = some(inner_parser);
//     assert!(parser(TestTokenStream::from("aaaba")).is_ok_and(
//         |(_, productions)| productions == vec![TestToken::A, TestToken::A, TestToken::A]
//     ));
// }

// #[test]
// fn describe_produce_it_works() {
//     let inner_parser = test_match('a');
//     let parser = produce(TestToken::B, inner_parser);
//     assert!(
//         parser(TestTokenStream::from("a")).is_ok_and(|(_, production)| production == TestToken::B)
//     );
// }

// #[test]
// fn describe_produce_it_errors_on_inner_parser_error() {
//     let inner_parser = test_match('a');
//     let parser = produce(TestToken::A, inner_parser);
//     assert!(parser(TestTokenStream::from("b")).is_err());
// }

// #[test]
// fn describe_sequence_it_works() {
//     let inner_parsers = test_matches("abc");
//     let parser = sequence(inner_parsers);
//     assert!(parser(TestTokenStream::from("abc")).is_ok_and(
//         |(_, productions)| productions == vec![TestToken::A, TestToken::B, TestToken::C]
//     ));
// }

// #[test]
// fn describe_sequence_it_errors_on_inner_parser_error() {
//     let inner_parsers = test_matches("ab");
//     let parser = sequence(inner_parsers);
//     assert!(parser(TestTokenStream::from("aa")).is_err());
// }
