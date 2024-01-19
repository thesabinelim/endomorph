// use crate::parser_combinators::{combinator::choice, tests::test_match, ParseSuccess};

// #[test]
// fn describe_choice_it_works() {
//     let parser = choice(vec![test_match('a'), test_match('b'), test_match('c')]);
//     assert!(parser("a").is_ok_and(|ParseSuccess { result, rest }| result == 'a'));
//     assert!(parser("b").is_ok_and(|ParseSuccess { result, rest }| result == 'b'));
//     assert!(parser("c").is_ok_and(|ParseSuccess { result, rest }| result == 'c'));
// }

// #[test]
// fn describe_choice_it_errors_on_all_inner_parser_error() {
//     let parser = choice(vec![test_match('a'), test_match('b'), test_match('c')]);
//     assert!(parser("d").is_err());
// }

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
