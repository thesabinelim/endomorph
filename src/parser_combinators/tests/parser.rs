use crate::parser_combinators::{
    parser::{end, just, matches, one_of},
    Parser,
};

#[test]
fn describe_end_it_succeeds_on_input_end() {
    assert_eq!(end().parse(&""), (Some(()), ""));
}

#[test]
fn describe_end_it_fails_on_not_input_end() {
    assert_eq!(end().parse(&"a"), (None, "a"));
}

#[test]
fn describe_just_it_succeeds_on_match() {
    assert_eq!(just('a').parse(&"a"), (Some('a'), ""));
}

#[test]
fn describe_just_it_fails_on_mismatch() {
    assert_eq!(just('a').parse(&"b"), (None, "b"));
}

#[test]
fn describe_just_it_fails_on_input_end() {
    assert_eq!(just('a').parse(&""), (None, ""));
}

#[test]
fn describe_matches_it_succeeds_on_match() {
    assert_eq!(matches(|c| c == 'a').parse(&"a"), (Some('a'), ""));
}

#[test]
fn describe_matches_it_fails_on_mismatch() {
    assert_eq!(matches(|c| c == 'a').parse(&"b"), (None, "b"));
}

#[test]
fn describe_matches_it_fails_on_eof() {
    assert_eq!(matches(|c| c == 'a').parse(&""), (None, ""));
}

#[test]
fn describe_one_of_it_succeeds_on_match() {
    let parser = one_of(vec!['a', 'b', 'c']);
    assert_eq!(parser.parse(&"a"), (Some('a'), ""));
    assert_eq!(parser.parse(&"b"), (Some('b'), ""));
    assert_eq!(parser.parse(&"c"), (Some('c'), ""));
}

#[test]
fn describe_one_of_it_fails_on_mismatch() {
    assert_eq!(one_of(vec!['a', 'b', 'c']).parse(&"d"), (None, "d"));
}
