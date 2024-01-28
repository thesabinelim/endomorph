use crate::parser_combinators::{
    parser::{end, just, matches},
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
    assert_eq!(matches(|c| c == 'a').parse(&"a"), (Some('a'), ""))
}

#[test]
fn describe_matches_it_fails_on_mismatch() {
    assert_eq!(matches(|c| c == 'a').parse(&"b"), (None, "b"))
}

#[test]
fn describe_matches_it_fails_on_eof() {
    assert_eq!(matches(|c| c == 'a').parse(&""), (None, ""))
}
