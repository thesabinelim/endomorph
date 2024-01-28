use crate::parser_combinators::{
    parser::{End, Just},
    Parser,
};

#[test]
fn describe_end_it_succeeds_on_input_end() {
    assert_eq!(End.parse(&""), (Some(()), ""));
}

#[test]
fn describe_end_it_fails_on_not_input_end() {
    assert_eq!(End.parse(&"a"), (None, "a"));
}

#[test]
fn describe_just_it_succeeds_on_match() {
    assert_eq!(Just('a').parse(&"a"), (Some('a'), ""));
}

#[test]
fn describe_just_it_fails_on_mismatch() {
    assert_eq!(Just('a').parse(&"b"), (None, "b"));
}

#[test]
fn describe_just_it_fails_on_input_end() {
    assert_eq!(Just('a').parse(&""), (None, ""));
}
