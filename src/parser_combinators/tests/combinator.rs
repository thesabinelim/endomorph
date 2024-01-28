use crate::{
    parser_combinators::{
        combinator::{or, seq, to},
        parser::just,
        Parser,
    },
    types::list::list,
};

#[test]
fn describe_or_it_succeeds_if_some_inner_parser_succeeds() {
    let parser = or(list![just('a'), just('b'), just('c')]);
    assert_eq!(parser.parse(&"a"), (Some('a'), ""));
    assert_eq!(parser.parse(&"b"), (Some('b'), ""));
    assert_eq!(parser.parse(&"c"), (Some('c'), ""));
}

#[test]
fn describe_or_it_fails_if_all_inner_parsers_fail() {
    assert_eq!(
        or(list![just('a'), just('b'), just('c')]).parse(&"d"),
        (None, "d")
    );
}

#[test]
fn describe_sequence_it_succeeds_if_all_inner_parsers_succeed_in_order() {
    assert_eq!(
        seq(list![just('a'), just('b'), just('c')]).parse(&"abcd"),
        (Some(vec!['a', 'b', 'c']), "d")
    );
}

#[test]
fn describe_sequence_it_fails_if_some_inner_parser_fails() {
    assert_eq!(
        seq(list![just('a'), just('b'), just('c')]).parse(&"abd"),
        (None, "abd")
    );
}

#[test]
fn describe_to_it_overwrites_output_if_inner_parser_succeeds() {
    assert_eq!(
        to("overwritten", just('a')).parse(&"a"),
        (Some("overwritten"), "")
    )
}

#[test]
fn describe_to_it_fails_if_inner_parser_fails() {
    assert_eq!(to("should not appear", just('a')).parse(&"b"), (None, "b"))
}
