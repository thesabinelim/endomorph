use crate::{
    parser_combinators::{
        combinator::{Or, Seq},
        parser::Just,
        Parser,
    },
    types::list::list,
};

#[test]
fn describe_or_it_succeeds_if_some_inner_parser_succeeds() {
    let parser = Or::of(list![Just('a'), Just('b'), Just('c')]);
    assert_eq!(parser.parse(&"a"), (Some('a'), ""));
    assert_eq!(parser.parse(&"b"), (Some('b'), ""));
    assert_eq!(parser.parse(&"c"), (Some('c'), ""));
}

#[test]
fn describe_or_it_fails_if_all_inner_parsers_fail() {
    let parser = Or::of(list![Just('a'), Just('b'), Just('c')]);
    assert_eq!(parser.parse(&"d"), (None, "d"));
}

#[test]
fn describe_sequence_it_works() {
    let parser = Seq::of(list![Just('a'), Just('b'), Just('c')]);
    assert_eq!(parser.parse(&"abcd"), (Some(vec!['a', 'b', 'c']), "d"));
}

#[test]
fn describe_sequence_it_errors_on_inner_parser_error() {
    let parser = Seq::of(list![Just('a'), Just('b'), Just('c')]);
    assert_eq!(parser.parse(&"abd"), (None, "abd"),);
}
