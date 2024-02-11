use crate::{
    parser_combinators::{
        combinator::{catch, map, maybe, or, repeat, seq, to},
        parser::{just, one_of},
        Parser,
    },
    types::list::list,
};

#[test]
fn describe_catch_it_overwrites_output_if_inner_parser_fails() {
    assert_eq!(catch('b', just('a')).parse(&"b"), (Some('b'), "b"));
}

#[test]
fn describe_catch_it_returns_original_output_if_inner_parser_succeeds() {
    assert_eq!(catch('b', just('a')).parse(&"a"), (Some('a'), ""));
}

#[test]
fn describe_map_it_overwrites_output_if_inner_parser_succeeds() {
    let parser = map(
        |c| match c {
            'a' => 1,
            'b' => 2,
            'c' => 3,
            _ => panic!("should not reach this point"),
        },
        one_of(vec!['a', 'b', 'c']),
    );
    assert_eq!(parser.parse(&"a"), (Some(1), ""));
    assert_eq!(parser.parse(&"b"), (Some(2), ""));
    assert_eq!(parser.parse(&"c"), (Some(3), ""));
}

#[test]
fn describe_map_it_fails_if_inner_parser_fails() {
    assert_eq!(
        map(|_| "should not appear", just('a')).parse(&"b"),
        (None, "b")
    );
}

#[test]
fn describe_maybe_it_wraps_output_in_some_if_inner_parser_succeeds() {
    assert_eq!(maybe(just('a')).parse(&"a"), (Some(Some('a')), ""));
}

#[test]
fn describe_maybe_it_outputs_none_if_inner_parser_fails() {
    assert_eq!(maybe(just('a')).parse(&"b"), (Some(None), "b"));
}

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
fn describe_repeat_range_full_it_succeeds_if_inner_parser_succeeds_1_time() {
    assert_eq!(repeat(.., just('a')).parse(&"a"), (Some(vec!['a']), ""));
}

#[test]
fn describe_repeat_range_full_it_succeeds_if_inner_parser_succeeds_several_times() {
    assert_eq!(
        repeat(.., just('a')).parse(&"aaa"),
        (Some(vec!['a', 'a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_full_it_succeeds_if_inner_parser_fails() {
    assert_eq!(repeat(.., just('a')).parse(&"b"), (Some(vec![]), "b"));
}

#[test]
fn describe_repeat_range_from_it_succeeds_if_inner_parser_succeeds_exactly_the_minimum_number_of_times(
) {
    assert_eq!(
        repeat(3.., just('a')).parse(&"aaa"),
        (Some(vec!['a', 'a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_from_it_succeeds_if_inner_parser_succeeds_more_than_the_minimum_number_of_times(
) {
    assert_eq!(
        repeat(1.., just('a')).parse(&"aaa"),
        (Some(vec!['a', 'a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_from_it_fails_if_inner_parser_succeeds_fewer_than_the_minimum_number_of_times(
) {
    assert_eq!(repeat(1.., just('a')).parse(&"b"), (None, "b"));
}

#[test]
fn describe_repeat_range_to_it_succeeds_if_inner_parser_succeeds_less_than_the_maximum_number_of_times(
) {
    assert_eq!(repeat(..2, just('a')).parse(&"b"), (Some(vec![]), "b"));
}

#[test]
fn describe_repeat_range_to_it_succeeds_if_inner_parser_succeeds_exactly_the_maximum_number_of_times(
) {
    assert_eq!(
        repeat(..4, just('a')).parse(&"aaa"),
        (Some(vec!['a', 'a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_to_it_does_not_match_more_than_the_maximum_number_of_times() {
    assert_eq!(
        repeat(..2, just('a')).parse(&"aaa"),
        (Some(vec!['a']), "aa")
    );
}

#[test]
fn describe_repeat_range_to_inclusive_it_succeeds_if_inner_parser_succeeds_less_than_the_maximum_number_of_times(
) {
    assert_eq!(repeat(..=1, just('a')).parse(&"b"), (Some(vec![]), "b"));
}

#[test]
fn describe_repeat_range_to_inclusive_it_succeeds_if_inner_parser_succeeds_exactly_the_maximum_number_of_times(
) {
    assert_eq!(
        repeat(..=3, just('a')).parse(&"aaa"),
        (Some(vec!['a', 'a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_to_inclusive_it_does_not_match_more_than_the_maximum_number_of_times() {
    assert_eq!(
        repeat(..=1, just('a')).parse(&"aaa"),
        (Some(vec!['a']), "aa")
    );
}

#[test]
fn describe_repeat_range_it_succeeds_if_inner_parser_succeeds_exactly_the_minimum_number_of_times()
{
    assert_eq!(repeat(1..4, just('a')).parse(&"a"), (Some(vec!['a']), ""));
}

#[test]
fn describe_repeat_range_it_succeeds_if_inner_parser_succeeds_exactly_the_maximum_number_of_times()
{
    assert_eq!(
        repeat(1..4, just('a')).parse(&"aaa"),
        (Some(vec!['a', 'a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_it_succeeds_if_inner_parser_succeeds_within_the_expected_number_of_times()
{
    assert_eq!(
        repeat(1..4, just('a')).parse(&"aa"),
        (Some(vec!['a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_it_does_not_match_more_than_the_maximum_number_of_times() {
    assert_eq!(
        repeat(1..2, just('a')).parse(&"aaa"),
        (Some(vec!['a']), "aa")
    );
}

#[test]
fn describe_repeat_range_it_fails_if_inner_parser_succeeds_fewer_than_the_minimum_number_of_times()
{
    assert_eq!(repeat(1..2, just('a')).parse(&"b"), (None, "b"));
}

#[test]
fn describe_repeat_range_inclusive_it_succeeds_if_inner_parser_succeeds_exactly_the_minimum_number_of_times(
) {
    assert_eq!(repeat(1..=3, just('a')).parse(&"a"), (Some(vec!['a']), ""));
}

#[test]
fn describe_repeat_range_inclusive_it_succeeds_if_inner_parser_succeeds_exactly_the_maximum_number_of_times(
) {
    assert_eq!(
        repeat(1..=3, just('a')).parse(&"aaa"),
        (Some(vec!['a', 'a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_inclusive_it_succeeds_if_inner_parser_succeeds_within_the_expected_number_of_times(
) {
    assert_eq!(
        repeat(1..=3, just('a')).parse(&"aa"),
        (Some(vec!['a', 'a']), "")
    );
}

#[test]
fn describe_repeat_range_inclusive_it_does_not_match_more_than_the_maximum_number_of_times() {
    assert_eq!(
        repeat(1..=1, just('a')).parse(&"aaa"),
        (Some(vec!['a']), "aa")
    );
}

#[test]
fn describe_repeat_range_inclusive_it_fails_if_inner_parser_succeeds_fewer_than_the_minimum_number_of_times(
) {
    assert_eq!(repeat(1..=1, just('a')).parse(&"b"), (None, "b"));
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
    );
}

#[test]
fn describe_to_it_fails_if_inner_parser_fails() {
    assert_eq!(to("should not appear", just('a')).parse(&"b"), (None, "b"));
}
