use crate::types::list::list;

#[test]
fn describe_list_eq_it_returns_true_when_both_empty() {
    assert_eq!(list![], list![])
}

#[test]
fn describe_list_eq_it_returns_false_with_different_lengths() {
    assert_ne!(list![1, 2, 3], list![1, 2, 3, 4])
}

#[test]
fn describe_list_eq_it_returns_true_with_matching_elements() {
    assert_eq!(list![1, 2, 3], list![1, 2, 3])
}

#[test]
fn describe_list_eq_it_returns_false_with_mismatching_elements() {
    assert_ne!(list![1, 2, 3], list![3, 2, 1])
}
