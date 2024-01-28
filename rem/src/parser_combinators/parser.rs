use core::fmt::{Debug, Display};

use super::{ParseResult, Parser, ParserInput};

// TODO: Match, Succeed, Fail

#[derive(Clone, PartialEq)]
pub struct End;

impl<Input> Parser<Input> for End
where
    Input: ParserInput,
{
    type Output = ();

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        (
            match input.next() {
                Some(_) => None,
                None => Some(()),
            },
            input.clone(),
        )
    }
}

#[derive(Clone, PartialEq)]
pub struct Just<Token>(pub Token);

impl<Input> Parser<Input> for Just<Input::Token>
where
    Input: ParserInput,
    Input::Token: Clone + Display + Eq + Debug,
{
    type Output = Input::Token;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        let Just(expected) = self;
        match input.next() {
            Some((actual, next_input)) => {
                if actual == *expected {
                    (Some(actual), next_input)
                } else {
                    (None, input.clone())
                }
            }
            None => (None, input.clone()),
        }
    }
}
