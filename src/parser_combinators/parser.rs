use std::marker::PhantomData;

use super::{ParseResult, Parser, ParserInput};

// TODO: Succeed, Fail

pub fn end() -> End {
    End
}

#[derive(Clone)]
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

pub fn just<Token>(expected: Token) -> Just<Token>
where
    Token: PartialEq,
{
    Just(expected)
}

#[derive(Clone)]
pub struct Just<Token>(pub Token);

impl<Input> Parser<Input> for Just<Input::Token>
where
    Input: ParserInput,
    Input::Token: PartialEq,
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

pub fn matches<Token, Predicate>(predicate: Predicate) -> Matches<Token, Predicate>
where
    Predicate: Fn(Token) -> bool + Clone,
{
    Matches::of(predicate)
}

#[derive(Clone)]
pub struct Matches<Token, Predicate>
where
    Predicate: Fn(Token) -> bool + Clone,
{
    predicate: Predicate,
    token: PhantomData<Token>,
}

impl<Token, Predicate> Matches<Token, Predicate>
where
    Predicate: Fn(Token) -> bool + Clone,
{
    pub fn of(predicate: Predicate) -> Self {
        Matches {
            predicate,
            token: PhantomData,
        }
    }
}

impl<Input, Predicate> Parser<Input> for Matches<Input::Token, Predicate>
where
    Input: ParserInput,
    Input::Token: PartialEq,
    Predicate: Fn(Input::Token) -> bool + Clone,
{
    type Output = Input::Token;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        match input.next() {
            Some((token, next_input)) => {
                if (self.predicate)(token.clone()) {
                    (Some(token), next_input)
                } else {
                    (None, input.clone())
                }
            }
            None => (None, input.clone()),
        }
    }
}
