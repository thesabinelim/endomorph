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
    Token: Eq,
{
    Just(expected)
}

#[derive(Clone)]
pub struct Just<Token>(pub Token)
where
    Token: Eq;

impl<Input> Parser<Input> for Just<Input::Token>
where
    Input: ParserInput,
    Input::Token: Eq,
{
    type Output = Input::Token;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        match input.next() {
            Some((actual, next_input)) => {
                let Just(expected) = self;
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
    Predicate: Fn(Token) -> bool,
{
    Matches::of(predicate)
}

#[derive(Clone)]
pub struct Matches<Token, Predicate>
where
    Predicate: Fn(Token) -> bool,
{
    pub predicate: Predicate,
    token: PhantomData<Token>,
}

impl<Token, Predicate> Matches<Token, Predicate>
where
    Predicate: Fn(Token) -> bool,
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
    Input::Token: Eq + Copy,
    Predicate: Fn(Input::Token) -> bool,
{
    type Output = Input::Token;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        match input.next() {
            Some((token, next_input)) => {
                if (self.predicate)(token) {
                    (Some(token), next_input)
                } else {
                    (None, input.clone())
                }
            }
            None => (None, input.clone()),
        }
    }
}

pub fn one_of<Token>(tokens: Vec<Token>) -> OneOf<Token>
where
    Token: Eq,
{
    OneOf(tokens)
}

#[derive(Clone)]
pub struct OneOf<Token>(pub Vec<Token>)
where
    Token: Eq;

impl<Input> Parser<Input> for OneOf<Input::Token>
where
    Input: ParserInput,
    Input::Token: Eq,
{
    type Output = Input::Token;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        match input.next() {
            Some((token, next_input)) => {
                let OneOf(expected) = self;
                if expected.contains(&token) {
                    (Some(token), next_input)
                } else {
                    (None, input.clone())
                }
            }
            None => (None, input.clone()),
        }
    }
}
