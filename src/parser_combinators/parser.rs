use std::marker::PhantomData;

use super::{ParseResult, Parser, ParserInput};

// TODO: Fail

pub fn emit<Output>(output: Output) -> Emit<Output> {
    Emit(output)
}

#[derive(Clone)]
pub struct Emit<Output>(pub Output);

impl<Input, Output> Parser<Input> for Emit<Output>
where
    Input: ParserInput,
    Output: Clone,
{
    type Output = Output;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        let Emit(output) = self;
        (Some(output.clone()), input.clone())
    }
}

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

pub fn just<Token>(expected: Token) -> Matches<Token, impl Fn(Token) -> bool + Clone>
where
    Token: Eq + Copy,
{
    matches(move |token| token == expected)
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

pub fn one_of<Token>(expected: Vec<Token>) -> Matches<Token, impl Fn(Token) -> bool + Clone>
where
    Token: Eq + Copy,
{
    matches(move |token| expected.contains(&token))
}
