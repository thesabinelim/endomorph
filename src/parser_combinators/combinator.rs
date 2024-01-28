use core::marker::PhantomData;

use super::{LikeParserList, ParseResult, Parser, ParserInput};
use crate::types::list::{ListOf, ListPat, NonEmptyList};

// TODO: Not, To, Catch (MapErr?), Maybe, While, Map

pub fn or<Input, Output, Parsers>(parsers: Parsers) -> Or<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    Or::of(parsers)
}

#[derive(Clone)]
pub struct Or<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    pub parsers: Parsers,
    input: PhantomData<Input>,
    output: PhantomData<Output>,
}

impl<Input, Output, Parsers> Or<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    pub fn of(parsers: Parsers) -> Self {
        Or {
            parsers,
            input: PhantomData,
            output: PhantomData,
        }
    }
}

impl<Input, Output, Item, Rest> Parser<Input> for Or<Input, Output, ListOf![Item, ..Rest]>
where
    Input: ParserInput,
    Output: Clone,
    Item: Parser<Input, Output = Output>,
    Rest: LikeParserList<Input, Output> + NonEmptyList,
    Or<Input, Output, Rest>: Parser<Input, Output = Output>,
{
    type Output = Output;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        let ListPat![first, ..rest] = &self.parsers;
        let (result, next_input) = first.parse(input);
        match result {
            Some(output) => (Some(output), next_input),
            None => Or::of(rest.clone()).parse(input),
        }
    }
}

impl<Input, Output, Item> Parser<Input> for Or<Input, Output, ListOf![Item]>
where
    Input: ParserInput,
    Output: Clone,
    Item: Parser<Input, Output = Output>,
{
    type Output = Output;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        let ListPat![first] = &self.parsers;
        let (result, next_input) = first.parse(&input);
        match result {
            Some(output) => (Some(output), next_input),
            None => (None, input.clone()),
        }
    }
}

pub fn seq<Input, Output, Parsers>(parsers: Parsers) -> Seq<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    Seq::of(parsers)
}

#[derive(Clone)]
pub struct Seq<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    pub parsers: Parsers,
    input: PhantomData<Input>,
    output: PhantomData<Output>,
}

impl<Input, Output, Parsers> Seq<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: LikeParserList<Input, Output> + NonEmptyList,
{
    pub fn of(parsers: Parsers) -> Self {
        Seq {
            parsers,
            input: PhantomData,
            output: PhantomData,
        }
    }
}

impl<Input, Output, Item, Rest> Parser<Input> for Seq<Input, Output, ListOf![Item, ..Rest]>
where
    Input: ParserInput,
    Output: Clone,
    Item: Parser<Input, Output = Output>,
    Rest: LikeParserList<Input, Output> + NonEmptyList,
    Seq<Input, Output, Rest>: Parser<Input, Output = Vec<Output>>,
{
    type Output = Vec<Output>;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        let ListPat![first, ..rest] = &self.parsers;
        let (result, next_input) = first.parse(input);
        if let Some(output) = result {
            let (rest_result, rest_next_input) = Seq::of(rest.clone()).parse(&next_input);
            if let Some(rest_output) = rest_result {
                return (
                    Some(vec![vec![output], rest_output].concat()),
                    rest_next_input,
                );
            }
        }
        (None, input.clone())
    }
}

impl<Input, Output, Item> Parser<Input> for Seq<Input, Output, ListOf![Item]>
where
    Input: ParserInput,
    Output: Clone,
    Item: Parser<Input, Output = Output>,
{
    type Output = Vec<Output>;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        let ListPat![first] = &self.parsers;
        let (result, next_input) = first.parse(&input);
        match result {
            Some(output) => (Some(vec![output]), next_input),
            None => (None, input.clone()),
        }
    }
}
