use core::marker::PhantomData;

use super::{ParseResult, Parser, ParserInput, ParserList};
use crate::types::list::{ListOf, ListPat, NonEmptyList};

// TODO: Catch (MapErr?), Maybe, While, Map

pub fn or<Input, Output, Parsers>(parsers: Parsers) -> Or<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: ParserList<Input, Output> + NonEmptyList + Clone,
{
    Or::of(parsers)
}

#[derive(Clone)]
pub struct Or<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: ParserList<Input, Output> + NonEmptyList + Clone,
{
    pub parsers: Parsers,
    input: PhantomData<Input>,
    output: PhantomData<Output>,
}

impl<Input, Output, Parsers> Or<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: ParserList<Input, Output> + NonEmptyList + Clone,
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
    Item: Parser<Input, Output = Output> + Clone,
    Rest: ParserList<Input, Output> + NonEmptyList + Clone,
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
    Item: Parser<Input, Output = Output> + Clone,
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
    Parsers: ParserList<Input, Output> + NonEmptyList + Clone,
{
    Seq::of(parsers)
}

#[derive(Clone)]
pub struct Seq<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: ParserList<Input, Output> + NonEmptyList + Clone,
{
    pub parsers: Parsers,
    input: PhantomData<Input>,
    output: PhantomData<Output>,
}

impl<Input, Output, Parsers> Seq<Input, Output, Parsers>
where
    Input: ParserInput,
    Parsers: ParserList<Input, Output> + NonEmptyList + Clone,
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
    Item: Parser<Input, Output = Output> + Clone,
    Rest: ParserList<Input, Output> + NonEmptyList + Clone,
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
    Item: Parser<Input, Output = Output> + Clone,
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

pub fn to<Input, Output, InnerParser>(
    output: Output,
    inner_parser: InnerParser,
) -> To<Input, Output, InnerParser>
where
    Output: Clone,
    InnerParser: Parser<Input>,
{
    To::of(output, inner_parser)
}

#[derive(Clone)]
pub struct To<Input, Output, InnerParser>
where
    Output: Clone,
    InnerParser: Parser<Input>,
{
    pub output: Output,
    pub inner_parser: InnerParser,
    input: PhantomData<Input>,
}

impl<Input, Output, InnerParser> To<Input, Output, InnerParser>
where
    Output: Clone,
    InnerParser: Parser<Input>,
{
    pub fn of(output: Output, inner_parser: InnerParser) -> Self {
        To {
            output,
            inner_parser,
            input: PhantomData,
        }
    }
}

impl<Input, Output, InnerParser> Parser<Input> for To<Input, Output, InnerParser>
where
    Input: ParserInput,
    Output: Clone,
    InnerParser: Parser<Input>,
{
    type Output = Output;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output> {
        let (result, next_input) = self.inner_parser.parse(input);
        match result {
            Some(_) => (Some(self.output.clone()), next_input),
            None => (None, input.clone()),
        }
    }
}
