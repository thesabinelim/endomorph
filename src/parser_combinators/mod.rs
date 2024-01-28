use crate::types::list::{List, ListOf};

pub mod combinator;
pub mod parser;

#[cfg(test)]
mod tests;

pub trait Parser<Input> {
    type Output;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output>;
}

pub type ParseResult<Input, Output> = (Option<Output>, Input);

pub trait ParserList<Input, Output>: List {}

impl<Input, Output, Item, Rest> ParserList<Input, Output> for ListOf![Item, ..Rest]
where
    Item: Parser<Input, Output = Output>,
    Rest: ParserList<Input, Output>,
{
}

impl<Input, Output> ParserList<Input, Output> for ListOf![] {}

pub trait ParserInput: Clone {
    type Token: Clone + Eq;

    fn next(&self) -> Option<(Self::Token, Self)>;
}
