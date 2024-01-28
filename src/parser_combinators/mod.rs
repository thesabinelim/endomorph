use crate::types::list::{List, ListOf};

pub mod combinator;
pub mod parser;

#[cfg(test)]
mod tests;

pub trait Parser<Input>: Clone {
    type Output;

    fn parse(&self, input: &Input) -> ParseResult<Input, Self::Output>;
}

pub type ParseResult<Input, Output> = (Option<Output>, Input);

pub trait ParserList<Input>: List {}

impl<Input, Item, Rest> ParserList<Input> for ListOf![Item, ..Rest]
where
    Item: Parser<Input>,
    Rest: ParserList<Input>,
{
}

impl<Input> ParserList<Input> for ListOf![] {}

pub trait LikeParserList<Input, Output>: ParserList<Input> {}

impl<Input, Output, Item, Rest> LikeParserList<Input, Output> for ListOf![Item, ..Rest]
where
    Item: Parser<Input, Output = Output>,
    Rest: LikeParserList<Input, Output>,
{
}

impl<Input, Output> LikeParserList<Input, Output> for ListOf![] {}

pub trait ParserInput: Clone {
    type Token: Clone + Eq;

    fn next(&self) -> Option<(Self::Token, Self)>;
}
