use std::fmt::Debug;

use crate::types::list::{List, ListOf};

pub mod combinator;
pub mod parser;

#[cfg(test)]
mod tests;

pub trait Parser<Input>: Clone + PartialEq {
    type Output: Clone + PartialEq + Debug;
    type Error;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>>;
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ParseError<Error> {
    pub expected: String,
    pub recoverable: bool,
    pub inner_error: Error,
}

pub trait ParserList<Input>: List {}

impl<Input, Item, Rest> ParserList<Input> for ListOf![Item, ..Rest]
where
    Item: Parser<Input>,
    Rest: ParserList<Input>,
{
}

impl<Input> ParserList<Input> for ListOf![] {}

pub trait TokenStream: Clone + PartialEq {
    type Token: Clone + Eq;

    fn next(&self) -> Result<(Self::Token, Self), TokenStreamError>;
}

pub enum TokenStreamError {
    Eof,
}

impl TokenStream for &str {
    type Token = char;

    fn next(&self) -> Result<(char, Self), TokenStreamError> {
        let mut iter = self.char_indices();
        match iter.next() {
            Some((_, first)) => Ok((first, &self[first.len_utf8()..])),
            None => Err(TokenStreamError::Eof),
        }
    }
}

impl TokenStream for String {
    type Token = char;

    fn next(&self) -> Result<(char, Self), TokenStreamError> {
        let mut iter = self.char_indices();
        match iter.next() {
            Some((_, first)) => Ok((first, self[first.len_utf8()..].to_string())),
            None => Err(TokenStreamError::Eof),
        }
    }
}
