use std::fmt::Debug;

pub mod combinator;
pub mod parser;

#[cfg(test)]
mod tests;

pub trait Parser<Input>: Clone + PartialEq
where
    Self::Output: Clone + Eq + Debug,
{
    type Output;
    type Error;

    fn parse(
        &self,
        input: Input,
    ) -> Result<ParseSuccess<Self::Output, Input>, ParseError<Self::Error>>;
}

pub type ParseSuccess<Output, NextInput> = (Output, NextInput);

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ParseError<Error> {
    pub expected: String,
    pub recoverable: bool,
    pub inner_error: Error,
}

pub trait TokenStream: Clone
where
    Self::Token: Clone + Eq,
{
    type Token;

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
