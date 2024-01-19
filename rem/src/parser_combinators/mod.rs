use core::fmt::Debug;

pub mod combinator;
pub mod parser;

#[cfg(test)]
mod tests;

pub trait Parser<Input> {
    type Output;
    type Error;

    fn parse(
        &self,
        input: Input,
    ) -> Result<ParseSuccess<Input, Self::Output>, ParseError<Self::Error>>;
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseSuccess<Input, Output> {
    pub result: Output,
    pub rest: Input,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ParseError<Error> {
    pub expected: String,
    pub recoverable: bool,
    pub inner_error: Error,
}

pub trait TokenStream {
    type Token;

    fn next(&self) -> Result<(Self::Token, Self), TokenStreamError>
    where
        Self: Sized;
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
