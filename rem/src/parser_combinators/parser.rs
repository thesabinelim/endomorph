use std::fmt::{Debug, Display};

use super::{ParseError, Parser, TokenStream, TokenStreamError};

#[derive(Clone, PartialEq)]
pub struct Eof;

#[derive(Debug, Eq, PartialEq)]
pub enum EofError {
    NotEof,
}

impl<Input> Parser<Input> for Eof
where
    Input: TokenStream,
{
    type Output = ();
    type Error = EofError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        match input.next() {
            Ok(_) => Err(ParseError {
                expected: "EOF".to_string(),
                recoverable: true,
                inner_error: EofError::NotEof,
            }),
            Err(_) => Ok(((), input)),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Single<Token>(pub Token);

#[derive(Debug, Eq, PartialEq)]
pub enum SingleError {
    Mismatch,
    Eof,
}

impl<Input> Parser<Input> for Single<Input::Token>
where
    Input: TokenStream,
    Input::Token: Clone + Display + Eq + Debug,
{
    type Output = Input::Token;
    type Error = SingleError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let Single(expected) = self;
        match input.next() {
            Ok((actual, rest)) => {
                if actual == *expected {
                    Ok((actual, rest))
                } else {
                    Err(ParseError {
                        expected: expected.to_string(),
                        recoverable: true,
                        inner_error: SingleError::Mismatch,
                    })
                }
            }
            Err(TokenStreamError::Eof) => Err(ParseError {
                expected: expected.to_string(),
                recoverable: true,
                inner_error: SingleError::Eof,
            }),
        }
    }
}
