use core::fmt::{Debug, Display};

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
                recoverable: true,
                inner_error: EofError::NotEof,
            }),
            Err(_) => Ok(((), input)),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Just<Token>(pub Token);

#[derive(Debug, Eq, PartialEq)]
pub enum JustError {
    Mismatch,
    Eof,
}

impl<Input> Parser<Input> for Just<Input::Token>
where
    Input: TokenStream,
    Input::Token: Clone + Display + Eq + Debug,
{
    type Output = Input::Token;
    type Error = JustError;

    fn parse(&self, input: Input) -> Result<(Self::Output, Input), ParseError<Self::Error>> {
        let Just(expected) = self;
        match input.next() {
            Ok((actual, rest)) => {
                if actual == *expected {
                    Ok((actual, rest))
                } else {
                    Err(ParseError {
                        recoverable: true,
                        inner_error: JustError::Mismatch,
                    })
                }
            }
            Err(TokenStreamError::Eof) => Err(ParseError {
                recoverable: true,
                inner_error: JustError::Eof,
            }),
        }
    }
}
