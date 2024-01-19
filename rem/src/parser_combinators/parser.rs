use super::{ParseError, ParseSuccess, Parser, TokenStream, TokenStreamError};
use core::fmt::Debug;
use std::fmt::Display;

pub fn eof<Input>() -> impl Parser<Input, Output = (), Error = EofError>
where
    Input: TokenStream,
{
    Eof
}

#[derive(Debug, Eq, PartialEq)]
pub enum EofError {
    NotEof,
}

struct Eof;

impl<Input> Parser<Input> for Eof
where
    Input: TokenStream,
{
    type Output = ();
    type Error = EofError;

    fn parse(
        &self,
        input: Input,
    ) -> Result<ParseSuccess<Input, Self::Output>, ParseError<Self::Error>> {
        match input.next() {
            Ok(_) => Err(ParseError {
                expected: "EOF".to_string(),
                recoverable: true,
                inner_error: EofError::NotEof,
            }),
            Err(_) => Ok(ParseSuccess {
                result: (),
                rest: input,
            }),
        }
    }
}

pub fn single<Input>(
    expected: Input::Token,
) -> impl Parser<Input, Output = Input::Token, Error = SingleError>
where
    Input: TokenStream,
    Input::Token: Display + Eq,
{
    Single { expected }
}

#[derive(Debug, Eq, PartialEq)]
pub enum SingleError {
    Mismatch,
    Eof,
}

struct Single<Token> {
    pub expected: Token,
}

impl<Input> Parser<Input> for Single<Input::Token>
where
    Input: TokenStream,
    Input::Token: Display + Eq,
{
    type Output = Input::Token;
    type Error = SingleError;

    fn parse(
        &self,
        input: Input,
    ) -> Result<ParseSuccess<Input, Self::Output>, ParseError<Self::Error>> {
        match input.next() {
            Ok((actual, rest)) => {
                if actual == self.expected {
                    Ok(ParseSuccess {
                        result: actual,
                        rest,
                    })
                } else {
                    Err(ParseError {
                        expected: self.expected.to_string(),
                        recoverable: true,
                        inner_error: SingleError::Mismatch,
                    })
                }
            }
            Err(TokenStreamError::Eof) => Err(ParseError {
                expected: self.expected.to_string(),
                recoverable: true,
                inner_error: SingleError::Eof,
            }),
        }
    }
}
