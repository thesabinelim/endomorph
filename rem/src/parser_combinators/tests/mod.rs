use super::{ParseError, ParseSuccess, Parser, TokenStream, TokenStreamError};

mod combinator;
mod parser;

pub fn test_match(expected: char) -> impl Parser<String> {
    TestMatch {
        expected,
        recoverable: true,
    }
}

pub fn test_match_unrecoverable<I>(expected: char) -> impl Parser<String> {
    TestMatch {
        expected,
        recoverable: false,
    }
}

struct TestMatch {
    pub expected: char,
    pub recoverable: bool,
}

impl Parser<String> for TestMatch {
    type Output = char;
    type Error = ();

    fn parse(
        &self,
        input: String,
    ) -> Result<ParseSuccess<String, Self::Output>, ParseError<Self::Error>> {
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
                        recoverable: self.recoverable,
                        inner_error: (),
                    })
                }
            }
            Err(TokenStreamError::Eof) => Err(ParseError {
                expected: self.expected.to_string(),
                recoverable: self.recoverable,
                inner_error: (),
            }),
        }
    }
}
