use super::{ParseError, ParseSuccess, Parser, TokenStream, TokenStreamError};

mod combinator;
mod parser;

#[derive(Clone)]
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
    ) -> Result<ParseSuccess<Self::Output, String>, ParseError<Self::Error>> {
        match input.next() {
            Ok((actual, rest)) => {
                if actual == self.expected {
                    Ok((actual, rest))
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
