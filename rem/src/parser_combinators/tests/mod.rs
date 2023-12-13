use super::{Parser, TokenStream};
use std::{any::Any, collections::VecDeque};
mod combinator;
mod parser;

#[macro_export]
macro_rules! test_tokens {
    ( $( $x:expr ),* ) => {
        {
            let mut temp_vec: Vec<crate::parser_combinators::tests::TestToken> = Vec::new();
            $(
                temp_vec.push($x);
            )*
            &mut crate::parser_combinators::tests::TestTokenStream::new(temp_vec)
        }
    };
}

pub struct TestTokenStream {
    pub n_advances: usize,

    stream: VecDeque<TestToken>,
}

impl TestTokenStream {
    fn new(tokens: Vec<TestToken>) -> TestTokenStream {
        TestTokenStream {
            n_advances: 0,
            stream: VecDeque::from(tokens),
        }
    }
}

impl TokenStream<TestToken> for TestTokenStream {
    fn advance(&mut self) -> Result<(), ()> {
        match self.stream.pop_front() {
            Some(_) => Ok(()),
            None => Err(()),
        }
    }

    fn peek(&self) -> Result<TestToken, ()> {
        match self.stream.front() {
            Some(token) => Ok(*token),
            None => Err(()),
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum TestToken {
    A,
    B,
    C,
    D,
    E,
    F,
}

pub fn test_success<'parser, Consumes, Produces, Error>(
    value: Produces,
) -> Parser<'parser, Consumes, Produces, Error>
where
    Produces: 'parser + Copy,
    Error: Any,
{
    Box::new(move |_| Ok(value))
}

pub fn test_error<'parser, Consumes, Produces, Error>(
    error: Error,
) -> Parser<'parser, Consumes, Produces, Error>
where
    Produces: Any,
    Error: 'parser + Copy,
{
    Box::new(move |_| Err(error))
}
