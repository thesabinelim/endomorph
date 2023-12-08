use std::collections::VecDeque;

use super::TokenStream;

mod parser;

#[derive(Clone, Copy, Eq, PartialEq)]
pub enum TestToken {
    A,
    B,
    C,
    D,
    E,
    F,
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

    fn peek(&mut self) -> Result<TestToken, ()> {
        match self.stream.front() {
            Some(token) => Ok(*token),
            None => Err(()),
        }
    }
}
