use super::TokenStream;
use std::collections::VecDeque;

mod parser;

pub struct TestTokenStream {
    pub n_advances: usize,

    stream: VecDeque<TestToken>,
}

impl TestTokenStream {
    fn new(tokens: Vec<TestToken>) -> Box<TestTokenStream> {
        Box::new(TestTokenStream {
            n_advances: 0,
            stream: VecDeque::from(tokens),
        })
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
