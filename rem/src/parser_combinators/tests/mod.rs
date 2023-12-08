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

pub struct TestTokenStream<Token> {
    pub n_advances: usize,

    stream: VecDeque<Token>,
}

impl<Token> TestTokenStream<Token> {
    fn new(tokens: Vec<Token>) -> TestTokenStream<Token> {
        TestTokenStream {
            n_advances: 0,
            stream: VecDeque::from(tokens),
        }
    }
}

impl<Token: Copy> TokenStream<Token> for TestTokenStream<Token> {
    fn advance(&mut self) -> Result<(), ()> {
        match self.stream.pop_front() {
            Some(_) => Ok(()),
            None => Err(()),
        }
    }

    fn peek(&mut self) -> Result<Token, ()> {
        match self.stream.front() {
            Some(token) => Ok(*token),
            None => Err(()),
        }
    }
}
