use super::{Parser, TokenStream};
use std::collections::VecDeque;
mod combinator;
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

impl TestToken {
    fn from(name: char) -> TestToken {
        match name {
            'a' => TestToken::A,
            'b' => TestToken::B,
            'c' => TestToken::C,
            'd' => TestToken::D,
            'e' => TestToken::E,
            'f' => TestToken::F,
            _ => panic!("{}", format!("Unrecognised TestToken: {}", name)),
        }
    }
}

pub struct TestTokenStream {
    pub n_advances: usize,

    stream: VecDeque<TestToken>,
}

impl TestTokenStream {
    fn from<StringT: AsRef<str>>(token_names: StringT) -> TestTokenStream {
        let mut tokens = VecDeque::new();
        for name in token_names.as_ref().chars() {
            tokens.push_back(TestToken::from(name));
        }
        TestTokenStream {
            n_advances: 0,
            stream: tokens,
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

pub fn test_match<'parser>(expected_token_name: char) -> Parser<'parser, TestToken, TestToken, ()> {
    let expected = TestToken::from(expected_token_name);
    Box::new(move |stream| {
        let actual = stream.peek()?;
        if actual == expected {
            stream.advance()?;
            Ok(actual)
        } else {
            Err(())
        }
    })
}

pub fn test_matches<'parser, StringT: AsRef<str>>(
    expected_token_names: StringT,
) -> Vec<Parser<'parser, TestToken, TestToken, ()>> {
    expected_token_names
        .as_ref()
        .chars()
        .map(|name| test_match(name))
        .collect()
}
