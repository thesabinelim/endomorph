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

#[derive(Clone)]
pub struct TestTokenStream {
    pub n_advances: usize,

    stream: VecDeque<TestToken>,
}

impl TestTokenStream {
    fn from<StringT: AsRef<str>>(token_names: StringT) -> Box<TestTokenStream> {
        let mut tokens = VecDeque::new();
        for name in token_names.as_ref().chars() {
            tokens.push_back(TestToken::from(name));
        }
        Box::new(TestTokenStream {
            n_advances: 0,
            stream: tokens,
        })
    }
}

impl TokenStream<TestToken> for TestTokenStream {
    fn next(&self) -> Result<(Box<dyn TokenStream<TestToken>>, TestToken), ()> {
        let mut next_stream = self.clone();
        match next_stream.stream.pop_front() {
            Some(token) => Ok((Box::new(next_stream), token)),
            None => Err(()),
        }
    }
}

pub fn test_match<'parser>(expected_token_name: char) -> Parser<'parser, TestToken, TestToken, ()> {
    let expected = TestToken::from(expected_token_name);
    Box::new(move |stream| {
        let (next_stream, actual) = stream.next()?;
        if actual == expected {
            Ok((next_stream, actual))
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
