use crate::{
    diagnostics::Problem,
    parser_combinators::{ParseResult, Parser, ParserInput},
    types::{
        source::{Offset, SourceId},
        token::Token,
    },
};

pub fn lex(source: SourceId, offset: Offset, text: String) -> (Vec<Token>, Vec<Problem>) {
    let input = StringParserInput {
        source,
        text,
        offset,
    };
    let (result, _) = Tokens.parse(&input);
    match result {
        Some(output) => output,
        None => panic!("Internal compiler error!"),
    }
}

#[derive(Clone, PartialEq)]
struct Tokens;

impl Parser<StringParserInput> for Tokens {
    type Output = (Vec<Token>, Vec<Problem>);

    fn parse(&self, input: &StringParserInput) -> ParseResult<StringParserInput, Self::Output> {
        todo!()
    }
}

#[derive(Clone, PartialEq)]
struct StringParserInput {
    pub source: SourceId,
    pub text: String,
    pub offset: Offset,
}

impl ParserInput for StringParserInput {
    type Token = char;

    fn next(&self) -> Option<(Self::Token, Self)> {
        match self.text.char_indices().next() {
            Some((_, first)) => Some((
                first,
                StringParserInput {
                    source: self.source.clone(),
                    text: self.text[first.len_utf8()..].to_string(),
                    offset: self.offset + 1,
                },
            )),
            None => None,
        }
    }
}
