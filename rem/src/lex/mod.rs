use crate::{
    diagnostics::Problem,
    parser_combinators::{ParseError, Parser, TokenStream, TokenStreamError},
    types::{
        source::{Offset, SourceId},
        token::Token,
    },
};

pub fn lex(source: SourceId, offset: Offset, text: String) -> (Vec<Token>, Vec<Problem>) {
    let input = CharStream {
        source,
        text,
        offset,
    };
    match Tokens.parse(input) {
        Ok((output, _)) => output,
        Err(_) => todo!(),
    }
}

#[derive(Clone, PartialEq)]
struct Tokens;

impl Parser<CharStream> for Tokens {
    type Output = (Vec<Token>, Vec<Problem>);

    type Error = ();

    fn parse(
        &self,
        input: CharStream,
    ) -> Result<(Self::Output, CharStream), ParseError<Self::Error>> {
        todo!()
    }
}

#[derive(Clone, PartialEq)]
struct CharStream {
    pub source: SourceId,
    pub text: String,
    pub offset: Offset,
}

impl TokenStream for CharStream {
    type Token = char;

    fn next(&self) -> Result<(Self::Token, Self), TokenStreamError> {
        match self.text.char_indices().next() {
            Some((_, first)) => Ok((
                first,
                CharStream {
                    source: self.source.clone(),
                    text: self.text[first.len_utf8()..].to_string(),
                    offset: self.offset + 1,
                },
            )),
            None => Err(TokenStreamError::Eof),
        }
    }
}
