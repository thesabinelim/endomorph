mod token;

use crate::types::{
    source::{Source, SourceError},
    token::Token,
};

pub struct PartialLexInput {
    pub offset: usize,
    pub remaining: String,
}

pub type PartialLexResult = Result<PartialLexSuccess, SourceError>;

pub struct PartialLexSuccess {
    pub token: Token,
    pub next_input: PartialLexInput,
}

pub fn lex(source: &Source) -> Result<Vec<Token>, SourceError> {
    let success = token(PartialLexInput {
        offset: 0,
        remaining: source.text.clone(),
    })?;
    Ok(vec![success.token])
}

fn token(input: PartialLexInput) -> PartialLexResult {
    Err(SourceError {
        offset: input.offset,
        message: "Lexer unimplemented".to_string(),
    })
}
