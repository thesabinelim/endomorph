mod token;
mod types;

use self::types::{PartialLexInput, PartialLexResult};
use crate::types::{
    source::{Source, SourceError},
    token::TokenData,
};

pub fn lex(source: &Source) -> Result<Vec<TokenData>, SourceError> {
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
