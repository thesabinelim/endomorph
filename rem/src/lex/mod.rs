mod token;
mod types;

use self::types::{PartialLexInput, PartialLexResult};
use crate::types::{
    source::{Source, SourceError},
    token::TokenData,
};

pub fn lex(input: &Source) -> Result<Vec<TokenData>, SourceError> {
    let success = token(PartialLexInput {
        offset: 0,
        remaining: input.text.clone(),
    })?;
    Ok(vec![success.token])
}

fn token(input: PartialLexInput) -> PartialLexResult {
    Err(SourceError {
        offset: input.offset,
        message: "Lexer unimplemented".to_string(),
    })
}
