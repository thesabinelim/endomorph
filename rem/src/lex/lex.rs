use crate::data::source_info::{SourceError, SourceLocation};
use crate::data::token::TokenData;

pub fn lex(file: String, _text: String) -> Result<Vec<TokenData>, SourceError> {
    Err(SourceError {
        location: SourceLocation {
            file: file,
            line_offset: 0,
            col_offset: 0,
        },
        message: "Lexer unimplemented".to_string(),
    })
}
