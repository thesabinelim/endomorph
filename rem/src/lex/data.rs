use crate::data::{source::SourceError, token::TokenData};

pub struct PartialLexInput {
    pub offset: usize,
    pub remaining: String,
}

pub type PartialLexResult = Result<PartialLexSuccess, SourceError>;

pub struct PartialLexSuccess {
    pub token: TokenData,
    pub next_input: PartialLexInput,
}
