pub mod syntax_error;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Code {
    SyntaxError = 1,
    UnbalancedDelimiter = 2,
    UnexpectedIndent = 3,
    UseOfReservedKeyword = 4,
    MismatchingTypes = 5,
}
