pub mod combinator;
pub mod parser;
#[cfg(test)]
mod tests;

pub trait TokenStream<Token> {
    fn next(&self) -> Result<(Box<dyn TokenStream<Token>>, Token), ()>;
}

// Boxed only because rustc does't support `impl` in type aliases yet
// For performance reasons, a parser directly modifies the token stream via the
// trait methods on consumption rather than return a copy for the remainder
pub type Parser<'parser, Consumes, Produces, Error> =
    Box<dyn Fn(Box<dyn TokenStream<Consumes>>) -> ParseResult<Consumes, Produces, Error> + 'parser>;

pub type ParseResult<Consumes, Produces, Error> =
    Result<(Box<dyn TokenStream<Consumes>>, Produces), Error>;
