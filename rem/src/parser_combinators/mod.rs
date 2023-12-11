pub mod combinator;
pub mod parser;
#[cfg(test)]
mod tests;

pub trait TokenStream<Token> {
    fn advance(&mut self) -> Result<(), ()>;
    fn peek(&self) -> Result<Token, ()>;
}

// Boxed only because rustc does't support `impl` in type aliases yet
// For performance reasons, a parser directly modifies the token stream via the
// trait methods on consumption rather than return a copy for the remainder
pub type Parser<'parser, Consumes, Produces, Error> =
    Box<dyn Fn(&mut dyn TokenStream<Consumes>) -> Result<Produces, Error> + 'parser>;
