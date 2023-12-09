pub mod parser;
#[cfg(test)]
mod tests;

pub trait TokenStream<Token> {
    fn advance(&mut self) -> Result<(), ()>;
    fn peek(&self) -> Result<Token, ()>;
}

// For performance reasons, a parser directly modifies the token stream via the
// trait methods on consumption rather than return a copy for the remainder
pub type Parser<Token, Production, Error> =
    Box<dyn Fn(&mut dyn TokenStream<Token>) -> Result<Production, Error>>;
