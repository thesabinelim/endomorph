pub mod combinator;
pub mod parser;
#[cfg(test)]
mod tests;

pub trait TokenStream<Token> {
    fn advance(&mut self) -> Result<(), ()>;
    fn peek(&self) -> Result<Token, ()>;
}
