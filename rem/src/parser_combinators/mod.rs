pub trait TokenStream<Token> {
    fn advance(&self);
    fn peek(&self) -> Token;
}

// For performance reasons, a parser directly modifies the token stream via the
// trait methods on consumption rather than return a copy for the remainder
pub type Parser<Token, Production> = fn(dyn TokenStream<Token>) -> Result<Production, String>;
