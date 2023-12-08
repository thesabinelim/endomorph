use super::{Parser, TokenStream};

pub fn eof<Token>(stream: Box<dyn TokenStream<Token>>) -> Result<(), ()> {
    match stream.peek() {
        Ok(_) => Err(()),
        Err(_) => Ok(()),
    }
}

pub fn single<Token: Eq + 'static>(expected: Token) -> Parser<Token, Token, ()> {
    Box::new(move |mut stream| {
        let actual = stream.peek()?;
        if actual == expected {
            stream.advance()?;
            Ok(actual)
        } else {
            Err(())
        }
    })
}
