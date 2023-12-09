use super::Parser;

pub fn eof<Token>() -> Parser<Token, (), ()> {
    Box::new(|stream| match stream.peek() {
        Ok(_) => Err(()),
        Err(_) => Ok(()),
    })
}

pub fn single<Token: Eq + 'static>(expected: Token) -> Parser<Token, Token, ()> {
    Box::new(move |stream| {
        let actual = stream.peek()?;
        if actual == expected {
            stream.advance()?;
            Ok(actual)
        } else {
            Err(())
        }
    })
}
