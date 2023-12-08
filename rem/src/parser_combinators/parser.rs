use super::Parser;

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
