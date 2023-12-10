use super::TokenStream;

pub fn eof<Token>(stream: &dyn TokenStream<Token>) -> Result<(), ()> {
    match stream.peek() {
        Ok(_) => Err(()),
        Err(_) => Ok(()),
    }
}

pub fn single<Token: Eq>(
    expected: Token,
) -> impl Fn(&mut dyn TokenStream<Token>) -> Result<Token, ()> {
    move |stream| {
        let actual = stream.peek()?;
        if actual == expected {
            stream.advance()?;
            Ok(actual)
        } else {
            Err(())
        }
    }
}
