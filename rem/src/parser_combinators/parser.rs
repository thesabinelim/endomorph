use super::Parser;

pub fn eof<'parser, Consumes>() -> Parser<'parser, Consumes, (), ()> {
    Box::new(|stream| match stream.peek() {
        Ok(_) => Err(()),
        Err(_) => Ok(()),
    })
}

pub fn single<'parser, Consumes>(expected: Consumes) -> Parser<'parser, Consumes, Consumes, ()>
where
    Consumes: 'parser + Eq,
{
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
