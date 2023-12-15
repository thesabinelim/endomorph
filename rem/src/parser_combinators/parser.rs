use super::Parser;

pub fn eof<'parser, Consumes>() -> Parser<'parser, Consumes, (), ()> {
    Box::new(|stream| match stream.next() {
        Ok(_) => Err(()),
        Err(_) => Ok((stream, ())),
    })
}

pub fn single<'parser, Consumes>(expected: Consumes) -> Parser<'parser, Consumes, Consumes, ()>
where
    Consumes: 'parser + Eq,
{
    Box::new(move |stream| {
        let (next_stream, actual) = stream.next()?;
        if actual == expected {
            Ok((next_stream, actual))
        } else {
            Err(())
        }
    })
}
