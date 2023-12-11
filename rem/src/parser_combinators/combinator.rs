use std::any::Any;

use super::Parser;

pub fn choice<'parser, Consumes, Produces, Error>(
    parsers: Vec<Parser<'parser, Consumes, Produces, Error>>,
) -> Parser<'parser, Consumes, Produces, ()>
where
    Consumes: 'parser,
    Produces: 'parser,
    Error: Any,
{
    Box::new(move |stream| {
        for parser in &parsers {
            let result = parser(stream);
            if let Ok(production) = result {
                return Ok(production);
            }
        }
        Err(())
    })
}

pub fn produce<'parser, Consumes, Produces, Error, InnerProduces>(
    value: Produces,
    parser: Parser<'parser, Consumes, InnerProduces, Error>,
) -> Parser<'parser, Consumes, Produces, Error>
where
    Consumes: 'parser,
    Produces: 'parser + Copy,
    Error: 'parser,
    InnerProduces: Any,
{
    Box::new(move |stream| {
        let result = parser(stream);
        match result {
            Ok(_) => Ok(value),
            Err(error) => Err(error),
        }
    })
}

pub fn sequence<'parser, Consumes, Produces, Error>(
    parsers: Vec<Parser<'parser, Consumes, Produces, Error>>,
) -> Parser<'parser, Consumes, Vec<Produces>, Error>
where
    Consumes: 'parser,
    Produces: 'parser,
    Error: 'parser,
{
    Box::new(move |stream| {
        let mut productions = Vec::new();
        for parser in &parsers {
            let result = parser(stream);
            match result {
                Ok(production) => productions.push(production),
                Err(error) => return Err(error),
            }
        }
        Ok(productions)
    })
}
