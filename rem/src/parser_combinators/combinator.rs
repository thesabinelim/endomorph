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
            if let Ok(result) = parser(stream) {
                return Ok(result);
            }
        }
        Err(())
    })
}

pub fn some<'parser, Consumes, Produces, InnerError>(
    parser: Parser<'parser, Consumes, Produces, InnerError>,
) -> Parser<'parser, Consumes, Vec<Produces>, ()>
where
    Consumes: 'parser,
    Produces: 'parser,
    InnerError: 'parser,
{
    Box::new(move |stream| {
        let mut next_stream = stream;
        let mut productions = Vec::new();
        while let Ok((inner_next_stream, inner_production)) = parser(next_stream) {
            next_stream = inner_next_stream;
            productions.push(inner_production);
        }
        Ok((next_stream, productions))
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
        let (next_stream, _) = parser(stream)?;
        Ok((next_stream, value))
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
        let mut next_stream = stream;
        let mut productions = Vec::new();
        for parser in &parsers {
            let (inner_next_stream, inner_production) = parser(next_stream)?;
            next_stream = inner_next_stream;
            productions.push(inner_production);
        }
        Ok((next_stream, productions))
    })
}
