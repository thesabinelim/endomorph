use std::any::Any;

use super::TokenStream;

pub fn choice<Token, Production>(
    parsers: Vec<impl Fn(&mut dyn TokenStream<Token>) -> Result<Production, ()>>,
) -> impl Fn(&mut dyn TokenStream<Token>) -> Result<Production, ()> {
    move |stream| {
        for parser in &parsers {
            let result = parser(stream);
            if let Ok(production) = result {
                return Ok(production);
            }
        }
        Err(())
    }
}

pub fn produce<Token, Production: Copy, Error, OriginalProduction: Any>(
    value: Production,
    parser: impl Fn(&mut dyn TokenStream<Token>) -> Result<OriginalProduction, Error>,
) -> impl Fn(&mut dyn TokenStream<Token>) -> Result<Production, Error> {
    move |stream| {
        let result = parser(stream);
        match result {
            Ok(_) => Ok(value),
            Err(error) => Err(error),
        }
    }
}

pub fn sequence<Token, Production, Error>(
    parsers: Vec<impl Fn(&mut dyn TokenStream<Token>) -> Result<Production, Error>>,
) -> impl Fn(&mut dyn TokenStream<Token>) -> Result<Vec<Production>, Error> {
    move |stream| {
        let mut productions = Vec::new();
        for parser in &parsers {
            let result = parser(stream);
            match result {
                Ok(production) => productions.push(production),
                Err(error) => return Err(error),
            }
        }
        Ok(productions)
    }
}
