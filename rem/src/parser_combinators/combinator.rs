use super::Parser;

pub fn choice<Token: 'static, Production: 'static>(
    parsers: Vec<Parser<Token, Production, ()>>,
) -> Parser<Token, Production, ()> {
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
