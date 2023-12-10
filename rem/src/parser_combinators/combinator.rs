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
