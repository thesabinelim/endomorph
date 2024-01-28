use super::ParserInput;

mod combinator;
mod parser;

impl ParserInput for &str {
    type Token = char;

    fn next(&self) -> Option<(char, Self)> {
        match self.char_indices().next() {
            Some((_, first)) => Some((first, &self[first.len_utf8()..])),
            None => None,
        }
    }
}
