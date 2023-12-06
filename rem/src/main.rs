mod data;
mod io;
mod lex;
mod util;

use data::{
    source::{Source, SourceError},
    token::TokenData,
};
use io::io::prompt_line;
use lex::lex::lex;
use util::source::offset_to_position;
use util::text::remove_line_break;

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    println!("{} {}", NAME, VERSION);
    while let Ok(Some(line)) = prompt_line() {
        process_text(remove_line_break(line));
    }
}

fn process_text(text: String) {
    let source = Source {
        file: "stdin".to_string(),
        text,
    };
    match lex(&source) {
        Ok(tokens) => print_tokens(tokens),
        Err(error) => print_error(&source, error),
    }
}

fn print_tokens(tokens: Vec<TokenData>) {
    println!(
        "[{}]",
        tokens
            .iter()
            .map(|token| match token.token {
                data::token::Token::Comment(_) => "Comment",
                data::token::Token::EndOfInput => "EndOfInput",
                data::token::Token::Identifier(_) => "Identifier",
                data::token::Token::Keyword(_) => "Keyword",
                data::token::Token::Literal(_) => "Literal",
                data::token::Token::Operator(_) => "Operator",
                data::token::Token::Punctuator(_) => "Punctuator",
                data::token::Token::Whitespace(_) => "Whitespace",
            })
            .collect::<Vec<&str>>()
            .join(", ")
    );
}

fn print_error(source: &Source, error: SourceError) {
    let position = offset_to_position(&source, error.offset);
    println!(
        "{}:{}:{} {}",
        source.file,
        position.line + 1,
        position.col + 1,
        error.message
    );
}
