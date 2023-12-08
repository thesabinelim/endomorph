mod io;
mod lex;
mod parser_combinators;
mod types;
mod util;

use io::prompt_line;
use lex::lex;
use types::{
    source::{Source, SourceError},
    token::{Token, TokenKind},
};
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

fn print_tokens(tokens: Vec<Token>) {
    println!(
        "[{}]",
        tokens
            .iter()
            .map(|token| match token.kind {
                TokenKind::Comment(_) => "Comment",
                TokenKind::EndOfInput => "EndOfInput",
                TokenKind::Identifier(_) => "Identifier",
                TokenKind::Keyword(_) => "Keyword",
                TokenKind::Literal(_) => "Literal",
                TokenKind::Operator(_) => "Operator",
                TokenKind::Punctuator(_) => "Punctuator",
                TokenKind::Whitespace(_) => "Whitespace",
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
