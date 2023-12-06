mod data;
mod lex;

use std::io::{self, Write};

use crate::lex::lex::lex;
use data::{source_info::SourceError, token::TokenData};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    println!("{} {}", NAME, VERSION);
    while let Ok(Some(line)) = prompt_line() {
        process_text(remove_line_break(line));
    }
}

fn process_text(text: String) {
    match lex("stdin".to_string(), text) {
        Ok(tokens) => print_tokens(tokens),
        Err(error) => print_error(error),
    }
}

fn print_tokens(tokens: Vec<TokenData>) {
    println!(
        "[{}]",
        tokens
            .iter()
            .map(|token| match token.token {
                data::token::Token::Comment(_) => "Comment".to_string(),
                data::token::Token::EndOfInput => "EndOfInput".to_string(),
                data::token::Token::Identifier(_) => "Identifier".to_string(),
                data::token::Token::Keyword(_) => "Keyword".to_string(),
                data::token::Token::Literal(_) => "Literal".to_string(),
                data::token::Token::Operator(_) => "Operator".to_string(),
                data::token::Token::Punctuator(_) => "Punctuator".to_string(),
                data::token::Token::Whitespace(_) => "Whitespace".to_string(),
            })
            .collect::<Vec<String>>()
            .join(", ")
    );
}

fn print_error(error: SourceError) {
    println!(
        "{}:{}:{} {}",
        error.location.file,
        error.location.line_offset + 1,
        error.location.col_offset + 1,
        error.message
    );
}

fn prompt_line() -> Result<Option<String>, io::Error> {
    print!("> ");
    io::stdout().flush()?;
    read_line()
}

fn read_line() -> Result<Option<String>, io::Error> {
    let mut line = String::new();
    let len = io::stdin().read_line(&mut line)?;
    if len > 0 {
        Ok(Some(line))
    } else {
        Ok(None)
    }
}

fn remove_line_break(line: String) -> String {
    let mut line = line;
    if line.ends_with('\n') {
        line.pop();
    }
    if line.ends_with('\r') {
        line.pop();
    }
    line
}
