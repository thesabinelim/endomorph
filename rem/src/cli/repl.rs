use std::io::{self, Error, Write};

use crate::diagnostics::Problem;
use crate::lex::lex;
use crate::types::source::SourceId;
use crate::types::token::Token;
use crate::util::text::remove_line_break;

pub fn repl(compiler_name: String, compiler_version: String) {
    println!("{} {}", compiler_name, compiler_version);
    while let Ok(Some(line)) = prompt_line() {
        process_text(remove_line_break(line));
    }
}

fn process_text(text: String) {
    let (tokens, problems) = lex(SourceId::Stdin, 0, text);
    println!("{}", format_tokens(tokens));
    eprintln!("{}", format_problems(problems));
}

fn format_tokens(tokens: Vec<Token>) -> String {
    format!(
        "[{}]",
        tokens
            .into_iter()
            .map(|token| format!("{:?}", token))
            .collect::<Vec<String>>()
            .join(", ")
    )
}

fn format_problems(problems: Vec<Problem>) -> String {
    problems
        .into_iter()
        .map(|problem| format!("{:?}\n", problem))
        .collect::<Vec<String>>()
        .join("\n")
}

pub fn prompt_line() -> Result<Option<String>, Error> {
    print!("> ");
    io::stdout().flush()?;
    read_line()
}

fn read_line() -> Result<Option<String>, Error> {
    let mut line = String::new();
    let len = io::stdin().read_line(&mut line)?;
    if len > 0 {
        Ok(Some(line))
    } else {
        Ok(None)
    }
}
