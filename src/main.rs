use cli::repl::repl;

mod cli;
mod diagnostics;
mod lex;
mod parser_combinators;
mod types;
mod util;

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    repl(NAME.to_string(), VERSION.to_string());
}
