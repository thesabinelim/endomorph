use std::io::{self, Write};

const NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    println!("{} {}", NAME, VERSION);
    while let Ok(Some(line)) = prompt_line() {
        let line = remove_line_break(line);
        println!("{}", line);
    }
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
