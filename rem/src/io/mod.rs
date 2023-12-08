use std::io::{self, Write};

pub fn prompt_line() -> Result<Option<String>, io::Error> {
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
