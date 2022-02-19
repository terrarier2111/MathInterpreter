use std::io::{Error, ErrorKind, Write};

pub fn input(text: String) -> std::io::Result<String> {
    print!("{}", text);
    std::io::stdout().flush()?; // because print! doesn't flush
    let mut input = String::new();
    if std::io::stdin().read_line(&mut input)? == 0 {
        return Err(Error::new(
            ErrorKind::UnexpectedEof,
            "EOF while reading a line",
        ));
    }
    if input.ends_with('\n') {
        input.pop();
        if input.ends_with('\r') {
            input.pop();
        }
    }
    Ok(input)
}

#[macro_export]
macro_rules! pluralize {
    ($num: expr) => {
        if $num > 1 {
            "s"
        } else {
            ""
        }
    };
}
