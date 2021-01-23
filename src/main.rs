#[macro_use]
mod token;
mod lexer;
mod tests;
mod parser;
mod ast;

use anyhow::Result;

use lexer::Lexer;
use std::env::args;
use std::fs::File;
use std::io::Read;

fn main() -> Result<()> {
    let args: Vec<String> = args().into_iter().collect();

    let input_file_name = args[1].to_owned();
    let mut f = File::open(input_file_name).unwrap();
    let mut input = String::new();
    let _ = f.read_to_string(&mut input).unwrap();
    let mut lexer = Lexer::new(&input);
    match lexer.lex() {
        Ok(tokens) => println!("{:?}", tokens),
        Err(e) => eprintln!("{}", e),
    }
    Ok(())
}
