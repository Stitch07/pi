use std::io::{self, BufRead, Write};

#[macro_use]
mod parser;
mod lexer;

use parser::Parser;

fn main() {
    println!("Welcome to pi");
    loop {
        print!(">> ");
        // print! doesn't flush the stdout, so doesn't write anything
        io::stdout().flush().ok();
        let stdin = io::stdin();
        let input = stdin.lock().lines().next().unwrap().unwrap();
        let mut parser = Parser::new(&input);
        match parser.compute() {
            Ok(result) => println!("{}", result),
            Err(e) => println!("an error occurred: {}", e),
        }
    }
}
