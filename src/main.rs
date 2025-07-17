use aot_compiler::lexer;
use std::panic::panic_any;
use std::path::Path;
use std::io::prelude::*;
use std::fs::File;

fn main() {
    let p = Path::new("main.txt");
    let mut file = match File::open(&p) {
        Err(why) => panic!("{:?}",why),
        Ok(file) => file
    };

    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => panic!("{:?}",why),
        Ok(_) => println!("Succesful read")
    }

    let mut l = lexer::Lexer::new(code);
    for t in l.tokenize().unwrap() {
        println!("{:?}",t)
    }
}
