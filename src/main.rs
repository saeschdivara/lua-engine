use clap::Parser;
use my_gamma_script::parsing::lexer::{Lexer, TokenType};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    path: String,
}

fn main() {
    let args = Args::parse();

    let contents = std::fs::read_to_string(args.path)
        .expect("Should have been able to read the file");
    
    let mut lexer = Lexer::new(contents);
    while let tok = lexer.next_token() {
        if tok.token_type == TokenType::Eof { break }

        println!("{:?}", tok);
    }
}
