use clap::Parser;
use my_gamma_script::parsing::lexer::TokenType;
use my_gamma_script::parsing::parser::Parser as MyParser;

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
    
    let mut p = MyParser::new(contents);

    match p.parse_program(vec![TokenType::Eof]) {
        Ok(program) => {
            program.statements
                .iter()
                .for_each(|stmt| { println!("{}", stmt.to_string()); });
        }
        Err(err) => {
            eprintln!("Failed to parse program: {}", err.message);
        }
    }
}
