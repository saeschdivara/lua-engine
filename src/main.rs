use clap::Parser;
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
    let program = p.parse_program();

    program.statements
        .iter()
        .for_each(|stmt| { println!("{}", stmt.to_string()); });
}
