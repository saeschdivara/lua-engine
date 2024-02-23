use clap::Parser;

use my_gamma_script::evaluation::interpreter::Interpreter;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    path: String,
}

fn main() {
    let args = Args::parse();

    let mut interpreter = Interpreter::new();
    if let Some(result) = interpreter.evaluate_file(args.path) {
        println!("Received result: {:?}", result);
    }
}
