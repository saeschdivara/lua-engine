use clap::Parser;

use my_gamma_script::evaluation::interpreter::Interpreter;
use my_gamma_script::evaluation::runtime::Runtime;
use my_gamma_script::evaluation::typing::{NumberType, Value};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    path: String,
}

fn main() {
    let args = Args::parse();

    let mut interpreter = Interpreter::new();
    let mut runtime = Runtime::new();

    interpreter.prepare_runtime(&mut runtime);
    runtime.add_global_variable("my_global", Value::Number(NumberType::Int(100)));

    if let Some(result) = interpreter.evaluate_file(args.path, &mut runtime) {
        println!("Received result: {:?}", result);
    }
}
