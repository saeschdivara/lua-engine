use clap::Parser;

use lua_engine::evaluation::interpreter::Interpreter;
use lua_engine::evaluation::runtime::Runtime;
use lua_engine::evaluation::typing::{NumberType, Value};

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
