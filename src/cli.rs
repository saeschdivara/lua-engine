use std::error::Error;
use lua_engine::evaluation::interpreter::Interpreter;
use lua_engine::evaluation::runtime::Runtime;

fn main() -> Result<(), Box<dyn Error>> {
    let mut interpreter = Interpreter::new();
    let mut runtime = Runtime::new();

    interpreter.prepare_runtime(&mut runtime);

    let args = std::env::args().collect::<Vec<String>>();
    let path = args.get(1).unwrap();
    interpreter.evaluate_file(path.clone(), &mut runtime);

    Ok(())
}