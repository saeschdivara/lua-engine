use clap::Parser;
use nom_tester::lua::parse_file;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long, alias = "i")]
    input_path: String,
}

fn main() {
    let args = Args::parse();
    parse_file(args.input_path.as_str()).unwrap();
}