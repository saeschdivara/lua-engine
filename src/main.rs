use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long, alias = "i")]
    input_path: String,
}

fn main() {
    println!("{:?}", nom_tester::hex_color("#2F14DF"))
}