// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    env: Vec<String>,
    current_dir: String,
}

#[derive(Builder)]
pub(crate) struct Command2;

#[derive(Builder)]
pub(self) struct Command3(u32, u32, u32, f64);

fn main() {
    let builder = Command::builder();

    let _ = builder;

    let builder = Command2::builder();

    let _ = builder;

    let builder = Command3::builder();

    let _ = builder;
}
