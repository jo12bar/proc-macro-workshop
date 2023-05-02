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
    let mut builder = Command::builder();
    builder.executable("cargo".to_owned());
    builder.args(vec!["build".to_owned(), "--release".to_owned()]);
    builder.env(vec![]);
    builder.current_dir("..".to_owned());

    let _ = builder;

    let builder = Command2::builder();

    let _ = builder;

    let mut builder = Command3::builder();
    builder.field_0(3).field_2(45).field_3(-0.34);
    builder.field_1(2);

    let _ = builder;
}
