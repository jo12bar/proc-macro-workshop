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
    #[builder(each = "arg")]
    args: Vec<String>,
    #[builder(each = "env")]
    env: Vec<String>,
    current_dir: Option<String>,
}

#[derive(Builder, Debug)]
pub(crate) struct Command2;

#[derive(Builder, Debug)]
pub(self) struct Command3(u32, u32, u32, Option<f64>);

fn main() {
    let mut builder = Command::builder();
    builder.executable("cargo".to_owned());
    builder
        .args(vec!["build".to_owned(), "--release".to_owned()])
        .arg("--".to_owned())
        .arg("--help".to_owned());
    builder.current_dir("..".to_owned());

    let command = builder.build().unwrap();
    assert_eq!(command.executable, "cargo");
    println!("executable: {}", command.executable);
    println!("args: {:?}", command.args);
    println!("env: {:?}", command.env);
    println!("current_dir: {:?}", command.current_dir);

    let mut builder = Command2::builder();
    let command2 = builder.build().unwrap();
    println!("{command2:?}");

    let mut builder = Command3::builder();
    builder.field_0(3).field_2(45).field_3(-0.34);
    builder.field_1(2);

    let command3 = builder.build().unwrap();
    println!("{command3:?}");
}
