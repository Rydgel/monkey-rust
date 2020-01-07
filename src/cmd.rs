#[derive(Debug)]
pub enum Command {
    FileReadCommand(String),
    RunInlineCode(String),
    Noop,
}

pub fn read_command() -> Command {
    let matches = clap_app!(monkey =>
        (version: "0.3.0")
        (author: "exfly <exflyg@gmail.com>")
        (about: "The Monkey programming language")
        (@setting ArgRequiredElseHelp)
        (@arg src: -s --src +takes_value "Path of the source file")
        (@arg run: -r --run +takes_value "Code you want to run inline")
    )
    .get_matches();

    let src_path = matches.value_of("src").map(|s| s.to_string());
    let run_string = matches.value_of("run").map(|s| s.to_string());
    match (src_path, run_string) {
        (Some(s), _) => Command::FileReadCommand(s),
        (_, Some(s)) => Command::RunInlineCode(s),
        _ => Command::Noop,
    }
}
