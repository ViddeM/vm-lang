use std::fs;

mod core_types;
mod interpreter;
mod type_checker;
mod typed_types;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub core);

fn main() {
    let to_parse = fs::read_to_string("examples/to_parse.gr").expect("Failed read input file");
    let parsed = core::PrgrParser::new()
        .parse(&to_parse)
        .expect("Parse error");
    println!("Parsed {:?}\n", parsed);

    let type_checked = type_checker::type_check(parsed).expect("Typecheck error");
    println!("\n\nType checked {:?}\n", type_checked);

    println!("\n\n======Running Program======\n");
    interpreter::interpret(type_checked).expect("Interpretation error");
}
