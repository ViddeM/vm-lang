use std::fs;

mod core_types;
mod type_checker;
mod typed_types;

use crate::type_checker::TypeCheckResult;
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(pub core);

fn main() {
    let to_parse = fs::read_to_string("examples/to_parse.vm").expect("Failed read input file");
    let parsed = core::PrgrParser::new()
        .parse(&to_parse)
        .expect("Failed to parse");
    println!("Parsed {:?}\n", parsed);

    let type_checked = type_checker::type_check(parsed).unwrap();
    println!("\n\n\nType checked {:#?}", type_checked)
}
