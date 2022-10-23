use vm_lang;

fn main() {
    vm_lang::run_program(
        "examples/to_parse.gr",
        &mut |p| println!("{}", p),
        &mut || {
            let mut inp = String::new();
            std::io::stdin()
                .read_line(&mut inp)
                .expect("Failed to read input");
            match inp.strip_suffix("\n") {
                Some(s) => String::from(s),
                None => inp,
            }
        },
    )
    .expect("Failure");
}
