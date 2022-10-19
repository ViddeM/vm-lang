use vm_lang;

fn main() {
    vm_lang::run_program("examples/to_parse.gr", Box::new(|p| println!("{}", p))).expect("Failure");
}
