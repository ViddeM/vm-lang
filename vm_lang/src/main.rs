use vm_lang;

fn main() {
    vm_lang::run_program("examples/to_parse.gr", &mut |p| println!("{}", p)).expect("Failure");
}
