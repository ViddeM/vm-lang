use vm_lang;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let file_path = args.get(1).expect("Expected filepath argument");
    println!("Got filepath: {}", file_path);

    vm_lang::run_program(file_path, &mut |p| println!("{}", p), &mut || {
        let mut inp = String::new();
        std::io::stdin()
            .read_line(&mut inp)
            .expect("Failed to read input");
        match inp.strip_suffix("\n") {
            Some(s) => String::from(s),
            None => inp,
        }
    })
    .expect("Failure");
}
