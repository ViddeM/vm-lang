use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::{fs, io, path::Path};
use syn::{parse_macro_input, LitStr};

extern crate proc_macro;

const TEST_PATH_PREFIX: &str = "vm_lang/";

const GR_FILE_TYPE: &str = "gr";
const EXPECTED_FILE_TYPE: &str = "expected";

#[proc_macro]
pub fn generate_tests(arg: TokenStream) -> TokenStream {
    let tests_path = parse_macro_input!(arg as LitStr).value();
    let test_progs = get_tests(format!("{}{}", TEST_PATH_PREFIX, tests_path));

    let mut funcs = quote! {};

    for prog in test_progs.into_iter() {
        let prog_name = prog
            .path
            .split("/")
            .last()
            .expect("Missing program name")
            .strip_suffix(format!(".{}", GR_FILE_TYPE).as_str())
            .expect("Missing suffix");
        let name = Ident::new(&format!("test_prog_{}", prog_name), Span::call_site());

        let prog_path = &prog.path;
        let prog_expected = &prog.expected;

        funcs = quote! {
            #funcs

            #[test]
            fn #name () {
                println!("\n\n============ Running test {} ============", #prog_path);

                let prog_path = String::from(#prog_path);
                let prog_expected = String::from(#prog_expected);
                match run_tests(prog_path, prog_expected) {
                    Ok(()) => {},
                    Err(e) => panic!("Failure, {}", e),
                }
            }
        };
    }

    let test_module = quote! {
        #[cfg(test)]
        mod generated_tests {
            use vm_lang::ProgramError;
            use std::fs::File;
            use std::io::Write;
            use std::path::Path;
            use std::{fs, io};
            use std::fmt::Display;

            struct TestError {
                program_error: ProgramError,
            }

            impl Display for TestError {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "Program error: {}", self.program_error)
                }
            }

            impl TestError {
                fn check_against_string(&self, str: &str) -> Result<(), String> {
                    match str {
                        "parse" => match self.program_error {
                            ProgramError::LalrpopError(_) => Ok(()),
                            _ => Err(format!(
                                "Invalid error, expected parse (lalrpop) {:?}",
                                self.program_error
                            )),
                        },
                        "typecheck" => match self.program_error {
                            ProgramError::TypeCheck(_) => Ok(()),
                            _ => Err(format!(
                                "Invalid error, expected typecheck {:?}",
                                self.program_error
                            )),
                        },
                        "interpret" => match self.program_error {
                            ProgramError::InterpretError(_) => Ok(()),
                            _ => Err(format!(
                                "Invalid error, expected interpration {:?}",
                                self.program_error
                            )),
                        },
                        s => Err(format!(
                            "Expected output {}, got error {:?}",
                            s, self.program_error
                        )),
                    }
                }
            }

            impl From<ProgramError> for TestError {
                fn from(e: ProgramError) -> Self {
                    Self { program_error: e }
                }
            }

            fn run_tests(prog_path: String, prog_expected: String) -> Result<(), String> {
                let mut output = String::new();
                match vm_lang::run_program(
                    &prog_path,
                    &mut |p| output.push_str(&format!("{}\n", p)),
                ) {
                    Ok(()) => {
                        let got = output
                            .strip_suffix("\n")
                            .ok_or(String::from("Failed to strip newline"))?;

                        if got != prog_expected {
                            panic!("\nExpected\n{}\n\nGot\n{}\n", prog_expected, got);
                        }
                    }
                    Err(e) => {
                        if let Err(e) = TestError::from(e).check_against_string(&prog_expected) {
                            panic!("\nExpected error {}\nGot {}\n", prog_expected, e);
                        }
                    }
                }
                Ok(())
            }

            #funcs
        }
    };

    TokenStream::from(test_module)
}

struct TestProgram {
    path: String,
    expected: String,
}

fn get_tests(tests_path: String) -> Vec<TestProgram> {
    let test_path = Path::new(&tests_path);
    fs::read_dir(test_path)
        .expect("Failed to read directory test_programs")
        .map(|p| {
            let p = p.expect("Invalid direntry");
            let file_name = p
                .file_name()
                .to_str()
                .expect("Failed to convert filename")
                .to_string();

            let split_src = file_name.clone();
            let path = p.path();
            let base_path = path
                .to_str()
                .expect("Failed to convert base path to string")
                .strip_suffix(&split_src)
                .expect("Failed to create base path");
            let split = split_src.split(".").collect::<Vec<&str>>();
            (
                format!("{}{}", base_path, file_name),
                format!(
                    "{}/{}",
                    base_path,
                    split.get(0).expect("Filename err").to_string()
                ),
                split.get(1).expect("Filetype err").to_string(),
            )
        })
        .filter(|(_, _, t)| t == GR_FILE_TYPE)
        .map(|(full_name, name, _)| {
            let expected_path = format!("{}.{}", name, EXPECTED_FILE_TYPE);
            Ok(TestProgram {
                path: full_name
                    .strip_prefix(TEST_PATH_PREFIX)
                    .expect("Missing test path prefix")
                    .to_string(),
                expected: fs::read_to_string(expected_path)?,
            })
        })
        .collect::<io::Result<Vec<TestProgram>>>()
        .expect("Failed to read test files")
}
