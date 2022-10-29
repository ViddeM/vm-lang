use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::{fs, io, path::Path};
use syn::{parse_macro_input, LitStr};

extern crate proc_macro;

const TEST_READ_BASE_PATH: &str = "vm_lang";

const INPUT_FILES_DIRECTORY: &str = "test_program_inputs";
const TEST_PROGRAMS_DIRECTORY: &str = "test_programs";
const EXPECTED_FILES_DIRECTORY: &str = "test_programs_expected";

const GR_FILE_TYPE: &str = "gr";
const EXPECTED_FILE_TYPE: &str = "expected";
const INPUT_FILE_TYPE: &str = "input";

#[proc_macro]
pub fn generate_tests(arg: TokenStream) -> TokenStream {
    let tests_path = parse_macro_input!(arg as LitStr).value();
    let test_progs = get_tests(tests_path);

    let mut funcs = quote! {};

    for prog in test_progs.into_iter() {
        let name = Ident::new(&format!("test_prog_{}", prog.name), Span::call_site());

        let prog_path = &prog.path;
        let prog_expected = &prog.expected;
        let prog_input = prog.input.unwrap_or(String::new());

        funcs = quote! {
            #funcs

            #[test]
            fn #name () {
                println!("\n\n============ Running test {} ============", #prog_path);

                let prog_path = String::from(#prog_path);
                let prog_expected = String::from(#prog_expected);
                let mut prog_input = String::from(#prog_input)
                                    .split("\n")
                                    .map(|a| String::from(a))
                                    .collect::<Vec<String>>();
                prog_input.reverse();
                match run_tests(prog_path, prog_expected, &mut prog_input) {
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
                                "Invalid error, expected parse (lalrpop), got: {:?}",
                                self.program_error
                            )),
                        },
                        "typecheck" => match self.program_error {
                            ProgramError::TypeCheck(_) => Ok(()),
                            _ => Err(format!(
                                "Invalid error, expected typecheck, got: {:?}",
                                self.program_error
                            )),
                        },
                        "interpret" => match self.program_error {
                            ProgramError::InterpretError(_) => Ok(()),
                            _ => Err(format!(
                                "Invalid error, expected interpretation, got: {:?}",
                                self.program_error
                            )),
                        },
                        s => Err(format!(
                            "Expected output:\n{}\n\nGot error:\n{:?}",
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

            fn run_tests(prog_path: String, prog_expected: String, input: &mut Vec<String>) -> Result<(), String> {
                let mut output = String::new();
                match vm_lang::run_program(
                    &prog_path,
                    &mut |p| output.push_str(&format!("{}\n", p)),
                    &mut || return input.pop().expect("Not enough input data!"),
                ) {
                    Ok(()) => {
                        let got = output
                            .strip_suffix("\n")
                            .ok_or(String::from("Failed to strip newline"))?;

                        if got != prog_expected {
                            panic!("\nGot\n{}\n\nExpected\n{}\n", got, prog_expected);
                        }
                    }
                    Err(e) => {
                        if let Err(e) = TestError::from(e).check_against_string(&prog_expected) {
                            panic!("{}", e);
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

fn get_tests(internal_test_path: String) -> Vec<TestProgram> {
    let full_test_path = format!("{}/{}", TEST_READ_BASE_PATH, internal_test_path);

    let full_test_programs_path_name = format!("{}/{}", full_test_path, TEST_PROGRAMS_DIRECTORY);
    let test_path = Path::new(&full_test_programs_path_name);
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
            let split = split_src.split(".").collect::<Vec<&str>>();
            (
                file_name,
                split.get(0).expect("Filename err").to_string(), // Get filename without type ending
                split.get(1).expect("Filetype err").to_string(), // Get filetype
            )
        })
        .filter(|(_, _, t)| t == GR_FILE_TYPE)
        .map(|(full_file_name, file_name, _)| {
            TestProgram::new(
                file_name,
                full_file_name,
                &internal_test_path,
                &full_test_path,
            )
        })
        .collect::<io::Result<Vec<TestProgram>>>()
        .expect("Failed to read test files")
}

struct TestProgram {
    name: String,
    path: String,
    expected: String,
    input: Option<String>,
}

impl TestProgram {
    fn new<'a>(
        file_name: String,
        full_file_name: String,
        internal_test_path: &'a str,
        full_test_path: &'a str,
    ) -> io::Result<Self> {
        let program_path = format!(
            "{}/{}/{}",
            internal_test_path, TEST_PROGRAMS_DIRECTORY, full_file_name
        );
        let expected_path = format!(
            "{}/{}/{}.{}",
            full_test_path, EXPECTED_FILES_DIRECTORY, file_name, EXPECTED_FILE_TYPE
        );
        let input_path = format!(
            "{}/{}/{}.{}",
            full_test_path, INPUT_FILES_DIRECTORY, file_name, INPUT_FILE_TYPE
        );

        let input = if std::path::Path::new(&input_path).exists() {
            Some(fs::read_to_string(input_path)?)
        } else {
            None
        };

        Ok(TestProgram {
            name: file_name,
            path: program_path,
            expected: fs::read_to_string(expected_path)?,
            input: input,
        })
    }
}
