use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use std::{fs, io, path::Path};
use syn::{parse_macro_input, LitStr};

extern crate proc_macro;

const TEST_PATH_PREFIX: &str = "vm_lang/";

#[proc_macro]
pub fn generate_tests(arg: TokenStream) -> TokenStream {
    let tests_path = parse_macro_input!(arg as LitStr).value();
    let test_progs = get_tests(format!("{}{}", TEST_PATH_PREFIX, tests_path));

    let mut funcs = quote! {};

    let run_tests_func_name = Ident::new("run_test", Span::call_site());

    for prog in test_progs.into_iter() {
        let prog_name = prog
            .path
            .split("/")
            .last()
            .expect("Missing program name")
            .strip_suffix(format!(".{}", GR_FILE_TYPE).as_str())
            .expect("Missing suffix");
        let name = Ident::new(&format!("test_prog_{}", prog_name), Span::call_site());
        let helper_name = Ident::new(
            &format!("test_prog_{}_helper", prog_name),
            Span::call_site(),
        );

        let prog_path = &prog.path;
        let prog_expected = &prog.expected;

        funcs = quote! {
            #funcs

            #[test]
            fn #name () {
                println!("\n\n============ Running test {} ============", #prog_path);

                let prog_path = String::from(#prog_path);
                let prog_expected = String::from(#prog_expected);
                match #helper_name(prog_path, prog_expected) {
                    Ok(()) => {},
                    Err(e) => println!("Failure, {}", e),
                }
            }

            fn #helper_name (prog_path: String, prog_expected: String) -> Result<(), String> {
                let file_name_without_extension = prog_path
                    .strip_suffix(".gr")
                    .expect("Invalid program path");
                let tmp_output_path = format!("{}_{}", file_name_without_extension, OUTPUT_TMP_FILE);

                File::create(Path::new(&tmp_output_path))
                        .or(Err(&format!(
                            "Failed to open test output file {}",
                            tmp_output_path
                        )))?
                        .write_all("".as_bytes())
                        .or(Err("Failed to write to test output file"))?;
                match vm_lang::run_program(
                    &prog_path,
                    Box::new(|p| {
                        let prog_path = String::from(#prog_path);
                        let file_path_name = prog_path
                                    .strip_suffix(".gr")
                                    .expect("Invalid program path");
                        let path = format!("{}_{}", file_path_name, OUTPUT_TMP_FILE);

                        let mut file = fs::OpenOptions::new()
                            .write(true)
                            .append(true)
                            .open(Path::new(&path))
                            .expect(&format!(
                                "Failed to open test output file {}",
                                &file_path_name
                        ));
                        file.write_all(format!("{}\n", p).as_bytes()).unwrap();
                    }),
                ) {
                    Ok(()) => {
                        let got = fs::read_to_string(&tmp_output_path).or(Err(&format!(
                            "Failed to open test output file {}",
                            &tmp_output_path
                        )))?;
                        let got = got
                            .strip_suffix("\n")
                            .ok_or(String::from("Failed to strip newline"))?;
                        println!("Got\n{}\n\nExpected\n{}\n", got, prog_expected);
                        assert_eq!(&got, &prog_expected);
                    }
                    Err(e) => {
                        assert_eq!(
                        TestError::from(e).check_against_string(&prog_expected),
                        Ok(())
                    );
                    }
                }
                Ok(())
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

            const OUTPUT_TMP_FILE: &str = "__tmp_test_output.test";

            struct TestError {
                program_error: ProgramError,
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

            #funcs
        }
    };

    TokenStream::from(test_module)
}

struct TestProgram {
    path: String,
    expected: String,
}

const GR_FILE_TYPE: &str = "gr";
const EXPECTED_FILE_TYPE: &str = "expected";
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
