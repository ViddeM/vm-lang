use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::{fs, io};
use vm_lang::ProgramError;

extern crate vm_lang;

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

const OUTPUT_TMP_FILE: &str = "__tmp_test_output.test";

#[test]
fn run_tests() {
    let test_programs = get_tests();
    for prog in test_programs.into_iter() {
        println!("======== Running test {} ========", prog.path);
        match run_test(prog) {
            Ok(_) => println!("Success!"),
            Err(e) => println!("Failure: {}", e),
        }
    }
}

fn run_test(prog: TestProgram) -> Result<(), String> {
    File::create(Path::new(OUTPUT_TMP_FILE))
        .or(Err(&format!(
            "Failed to open test output file {}",
            OUTPUT_TMP_FILE
        )))?
        .write_all("".as_bytes())
        .or(Err(&format!("Failed to write to test output file")))?;
    match vm_lang::run_program(
        &prog.path,
        Box::new(|p| {
            let mut file = fs::OpenOptions::new()
                .write(true)
                .append(true)
                .open(Path::new(OUTPUT_TMP_FILE))
                .expect(&format!(
                    "Failed to open test output file {}",
                    OUTPUT_TMP_FILE
                ));
            file.write_all(format!("{}\n", p).as_bytes()).unwrap();
        }),
    ) {
        Ok(()) => {
            let got = fs::read_to_string(OUTPUT_TMP_FILE).or(Err(&format!(
                "Failed to open test output file {}",
                OUTPUT_TMP_FILE
            )))?;
            let got = got
                .strip_suffix("\n")
                .ok_or(String::from("Failed to strip newline"))?;
            assert_eq!(&got, &prog.expected);
        }
        Err(e) => {
            assert_eq!(
                TestError::from(e).check_against_string(&prog.expected),
                Ok(())
            );
        }
    }
    Ok(())
}

struct TestProgram {
    path: String,
    expected: String,
}

const GR_FILE_TYPE: &str = "gr";
const EXPECTED_FILE_TYPE: &str = "expected";
fn get_tests() -> Vec<TestProgram> {
    let test_path = Path::new("./tests/test_programs");
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
                path: full_name,
                expected: fs::read_to_string(expected_path)?,
            })
        })
        .collect::<io::Result<Vec<TestProgram>>>()
        .expect("Failed to read test files")
}
