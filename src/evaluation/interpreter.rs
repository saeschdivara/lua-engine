use std::collections::HashMap;
use crate::evaluation::typing::Value;
use crate::evaluation::typing::Value::Function;
use crate::parsing::ast::{FunctionCallStatement, FunctionStatement, Statement};
use crate::parsing::lexer::TokenType;
use crate::parsing::parser::Parser;

pub struct State {
    pub variables: HashMap<String, Value>
}

pub struct Interpreter {
    global_state: State,
}

impl Interpreter {
    pub fn new() -> Self {
        return Self {
            global_state: State {
                variables: HashMap::new(),
            },
        };
    }

    pub fn evaluate_file(&mut self, file_path: String) {
        let contents = std::fs::read_to_string(file_path)
            .expect("Should have been able to read the file");

        let mut p = Parser::new(contents);

        let statements = match p.parse_program(vec![TokenType::Eof]) {
            Ok(program) => { program.statements }
            Err(err) => {
                eprintln!(
                    "Failed to parse program: {} [{}:{}] {}",
                    err.file_path, err.line, err.column, err.message
                );
                return;
            }
        };

        for statement in statements {
            self.eval_statement(statement);
        }
    }

    fn eval_statement(&mut self, stmt: Box<dyn Statement>) {
        if let Some(func) = stmt.as_any().downcast_ref::<FunctionStatement>() {
            let function = func.function.clone();
            self.global_state.variables.insert(func.name.clone(), Function(function));
        }
        else if let Some(call) = stmt.as_any().downcast_ref::<FunctionCallStatement>() {
        }
    }
}