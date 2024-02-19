use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluation::typing::{NumberType, Value};
use crate::evaluation::typing::Value::Function;
use crate::parsing::ast::{CallExpression, Expression, FunctionCallStatement, FunctionExpression, FunctionStatement, IdentifierExpression, IntExpression, Statement};
use crate::parsing::lexer::TokenType;
use crate::parsing::parser::Parser;

pub struct EvalError {
    pub message: String,
}

impl EvalError {
    pub fn new(message: String) -> Self {
        Self {
            message,
        }
    }
}

type EvalResult = Result<Value, EvalError>;

pub struct Scope {
    pub variables: HashMap<String, Value>
}

pub struct Interpreter {
    global_scope: Scope,
}

impl Interpreter {
    pub fn new() -> Self {
        return Self {
            global_scope: Scope {
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
            self.global_scope.variables.insert(func.name.clone(), Function(function));
        }
        else if let Some(call) = stmt.as_any().downcast_ref::<FunctionCallStatement>() {
            if let Some(call_expr) = call.call.as_any().downcast_ref::<CallExpression>() {
                let scope = Scope { variables: HashMap::new() };
                match self.eval_expression(&call_expr.function, scope) {
                    Ok(function_val) => {
                        if let Function(func) = function_val {
                            let scope = Scope { variables: HashMap::new() };
                            self.eval_function(func, scope);
                        }
                    }
                    Err(err) => {
                        eprintln!("Failed to evaluate expression: {}", err.message);
                    }
                }

            } else {
                eprintln!("Failed to get call expr");
            }
        }
    }

    fn eval_function(&mut self, function: Rc<FunctionExpression>, scope: Scope) -> EvalResult {
        return Ok(Value::Nil);
    }

    fn eval_expression(&mut self, expr: &Box<dyn Expression>, scope: Scope) -> EvalResult {
        if let Some(int_expr) = expr.as_any().downcast_ref::<IntExpression>() {
            Ok(Value::Number(NumberType::Int(int_expr.value)))
        }
        else if let Some(ident_expr) = expr.as_any().downcast_ref::<IdentifierExpression>() {
            if let Some(val) = self.global_scope.variables.get(&ident_expr.identifier) {
                Ok(val.clone())
            } else {
                Err(EvalError::new(format!("Could not find variable: {}", &ident_expr.identifier)))
            }
        }
        else {
            Ok(Value::Nil)
        }
    }
}