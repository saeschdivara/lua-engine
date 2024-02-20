use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluation::typing::{NumberType, Value};
use crate::evaluation::typing::Value::Function;
use crate::parsing::ast::*;
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
type EvalBoolResult = Result<bool, EvalError>;

pub struct Scope {
    pub variables: HashMap<String, Value>,
}

pub struct Interpreter {
    global_scope: Scope,
    return_value: Option<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        return Self {
            global_scope: Scope {
                variables: HashMap::new(),
            },

            return_value: None,
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
            self.eval_statement(&statement);
        }
    }

    fn eval_statement(&mut self, stmt: &Box<dyn Statement>) {
        if let Some(func) = stmt.as_any().downcast_ref::<FunctionStatement>() {
            let function = func.function.clone();
            self.global_scope.variables.insert(func.name.clone(), Function(function));
        } else if let Some(call) = stmt.as_any().downcast_ref::<FunctionCallStatement>() {
            if let Some(call_expr) = call.call.as_any().downcast_ref::<CallExpression>() {
                match self.eval_call_expression(call_expr) {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("Failed function call statement: {}", err.message)
                    }
                }
            } else {
                eprintln!("Failed to get call expr");
            }
        } else if let Some(if_stmt) = stmt.as_any().downcast_ref::<IfStatement>() {
            match self.eval_if_statement(if_stmt) {
                Ok(_) => {}
                Err(err) => {
                    eprintln!("Failed if statement: {}", err.message)
                }
            }
        } else if let Some(return_stmt) = stmt.as_any().downcast_ref::<ReturnStatement>() {
            match self.eval_return_statement(return_stmt) {
                Ok(val) => {
                    self.return_value = Some(val);
                }
                Err(err) => {
                    eprintln!("Failed return statement: {}", err.message)
                }
            }
        }
    }

    fn eval_function(&mut self, function: Rc<FunctionExpression>, arguments: &Vec<Box<dyn Expression>>, scope: Scope) -> EvalResult {
        for i in 0..function.parameters.len() {
            let parameter = &function.parameters[i];
            let argument = &arguments[i];

            let scope = Scope { variables: HashMap::new() };

            match self.eval_expression(argument, scope) {
                Ok(val) => {
                    self.global_scope.variables.insert(parameter.clone(), val);
                }
                Err(err) => return Err(err)
            }
        }

        for stmt in &function.block {
            self.eval_statement(stmt);

            if stmt.as_any().is::<ReturnStatement>() {
                if let Some(return_value) = &self.return_value {
                    return Ok(return_value.clone());
                }

                return Ok(Value::Nil);
            }
        }

        return Ok(Value::Nil);
    }

    fn eval_if_statement(&mut self, stmt: &IfStatement) -> EvalResult {
        let is_true = self.eval_condition(&stmt.condition);

        if is_true {
            for statement in &stmt.block {
                self.eval_statement(statement);
            }
        } else {
            let mut elseif_executed = false;
            for elseif_block in &stmt.elseif_blocks {
                if let Some(elseif_stmt) = elseif_block.as_any().downcast_ref::<ElseIfStatement>() {
                    let is_true = self.eval_condition(&elseif_stmt.condition);
                    if is_true {
                        elseif_executed = true;

                        for statement in &stmt.block {
                            self.eval_statement(statement);
                        }
                        break;
                    }
                }
            }

            if !elseif_executed {
                for statement in &stmt.else_block {
                    self.eval_statement(&statement);
                }
            }
        }

        return Ok(Value::Nil);
    }

    fn eval_return_statement(&mut self, stmt: &ReturnStatement) -> EvalResult {
        let scope = Scope { variables: HashMap::new() };
        return self.eval_expression(&stmt.value, scope);
    }

    fn eval_expression(&mut self, expr: &Box<dyn Expression>, scope: Scope) -> EvalResult {
        if let Some(int_expr) = expr.as_any().downcast_ref::<IntExpression>() {
            Ok(Value::Number(NumberType::Int(int_expr.value)))
        } else if let Some(ident_expr) = expr.as_any().downcast_ref::<IdentifierExpression>() {
            if let Some(val) = self.global_scope.variables.get(&ident_expr.identifier) {
                Ok(val.clone())
            } else {
                Err(EvalError::new(format!("Could not find variable: {}", &ident_expr.identifier)))
            }
        } else if let Some(expr) = expr.as_any().downcast_ref::<PrefixExpression>() {
            self.eval_prefix_expression(expr)
        } else if let Some(expr) = expr.as_any().downcast_ref::<InfixExpression>() {
            self.eval_infix_expression(expr)
        } else if let Some(expr) = expr.as_any().downcast_ref::<CallExpression>() {
            self.eval_call_expression(expr)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_call_expression(&mut self, expr: &CallExpression) -> EvalResult {
        let scope = Scope { variables: HashMap::new() };
        match self.eval_expression(&expr.function, scope) {
            Ok(function_val) => {
                if let Function(func) = function_val {
                    let scope = Scope { variables: HashMap::new() };
                    return self.eval_function(func, &expr.arguments, scope)
                } else {
                    Err(EvalError::new("Unknown function error".to_string()))
                }
            }
            Err(err) => Err(err)
        }
    }

    fn eval_prefix_expression(&mut self, expr: &PrefixExpression) -> EvalResult {
        Ok(Value::Nil)
    }

    fn eval_infix_expression(&mut self, expr: &InfixExpression) -> EvalResult {
        let scope = Scope { variables: HashMap::new() };
        let left = match self.eval_expression(&expr.left_value, scope) {
            Ok(val) => val,
            Err(err) => return Err(err),
        };

        let scope = Scope { variables: HashMap::new() };
        let right = match self.eval_expression(&expr.right_value, scope) {
            Ok(val) => val,
            Err(err) => return Err(err),
        };

        match expr.operator {
            TokenType::Plus => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for plus".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Number(NumberType::Int(l + r)) })
                );
            }
            TokenType::Minus => {}
            TokenType::Star => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for plus".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Number(NumberType::Int(l * r)) })
                );
            }
            TokenType::Slash => {}
            TokenType::Percent => {}
            TokenType::Caret => {}
            TokenType::Hash => {}
            TokenType::Ampersand => {}
            TokenType::Tilde => {}
            TokenType::Bar => {}
            TokenType::ShiftLeft => {}
            TokenType::ShiftRight => {}
            TokenType::DoubleEquals => {}
            TokenType::TildeEqual => {}
            TokenType::LowerEqual => {}
            TokenType::GreaterEqual => {}
            TokenType::Lower => {}
            TokenType::Greater => {}
            TokenType::Dot => {}
            TokenType::DoubleDot => {}
            TokenType::TripleDot => {}
            TokenType::And => {}
            TokenType::Or => {}
            _ => return Err(EvalError::new(format!("Unsupported infix operator: {:?}", expr.operator.clone())))
        }
        Ok(Value::Nil)
    }

    fn eval_operation_on_int(&mut self, left: &Value, right: &Value, operation: fn(l: i64, r: i64) -> Value) -> Value {
        if let Value::Number(NumberType::Int(l)) = left {
            if let Value::Number(NumberType::Int(r)) = right {
                return operation(*l, *r);
            }
        }

        panic!("Should never reach this")
    }

    fn eval_condition(&mut self, condition: &Box<dyn Expression>) -> bool {
        let scope = Scope { variables: HashMap::new() };
        let condition = self.eval_expression(condition, scope);

        let condition_res = match condition {
            Ok(condition_res) => condition_res,
            _ => return false
        };

        return match condition_res {
            Value::Boolean(bool_val) => bool_val,
            _ => return false
        };
    }
}