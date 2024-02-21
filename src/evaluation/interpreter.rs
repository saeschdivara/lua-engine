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
type EvalStatementResult = Result<(), EvalError>;

pub struct Scope {
    pub variables: HashMap<String, Value>,
}

pub struct Callstack {
    pub variables: Vec<HashMap<String, Value>>,
    pub return_triggered: bool
}

pub struct Interpreter {
    return_value: Option<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        return Self {
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

        let mut callstack = Callstack { variables: vec![HashMap::new()], return_triggered: false };
        match self.eval_all_statements(&statements, &mut callstack) {
            Ok(_) => {}
            Err(err) => {
                eprintln!("Failed eval: {}", err.message)
            }
        }
    }

    fn eval_all_statements(&mut self, statements: &Vec<Box<dyn Statement>>, callstack: &mut Callstack) -> EvalStatementResult {
        for statement in statements {
            match self.eval_statement(statement, callstack) {
                Ok(_) => {}
                Err(err) => return Err(err)
            }

            if statement.as_any().is::<ReturnStatement>() {
                return Ok(());
            }
        }

        return Ok(())
    }

    fn eval_statement(&mut self, stmt: &Box<dyn Statement>, callstack: &mut Callstack) -> EvalStatementResult {
        if let Some(func) = stmt.as_any().downcast_ref::<FunctionStatement>() {
            let function = func.function.clone();
            callstack.variables.last_mut().unwrap().insert(func.name.clone(), Function(function));
            Ok(())
        } else if let Some(call) = stmt.as_any().downcast_ref::<FunctionCallStatement>() {
            if let Some(call_expr) = call.call.as_any().downcast_ref::<CallExpression>() {
                match self.eval_call_expression(call_expr, callstack) {
                    Ok(_) => Ok(()),
                    Err(err) => {
                        Err(EvalError::new(format!("Failed call statement: {}", err.message)))
                    }
                }
            } else {
                Err(EvalError::new("Failed to get call expr".to_string()))
            }
        } else if let Some(if_stmt) = stmt.as_any().downcast_ref::<IfStatement>() {
            match self.eval_if_statement(if_stmt, callstack) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed if statement: {}", err.message)))
                }
            }
        } else if let Some(return_stmt) = stmt.as_any().downcast_ref::<ReturnStatement>() {
            match self.eval_return_statement(return_stmt, callstack) {
                Ok(val) => {
                    self.return_value = Some(val);
                    Ok(())
                }
                Err(err) => {
                    Err(EvalError::new(format!("Failed return statement: {}", err.message)))
                }
            }
        }
        else {
            Err(EvalError::new("Unknown statement type found".to_string()))
        }
    }

    fn eval_function(&mut self, function: Rc<FunctionExpression>, arguments: &Vec<Box<dyn Expression>>, callstack: &mut Callstack) -> EvalResult {
        let mut new_scope = HashMap::new();
        for i in 0..function.parameters.len() {
            let parameter = &function.parameters[i];
            let argument = &arguments[i];

            match self.eval_expression(argument, callstack) {
                Ok(val) => {
                    new_scope.insert(parameter.clone(), val);
                }
                Err(err) => return Err(err)
            }
        }

        callstack.variables.push(new_scope);
        match self.eval_all_statements(&function.block, callstack) {
            Ok(_) => {}
            Err(err) => return Err(err)
        }

        if self.return_value.is_none() { return Ok(Value::Nil) }
        let return_value = (&<Option<Value> as Clone>::clone(&self.return_value).unwrap()).clone();
        self.return_value = None;

        return Ok(return_value)
    }

    fn eval_if_statement(&mut self, stmt: &IfStatement, callstack: &mut Callstack) -> EvalResult {
        let is_true = self.eval_condition(&stmt.condition, callstack);

        if is_true {
            match self.eval_all_statements(&stmt.block, callstack) {
                Ok(_) => {}
                Err(err) => return Err(err)
            }
        } else {
            let mut elseif_executed = false;
            for elseif_block in &stmt.elseif_blocks {
                if let Some(elseif_stmt) = elseif_block.as_any().downcast_ref::<ElseIfStatement>() {
                    let is_true = self.eval_condition(&elseif_stmt.condition, callstack);
                    if is_true {
                        elseif_executed = true;

                        match self.eval_all_statements(&elseif_stmt.block, callstack) {
                            Ok(_) => {}
                            Err(err) => return Err(err)
                        }

                        break;
                    }
                }
            }

            if !elseif_executed {
                match self.eval_all_statements(&stmt.else_block, callstack) {
                    Ok(_) => {}
                    Err(err) => return Err(err)
                }
            }
        }

        return Ok(Value::Nil);
    }

    fn eval_return_statement(&mut self, stmt: &ReturnStatement, callstack: &mut Callstack) -> EvalResult {
        let result = self.eval_expression(&stmt.value, callstack);
        callstack.variables.pop();

        return result;
    }

    fn eval_expression(&mut self, expr: &Box<dyn Expression>, callstack: &mut Callstack) -> EvalResult {
        if let Some(int_expr) = expr.as_any().downcast_ref::<IntExpression>() {
            Ok(Value::Number(NumberType::Int(int_expr.value)))
        } else if let Some(ident_expr) = expr.as_any().downcast_ref::<IdentifierExpression>() {
            if let Some(val) = self.get_variable_value(&ident_expr.identifier, callstack) {
                Ok(val.clone())
            } else {
                Err(EvalError::new(format!("Could not find variable: {}", &ident_expr.identifier)))
            }
        } else if let Some(expr) = expr.as_any().downcast_ref::<PrefixExpression>() {
            self.eval_prefix_expression(expr)
        } else if let Some(expr) = expr.as_any().downcast_ref::<InfixExpression>() {
            self.eval_infix_expression(expr, callstack)
        } else if let Some(expr) = expr.as_any().downcast_ref::<CallExpression>() {
            self.eval_call_expression(expr, callstack)
        } else {
            Ok(Value::Nil)
        }
    }

    fn eval_call_expression(&mut self, expr: &CallExpression, callstack: &mut Callstack) -> EvalResult {
        match self.eval_expression(&expr.function, callstack) {
            Ok(function_val) => {
                if let Function(func) = function_val {
                    self.eval_function(func, &expr.arguments, callstack)
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

    fn eval_infix_expression(&mut self, expr: &InfixExpression, callstack: &mut Callstack) -> EvalResult {
        let left = match self.eval_expression(&expr.left_value, callstack) {
            Ok(val) => val,
            Err(err) => return Err(err),
        };

        let right = match self.eval_expression(&expr.right_value, callstack) {
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
            TokenType::Minus => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for minus".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Number(NumberType::Int(l - r)) })
                );
            }
            TokenType::Star => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for star".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Number(NumberType::Int(l * r)) })
                );
            }
            TokenType::Slash => {}
            TokenType::Percent => {}
            TokenType::Caret => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for caret".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Number(NumberType::Int(i64::pow(l, r as u32))) })
                );
            }
            TokenType::Hash => {}
            TokenType::Ampersand => {}
            TokenType::Tilde => {}
            TokenType::Bar => {}
            TokenType::ShiftLeft => {}
            TokenType::ShiftRight => {}
            TokenType::DoubleEquals => {
                return Ok(Value::Boolean(left.is_equals(&right)))
            }
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

    fn eval_condition(&mut self, condition: &Box<dyn Expression>, callstack: &mut Callstack) -> bool {
        let condition = self.eval_expression(condition, callstack);

        let condition_res = match condition {
            Ok(condition_res) => condition_res,
            _ => return false
        };

        return match condition_res {
            Value::Boolean(bool_val) => bool_val,
            _ => return false
        };
    }

    fn get_variable_value<'a>(&'a self, variable: &String, callstack: &'a mut Callstack) -> Option<&Value> {
        callstack.variables
            .iter()
            .rev()
            .find(|x| {x.contains_key(variable)})
            .map(move |t| {t.get(variable).unwrap()})
    }
}