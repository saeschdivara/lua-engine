use std::collections::HashMap;
use std::rc::Rc;

use crate::evaluation::typing::*;
use crate::evaluation::typing::Value::Function;
use crate::parsing::ast::*;
use crate::parsing::lexer::TokenType;
use crate::parsing::parser::Parser;

pub struct Scope {
    pub variables: HashMap<String, Value>,
}

pub struct Callstack {
    pub variables: Vec<HashMap<String, Value>>,
    pub return_triggered: bool
}

impl Callstack {
    pub fn add_new_layer(&mut self) {
        self.variables.push(HashMap::new());
    }
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

        let my_print = |args: &Vec<Value>| -> EvalResult {
            args.iter().for_each(|v| {print!("{:?} ", v);});
            println!();
            Ok(Value::Nil)
        };
        callstack.variables.first_mut().unwrap().insert("print".to_string(), Function(FunctionType::Native(my_print)));

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
            callstack.variables.last_mut().unwrap().insert(func.name.clone(), Function(FunctionType::Expression(function)));
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
                    callstack.return_triggered = true;
                    self.return_value = Some(val);
                    Ok(())
                }
                Err(err) => {
                    Err(EvalError::new(format!("Failed return statement: {}", err.message)))
                }
            }
        } else if let Some(stmt) = stmt.as_any().downcast_ref::<AssignmentStatement>() {
            match self.eval_assignment_statement(stmt, callstack) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed assignment statement: {}", err.message)))
                }
            }
        } else if let Some(stmt) = stmt.as_any().downcast_ref::<LoopStatement>() {
            match self.eval_loop_statement(stmt, callstack) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed loop statement: {}", err.message)))
                }
            }
        } else if let Some(stmt) = stmt.as_any().downcast_ref::<ForStatement>() {
            match self.eval_for_statement(stmt, callstack) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed for statement: {}", err.message)))
                }
            }
        }
        else {
            Err(EvalError::new(format!("Unknown statement type found: {}", stmt.to_string())))
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
        callstack.return_triggered = false;

        return Ok(return_value)
    }

    fn eval_native_function(&mut self, function: NativeFunc, arguments: &Vec<Box<dyn Expression>>, callstack: &mut Callstack) -> EvalResult {
        let mut evaluated_args = vec![];

        for argument in arguments {
            match self.eval_expression(argument, callstack) {
                Ok(val) => {
                    evaluated_args.push(val);
                }
                Err(err) => return Err(err)
            }
        }

        return function(&evaluated_args)
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

    fn eval_loop_statement(&mut self, stmt: &LoopStatement, callstack: &mut Callstack) -> EvalResult {
        match stmt.loop_type {
            LoopType::While => {
                loop {
                    match self.eval_expression(&stmt.condition, callstack) {
                        Ok(condition) => {
                            if !condition.is_boolean() { return Err(EvalError::new(format!("While condition evaluates to non-bool value: {:?}", condition))) }

                            let Value::Boolean(condition) = condition else { todo!() };
                            if condition {
                                match self.eval_all_statements(&stmt.block, callstack) {
                                    Ok(_) => {
                                        if callstack.return_triggered { break }
                                    }
                                    Err(err) => return Err(err)
                                }
                            } else {
                                break
                            }
                        }
                        Err(err) => return Err(err)
                    }
                }
            },
            LoopType::Repeat => {
                loop {
                    match self.eval_all_statements(&stmt.block, callstack) {
                        Ok(_) => {
                            if callstack.return_triggered { break }
                        }
                        Err(err) => return Err(err)
                    }

                    match self.eval_expression(&stmt.condition, callstack) {
                        Ok(condition) => {
                            if !condition.is_boolean() { return Err(EvalError::new(format!("While condition evaluates to non-bool value: {:?}", condition))) }

                            let Value::Boolean(condition) = condition else { todo!() };
                            if condition {
                                break
                            }
                        }
                        Err(err) => return Err(err)
                    }
                }
            },
        }

        return Ok(Value::Nil);
    }

    fn eval_for_statement(&mut self, stmt: &ForStatement, callstack: &mut Callstack) -> EvalResult {
        callstack.add_new_layer();

        let variable_name = stmt.initial_variable
            .as_any()
            .downcast_ref::<AssignmentStatement>()
            .unwrap().variable.literal
            .clone();

        match self.eval_statement(&stmt.initial_variable, callstack) {
            Ok(_) => {},
            Err(err) => return Err(err)
        }

        let value_to_stop = match self.eval_expression(&stmt.end_value, callstack) {
            Ok(val) => { val }
            Err(err) => { return Err(err) }
        };

        if !value_to_stop.is_number() { return Err(EvalError::new(format!("For loop stop value is not a number: {:?}", value_to_stop))) }

        let increment_value = match self.eval_expression(&stmt.increment_value, callstack) {
            Ok(val) => { val }
            Err(err) => { return Err(err) }
        };

        if !increment_value.is_number() { return Err(EvalError::new(format!("For loop increment value is not a number: {:?}", increment_value))) }

        loop {
            let current_counter_val = self.get_variable_value(&variable_name, callstack).unwrap();
            let will_stop = current_counter_val.is_equals(&value_to_stop);

            match self.eval_all_statements(&stmt.block, callstack) {
                Ok(_) => {
                    if callstack.return_triggered { break }
                }
                Err(err) => { return Err(err) }
            }

            let current_counter_val = self.get_variable_value(&variable_name, callstack).unwrap();
            let new_counter_val = current_counter_val.plus(&increment_value);
            callstack.variables.last_mut().unwrap().insert(variable_name.clone(), new_counter_val);

            if will_stop { break }
        }

        callstack.variables.pop();

        return Ok(Value::Nil);
    }

    fn eval_assignment_statement(&mut self, stmt: &AssignmentStatement, callstack: &mut Callstack) -> EvalResult {
        let result = match self.eval_expression(&stmt.value, callstack) {
            Ok(val) => val,
            Err(err) => return Err(err)
        };

        let variable_name = &stmt.variable.literal;

        match stmt.assignment_type {
            AssignmentType::Local => {
                match self.get_variable_value(variable_name, callstack) {
                    None => {
                        callstack.variables.last_mut().unwrap().insert(variable_name.clone(), result);
                    }
                    Some(_) => {
                        // currently considered not allowed
                        return Err(EvalError::new(format!("Cannot redeclare variable: {}", variable_name)))
                    }
                }
            }
            AssignmentType::Reassignment => {
                callstack.variables.last_mut().unwrap().insert(variable_name.clone(), result);
            }
        }

        return Ok(Value::Nil);
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
        } else if let Some(expr) = expr.as_any().downcast_ref::<StringExpression>() {
            Ok(Value::String(expr.value.clone()))
        } else if let Some(expr) = expr.as_any().downcast_ref::<PrefixExpression>() {
            self.eval_prefix_expression(expr, callstack)
        } else if let Some(expr) = expr.as_any().downcast_ref::<InfixExpression>() {
            self.eval_infix_expression(expr, callstack)
        } else if let Some(expr) = expr.as_any().downcast_ref::<CallExpression>() {
            self.eval_call_expression(expr, callstack)
        } else {
            Err(EvalError::new(format!("Unknown expression: {}", expr.to_string())))
        }
    }

    fn eval_call_expression(&mut self, expr: &CallExpression, callstack: &mut Callstack) -> EvalResult {
        match self.eval_expression(&expr.function, callstack) {
            Ok(function_val) => {
                if let Function(func_type) = function_val {
                    match func_type {
                        FunctionType::Native(func) => self.eval_native_function(func, &expr.arguments, callstack),
                        FunctionType::Expression(func) => self.eval_function(func, &expr.arguments, callstack),
                    }
                } else {
                    Err(EvalError::new("Unknown function error".to_string()))
                }
            }
            Err(err) => Err(err)
        }
    }

    fn eval_prefix_expression(&mut self, expr: &PrefixExpression, callstack: &mut Callstack) -> EvalResult {
        let value = match self.eval_expression(&expr.value, callstack) {
            Ok(val) => val,
            Err(err) => return Err(err),
        };

        match expr.operator {
            TokenType::Minus => {
                if !value.is_number() { return Err(EvalError::new(format!("- is not supported as prefix for non-numbers: {:?}", value))) }

                let minus_factor = Value::Number(NumberType::Int(-1));
                Ok(value.multiply(&minus_factor))
            }
            _ => return Err(EvalError::new(format!("Used unknown prefix operator: {:?}", expr.operator)))
        }
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
            TokenType::LowerEqual => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for < operator".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Boolean(l <= r) })
                );
            }
            TokenType::GreaterEqual => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for < operator".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Boolean(l >= r) })
                );
            }
            TokenType::Lower => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for < operator".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Boolean(l < r) })
                );
            }
            TokenType::Greater => {
                if !left.is_number() || !right.is_number() {
                    return Err(EvalError::new("Wrong type used for < operator".to_string()));
                }

                return Ok(self.eval_operation_on_int(
                    &left, &right,
                    |l, r| { Value::Boolean(l > r) })
                );
            }
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