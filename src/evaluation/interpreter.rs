use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluation::runtime::Runtime;

use crate::evaluation::standard_lib::add_standard_functions;
use crate::evaluation::typing::*;
use crate::evaluation::typing::Value::Function;
use crate::parsing::ast::*;
use crate::parsing::lexer::TokenType;
use crate::parsing::parser::Parser;

pub struct Interpreter {
    return_value: Option<Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        return Self {
            return_value: None,
        };
    }

    pub fn evaluate_file(&mut self, file_path: String) -> &Option<Value> {
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
                return &None;
            }
        };

        let mut runtime = Runtime::new();
        add_standard_functions(&mut runtime);

        match self.eval_all_statements(&statements, &mut runtime) {
            Ok(_) => &self.return_value,
            Err(err) => {
                eprintln!("Failed eval: {}", err.message);
                &None
            }
        }
    }

    fn eval_all_statements(&mut self, statements: &Vec<Box<dyn Statement>>, runtime: &mut Runtime) -> EvalStatementResult {
        for statement in statements {
            match self.eval_statement(statement, runtime) {
                Ok(_) => {}
                Err(err) => return Err(err)
            }

            if statement.as_any().is::<ReturnStatement>() {
                return Ok(());
            }
        }

        return Ok(());
    }

    fn eval_statement(&mut self, stmt: &Box<dyn Statement>, runtime: &mut Runtime) -> EvalStatementResult {
        if let Some(func) = stmt.as_any().downcast_ref::<FunctionStatement>() {
            let function = func.function.clone();
            runtime.stack.last_mut().unwrap().insert(func.name.clone(), Function(FunctionType::Expression(function)));
            Ok(())
        } else if let Some(call) = stmt.as_any().downcast_ref::<FunctionCallStatement>() {
            if let Some(call_expr) = call.call.as_any().downcast_ref::<CallExpression>() {
                match self.eval_call_expression(call_expr, runtime) {
                    Ok(_) => Ok(()),
                    Err(err) => {
                        Err(EvalError::new(format!("Failed call statement: {}", err.message)))
                    }
                }
            } else {
                Err(EvalError::new("Failed to get call expr".to_string()))
            }
        } else if let Some(if_stmt) = stmt.as_any().downcast_ref::<IfStatement>() {
            match self.eval_if_statement(if_stmt, runtime) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed if statement: {}", err.message)))
                }
            }
        } else if let Some(return_stmt) = stmt.as_any().downcast_ref::<ReturnStatement>() {
            match self.eval_return_statement(return_stmt, runtime) {
                Ok(val) => {
                    runtime.return_triggered = true;
                    self.return_value = Some(val);
                    Ok(())
                }
                Err(err) => {
                    Err(EvalError::new(format!("Failed return statement: {}", err.message)))
                }
            }
        } else if let Some(stmt) = stmt.as_any().downcast_ref::<AssignmentStatement>() {
            match self.eval_assignment_statement(stmt, runtime) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed assignment statement: {}", err.message)))
                }
            }
        } else if let Some(stmt) = stmt.as_any().downcast_ref::<TablePropertyAssignmentStatement>() {
            match self.eval_property_assignment_statement(stmt, runtime) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed property assignment statement: {}", err.message)))
                }
            }
        } else if let Some(stmt) = stmt.as_any().downcast_ref::<LoopStatement>() {
            match self.eval_loop_statement(stmt, runtime) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed loop statement: {}", err.message)))
                }
            }
        } else if let Some(stmt) = stmt.as_any().downcast_ref::<ForStatement>() {
            match self.eval_for_statement(stmt, runtime) {
                Ok(_) => Ok(()),
                Err(err) => {
                    Err(EvalError::new(format!("Failed for statement: {}", err.message)))
                }
            }
        } else {
            Err(EvalError::new(format!("Unknown statement type found: {}", stmt.to_string())))
        }
    }

    fn eval_function(&mut self, function: &Rc<FunctionExpression>, arguments: &Vec<Value>, runtime: &mut Runtime) -> EvalResult {
        let mut new_scope = HashMap::new();
        for i in 0..function.parameters.len() {
            let parameter = &function.parameters[i];
            let val = &arguments[i];

            new_scope.insert(parameter.clone(), val.clone());
        }

        runtime.stack.push(new_scope);
        match self.eval_all_statements(&function.block, runtime) {
            Ok(_) => {}
            Err(err) => return Err(err)
        }

        if self.return_value.is_none() { return Ok(Value::Nil); }
        let return_value = (&<Option<Value> as Clone>::clone(&self.return_value).unwrap()).clone();
        self.return_value = None;
        runtime.return_triggered = false;

        return Ok(return_value);
    }

    fn eval_native_function(&mut self, function: &NativeFunc, arguments: &Vec<Value>, runtime: &mut Runtime) -> EvalResult {
        return function(arguments, runtime);
    }

    fn eval_native_mutable_function(&mut self, function: &NativeMutableFunc, arguments: &mut Vec<Value>, runtime: &mut Runtime) -> EvalResult {
        return function(arguments, runtime);
    }

    fn eval_expressions(&mut self, arguments: &Vec<Box<dyn Expression>>, runtime: &mut Runtime) -> EvalListResult {
        let mut evaluated_values = vec![];

        for argument in arguments {
            match self.eval_expression(argument, runtime) {
                Ok(val) => {
                    evaluated_values.push(val);
                }
                Err(err) => return Err(err)
            }
        }

        return Ok(evaluated_values);
    }

    fn eval_if_statement(&mut self, stmt: &IfStatement, runtime: &mut Runtime) -> EvalResult {
        let is_true = self.eval_condition(&stmt.condition, runtime);

        if is_true {
            match self.eval_all_statements(&stmt.block, runtime) {
                Ok(_) => {}
                Err(err) => return Err(err)
            }
        } else {
            let mut elseif_executed = false;
            for elseif_block in &stmt.elseif_blocks {
                if let Some(elseif_stmt) = elseif_block.as_any().downcast_ref::<ElseIfStatement>() {
                    let is_true = self.eval_condition(&elseif_stmt.condition, runtime);
                    if is_true {
                        elseif_executed = true;

                        match self.eval_all_statements(&elseif_stmt.block, runtime) {
                            Ok(_) => {}
                            Err(err) => return Err(err)
                        }

                        break;
                    }
                }
            }

            if !elseif_executed {
                match self.eval_all_statements(&stmt.else_block, runtime) {
                    Ok(_) => {}
                    Err(err) => return Err(err)
                }
            }
        }

        return Ok(Value::Nil);
    }

    fn eval_return_statement(&mut self, stmt: &ReturnStatement, runtime: &mut Runtime) -> EvalResult {
        let result = self.eval_expression(&stmt.value, runtime);
        runtime.stack.pop();

        return result;
    }

    fn eval_loop_statement(&mut self, stmt: &LoopStatement, runtime: &mut Runtime) -> EvalResult {
        match stmt.loop_type {
            LoopType::While => {
                loop {
                    match self.eval_expression(&stmt.condition, runtime) {
                        Ok(condition) => {
                            if !condition.is_boolean() { return Err(EvalError::new(format!("While condition evaluates to non-bool value: {:?}", condition))); }

                            let Value::Boolean(condition) = condition else { todo!() };
                            if condition {
                                match self.eval_all_statements(&stmt.block, runtime) {
                                    Ok(_) => {
                                        if runtime.return_triggered { break; }
                                    }
                                    Err(err) => return Err(err)
                                }
                            } else {
                                break;
                            }
                        }
                        Err(err) => return Err(err)
                    }
                }
            }
            LoopType::Repeat => {
                loop {
                    match self.eval_all_statements(&stmt.block, runtime) {
                        Ok(_) => {
                            if runtime.return_triggered { break; }
                        }
                        Err(err) => return Err(err)
                    }

                    match self.eval_expression(&stmt.condition, runtime) {
                        Ok(condition) => {
                            if !condition.is_boolean() { return Err(EvalError::new(format!("While condition evaluates to non-bool value: {:?}", condition))); }

                            let Value::Boolean(condition) = condition else { todo!() };
                            if condition {
                                break;
                            }
                        }
                        Err(err) => return Err(err)
                    }
                }
            }
        }

        return Ok(Value::Nil);
    }

    fn eval_for_statement(&mut self, stmt: &ForStatement, runtime: &mut Runtime) -> EvalResult {
        runtime.add_new_layer();

        let variable_name = stmt.initial_variable
            .as_any()
            .downcast_ref::<AssignmentStatement>()
            .unwrap().variable.literal
            .clone();

        match self.eval_statement(&stmt.initial_variable, runtime) {
            Ok(_) => {}
            Err(err) => return Err(err)
        }

        let value_to_stop = match self.eval_expression(&stmt.end_value, runtime) {
            Ok(val) => { val }
            Err(err) => { return Err(err); }
        };

        if !value_to_stop.is_number() { return Err(EvalError::new(format!("For loop stop value is not a number: {:?}", value_to_stop))); }

        let increment_value = match self.eval_expression(&stmt.increment_value, runtime) {
            Ok(val) => { val }
            Err(err) => { return Err(err); }
        };

        if !increment_value.is_number() { return Err(EvalError::new(format!("For loop increment value is not a number: {:?}", increment_value))); }

        loop {
            let current_counter_val = self.get_variable_value(&variable_name, runtime).unwrap();
            let will_stop = current_counter_val.is_equals(&value_to_stop);

            match self.eval_all_statements(&stmt.block, runtime) {
                Ok(_) => {
                    if runtime.return_triggered { break; }
                }
                Err(err) => { return Err(err); }
            }

            let current_counter_val = self.get_variable_value(&variable_name, runtime).unwrap();
            let new_counter_val = current_counter_val.plus(&increment_value);
            runtime.stack.last_mut().unwrap().insert(variable_name.clone(), new_counter_val);

            if will_stop { break; }
        }

        runtime.stack.pop();

        return Ok(Value::Nil);
    }

    fn eval_assignment_statement(&mut self, stmt: &AssignmentStatement, runtime: &mut Runtime) -> EvalResult {
        let result = match self.eval_expression(&stmt.value, runtime) {
            Ok(val) => val,
            Err(err) => return Err(err)
        };

        let variable_name = &stmt.variable.literal;

        match stmt.assignment_type {
            AssignmentType::Local => {
                match self.get_variable_value(variable_name, runtime) {
                    None => {
                        runtime.stack.last_mut().unwrap().insert(variable_name.clone(), result);
                    }
                    Some(_) => {
                        // currently considered not allowed
                        return Err(EvalError::new(format!("Cannot redeclare variable: {}", variable_name)));
                    }
                }
            }
            AssignmentType::Reassignment => {
                runtime.stack.last_mut().unwrap().insert(variable_name.clone(), result);
            }
        }

        return Ok(Value::Nil);
    }

    fn eval_property_assignment_statement(&mut self, stmt: &TablePropertyAssignmentStatement, runtime: &mut Runtime) -> EvalResult {
        let assigned_value = match self.eval_expression(&stmt.value, runtime) {
            Ok(val) => val,
            Err(err) => return Err(err)
        };

        runtime.insert_table_property_via_variable(&stmt.table_variable, &stmt.property_name, assigned_value);

        Ok(Value::Nil)
    }

    fn eval_expression(&mut self, expr: &Box<dyn Expression>, runtime: &mut Runtime) -> EvalResult {
        if let Some(int_expr) = expr.as_any().downcast_ref::<IntExpression>() {
            Ok(Value::Number(NumberType::Int(int_expr.value)))
        } else if let Some(expr) = expr.as_any().downcast_ref::<TableExpression>() {
            self.eval_table_expression(expr, runtime)
        } else if let Some(expr) = expr.as_any().downcast_ref::<FunctionWrapperExpression>() {
            Ok(Function(FunctionType::Expression(expr.func.clone())))
        } else if let Some(ident_expr) = expr.as_any().downcast_ref::<IdentifierExpression>() {
            if let Some(val) = self.get_variable_value(&ident_expr.identifier, runtime) {
                Ok(val.clone())
            } else {
                Err(EvalError::new(format!("Could not find variable: {}", &ident_expr.identifier)))
            }
        } else if let Some(expr) = expr.as_any().downcast_ref::<StringExpression>() {
            Ok(Value::String(expr.value.clone()))
        } else if let Some(expr) = expr.as_any().downcast_ref::<PrefixExpression>() {
            self.eval_prefix_expression(expr, runtime)
        } else if let Some(expr) = expr.as_any().downcast_ref::<InfixExpression>() {
            self.eval_infix_expression(expr, runtime)
        } else if let Some(expr) = expr.as_any().downcast_ref::<CallExpression>() {
            self.eval_call_expression(expr, runtime)
        } else {
            Err(EvalError::new(format!("Unknown expression: {}", expr.to_string())))
        }
    }

    fn eval_table_expression(&mut self, expr: &TableExpression, runtime: &mut Runtime) -> EvalResult {
        let mut values = vec![];
        let mut properties = HashMap::new();

        for value_expr in &expr.values {
            if let Some(assignment) = value_expr.as_any().downcast_ref::<AssignmentExpression>() {
                match self.eval_expression(&assignment.value, runtime) {
                    Ok(val) => {
                        properties.insert(assignment.variable.clone(), val);
                    }
                    Err(err) => return Err(err)
                }
            } else {
                match self.eval_expression(value_expr, runtime) {
                    Ok(value) => { values.push(value) }
                    Err(err) => return Err(err)
                }
            }
        }

        Ok(Value::Table(runtime.create_object(ObjectValue::table(TableData { values, properties, meta_table: None }))))
    }

    fn eval_call_expression(&mut self, expr: &CallExpression, runtime: &mut Runtime) -> EvalResult {
        match self.eval_expression(&expr.function, runtime) {
            Ok(function_val) => {
                if let Function(func_type) = function_val {
                    let mut arguments = match self.eval_expressions(&expr.arguments, runtime) {
                        Ok(args) => args,
                        Err(err) => return Err(err)
                    };

                    match func_type {
                        FunctionType::Native(func) => self.eval_native_function(&func, &arguments, runtime),
                        FunctionType::NativeMutable(func) => self.eval_native_mutable_function(&func, &mut arguments, runtime),
                        FunctionType::Expression(func) => self.eval_function(&func, &arguments, runtime),
                    }
                } else {
                    Err(EvalError::new("Unknown function error".to_string()))
                }
            }
            Err(err) => Err(err)
        }
    }

    fn eval_prefix_expression(&mut self, expr: &PrefixExpression, runtime: &mut Runtime) -> EvalResult {
        let value = match self.eval_expression(&expr.value, runtime) {
            Ok(val) => val,
            Err(err) => return Err(err),
        };

        match expr.operator {
            TokenType::Minus => {
                if !value.is_number() { return Err(EvalError::new(format!("- is not supported as prefix for non-numbers: {:?}", value))); }

                let minus_factor = Value::Number(NumberType::Int(-1));
                Ok(value.multiply(&minus_factor))
            }
            _ => return Err(EvalError::new(format!("Used unknown prefix operator: {:?}", expr.operator)))
        }
    }

    fn eval_infix_expression(&mut self, expr: &InfixExpression, runtime: &mut Runtime) -> EvalResult {
        let left = match self.eval_expression(&expr.left_value, runtime) {
            Ok(val) => val,
            Err(err) => return Err(err),
        };

        let right = match self.eval_expression(&expr.right_value, runtime) {
            Ok(val) => val,
            Err(err) => return Err(err),
        };

        match expr.operator {
            TokenType::Plus => {
                if left.is_number() && right.is_number() {
                    return Ok(self.eval_operation_on_int(
                        &left, &right,
                        |l, r| { Value::Number(NumberType::Int(l + r)) })
                    );
                }

                let mut arguments = vec![left.clone(), right];

                match left {
                    Value::Table(pointer) => {
                        if let Some(table_data) = runtime.get_table(&pointer) {
                            if let Some(Value::Table(meta_table_pointer)) = table_data.meta_table {
                                if let Some(Function(plus_func)) = runtime.get_table_property(&meta_table_pointer, &"__add".to_string()) {
                                    return match plus_func.clone() {
                                        FunctionType::Native(func) => self.eval_native_function(&func, &arguments, runtime),
                                        FunctionType::NativeMutable(func) => self.eval_native_mutable_function(&func, &mut arguments, runtime),
                                        FunctionType::Expression(func) => self.eval_function(&func, &arguments, runtime),
                                    };
                                }
                            }
                        }

                        return Err(EvalError::new("Wrong type used for plus".to_string()));
                    }
                    _ => return Err(EvalError::new("Wrong type used for plus".to_string()))
                }
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
                return Ok(Value::Boolean(left.is_equals(&right)));
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

    fn eval_condition(&mut self, condition: &Box<dyn Expression>, runtime: &mut Runtime) -> bool {
        let condition = self.eval_expression(condition, runtime);

        let condition_res = match condition {
            Ok(condition_res) => condition_res,
            _ => return false
        };

        return match condition_res {
            Value::Boolean(bool_val) => bool_val,
            _ => return false
        };
    }

    fn get_variable_value<'a>(&'a self, variable: &String, runtime: &'a Runtime) -> Option<&Value> {
        if variable.contains(".") {
            let Some((table_name, property_name)) = variable.split_once(".") else { todo!() };
            let table = runtime.stack
                .iter()
                .rev()
                .find(|x| { x.contains_key(table_name) })
                .map(move |t| { t.get(table_name).unwrap() });

            if let Some(table_val) = table {
                match table_val {
                    Value::Table(pointer) => {
                        runtime.get_table_property(pointer, &property_name.to_string())
                    }
                    _ => None
                }
            } else {
                None
            }
        } else {
            runtime.stack
                .iter()
                .rev()
                .find(|x| { x.contains_key(variable) })
                .map(move |t| { t.get(variable).unwrap() })
        }
    }
}