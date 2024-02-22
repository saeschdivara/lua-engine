use crate::evaluation::interpreter::Callstack;
use crate::evaluation::typing::{EvalError, EvalResult, FunctionType, Value};
use crate::evaluation::typing::Value::Function;

pub fn add_standard_functions(callstack: &mut Callstack) {

    let my_print = |args: &Vec<Value>| -> EvalResult {
        args.iter().for_each(|v| {print!("{:?} ", v);});
        println!();
        Ok(Value::Nil)
    };

    let set_metatable = |args: &mut Vec<Value>| -> EvalResult {
        if args.len() != 2 { return Err(EvalError::new("setmetatable expects 2 arguments".to_string())) }

        let meta_table_value = args.last().unwrap().clone();

        match meta_table_value {
            Value::Table(_, _, _) => {},
            _ => return Err(EvalError::new("first arguments has to be a table".to_string()))
        }

        match args.first_mut().unwrap() {
            Value::Table(_, _, meta_table) => {
                meta_table.value = Some(Box::new(meta_table_value));
            }
            _ => return Err(EvalError::new("first arguments has to be a table".to_string()))
        }

        Ok(Value::Nil)
    };

    callstack.variables.first_mut().unwrap().insert("print".to_string(), Function(FunctionType::Native(my_print)));
    callstack.variables.first_mut().unwrap().insert("setmetatable".to_string(), Function(FunctionType::NativeMutable(set_metatable)));
}