use crate::evaluation::runtime::Runtime;
use crate::evaluation::typing::{EvalError, EvalResult, FunctionType, Value};
use crate::evaluation::typing::Value::Function;

pub fn add_standard_functions(callstack: &mut Runtime) {

    let my_print = |args: &Vec<Value>, _: &mut Runtime| -> EvalResult {
        args.iter().for_each(|v| {print!("{:?} ", v);});
        println!();
        Ok(Value::Nil)
    };

    let set_metatable = |args: &mut Vec<Value>, callstack: &mut Runtime| -> EvalResult {
        if args.len() != 2 { return Err(EvalError::new("setmetatable expects 2 arguments".to_string())) }

        let meta_table_value = args.last().unwrap().clone();

        match meta_table_value {
            Value::Table(_) => {},
            _ => return Err(EvalError::new("first arguments has to be a table".to_string()))
        }

        match args.first().unwrap() {
            Value::Table(pointer) => {
                if let Some(table_data) = callstack.get_table_mut(pointer) {
                    table_data.meta_table = Some(meta_table_value);
                }
            }
            _ => return Err(EvalError::new("first arguments has to be a table".to_string()))
        }

        Ok(Value::Nil)
    };

    callstack.stack.first_mut().unwrap().insert("print".to_string(), Function(FunctionType::Native(my_print)));
    callstack.stack.first_mut().unwrap().insert("setmetatable".to_string(), Function(FunctionType::NativeMutable(set_metatable)));
}