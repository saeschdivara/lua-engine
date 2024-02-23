use std::collections::HashMap;
use crate::evaluation::typing::{ObjectValue, TableData, Value};

pub struct Runtime {
    pub stack: Vec<HashMap<String, Value>>,
    pub object_pool: Vec<ObjectValue>,
    pub return_triggered: bool,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime { stack: vec![HashMap::new()], return_triggered: false, object_pool: vec![] }
    }

    pub fn add_global_variable(&mut self, variable: &str, value: Value) {
        if let Some(global_stack) = self.stack.first_mut() {
            global_stack.insert(variable.to_string(), value);
        }
    }

    pub fn add_new_layer(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn create_object(&mut self, value: ObjectValue) -> usize {
        let pointer = self.object_pool.len();
        self.object_pool.push(value);
        return pointer;
    }

    pub fn get_table(&self, pointer: &usize) -> Option<&TableData> {
        if let Some(ObjectValue::Table(data)) = self.object_pool.get(pointer.clone()) {
            Some(data)
        } else {
            None
        }
    }

    pub fn get_table_mut(&mut self, pointer: &usize) -> Option<&mut TableData> {
        if let Some(ObjectValue::Table(data)) = self.object_pool.get_mut(pointer.clone()) {
            Some(data)
        } else {
            None
        }
    }

    pub fn get_table_property(&self, pointer: &usize, property_name: &String) -> Option<&Value> {
        if let Some(ObjectValue::Table(data)) = self.object_pool.get(pointer.clone()) {
            data.properties.get(property_name)
        } else {
            None
        }
    }

    pub fn insert_table_property_via_variable(&mut self, table_name: &String, property_name: &String, value: Value) {
        self.stack
            .iter()
            .rev()
            .find(|x| { x.contains_key(table_name) })
            .map(|t| {
                if let Some(Value::Table(pointer)) = t.get(table_name) {
                    if let Some(ObjectValue::Table(table)) = self.object_pool.get_mut(pointer.clone()) {
                        table.properties.insert(property_name.to_string(), value);
                    }
                }
            });
    }
}