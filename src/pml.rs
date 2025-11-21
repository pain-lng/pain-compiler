// PML (Pain Markup Language) parser and AST

use pain_runtime::{ClassInstance, Value};
use std::collections::HashMap;

/// PML node kind
#[derive(Debug, Clone, PartialEq)]
pub enum PmlNodeKind {
    Scalar,
    Map,
    List,
}

/// PML AST node
#[derive(Debug, Clone)]
pub struct PmlNode {
    pub kind: PmlNodeKind,
    pub scalar: Option<String>,                // if SCALAR
    pub map: Option<HashMap<String, PmlNode>>, // if MAP
    pub list: Option<Vec<PmlNode>>,            // if LIST
}

impl PmlNode {
    /// Create a scalar node
    pub fn scalar(value: String) -> Self {
        Self {
            kind: PmlNodeKind::Scalar,
            scalar: Some(value),
            map: None,
            list: None,
        }
    }

    /// Create a map node
    pub fn map() -> Self {
        Self {
            kind: PmlNodeKind::Map,
            scalar: None,
            map: Some(HashMap::new()),
            list: None,
        }
    }

    /// Create a list node
    pub fn list() -> Self {
        Self {
            kind: PmlNodeKind::List,
            scalar: None,
            map: None,
            list: Some(Vec::new()),
        }
    }

    /// Get value from map by key
    pub fn get(&self, key: &str) -> Option<&PmlNode> {
        match &self.map {
            Some(map) => map.get(key),
            None => None,
        }
    }

    /// Get value from list by index
    pub fn get_index(&self, index: usize) -> Option<&PmlNode> {
        match &self.list {
            Some(list) => list.get(index),
            None => None,
        }
    }

    /// Convert PML node to runtime Value
    pub fn to_value(&self) -> Value {
        match &self.kind {
            PmlNodeKind::Scalar => {
                let s = self.scalar.as_ref().unwrap();
                // Try to parse as different types
                if s == "true" {
                    Value::Bool(true)
                } else if s == "false" {
                    Value::Bool(false)
                } else if s == "null" {
                    Value::None
                } else if let Ok(i) = s.parse::<i64>() {
                    Value::Int(i)
                } else if let Ok(f) = s.parse::<f64>() {
                    Value::Float(f)
                } else {
                    // Remove quotes if present
                    let unquoted = if s.starts_with('"') && s.ends_with('"') && s.len() >= 2 {
                        &s[1..s.len() - 1]
                    } else {
                        s
                    };
                    Value::String(unquoted.to_string())
                }
            }
            PmlNodeKind::Map => {
                let map = self.map.as_ref().unwrap();
                let mut instance = ClassInstance::new("PmlMap".to_string());
                for (k, v) in map {
                    instance.set_field(k.clone(), v.to_value());
                }
                Value::Object(instance)
            }
            PmlNodeKind::List => {
                let list = self.list.as_ref().unwrap();
                let values: Vec<Value> = list.iter().map(|n| n.to_value()).collect();
                Value::List(values)
            }
        }
    }
}
