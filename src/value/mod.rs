use crate::call::{
    Call,
    CallError,
};

pub mod class;
use class::Class;

pub mod function;
use function::Function;

pub mod method;
use method::Method;

pub mod native_function;
use native_function::NativeFunction;

pub mod object;
use object::Object;

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
    Function(Function),
    NativeFunction(NativeFunction),
    Class(Class),
    Object(Object),
    Method(Method),
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(v) => *v,
            Value::Number(v) => *v != 0.0,
            Value::String(v) => v.len() != 0,
            Value::Function(_) => true,
            Value::NativeFunction(_) => true,
            Value::Class(_) => true,
            Value::Object(_) => true,
            Value::Method(_) => true,
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Function(v) => write!(f, "{:?}", v),
            Value::NativeFunction(v) => write!(f, "{:?}", v),
            Value::Class(v) => write!(f, "{:?}", v),
            Value::Object(v) => write!(f, "{:?}", v),
            Value::Method(v) => write!(f, "{:?}", v),
        }
    }
}

impl Call for Value {
    fn call(&self, arguments: Vec<Value>) -> Result<Value, CallError> {
        match self {
            Value::Function(f) => {
                return f.call(arguments);
            }
            Value::NativeFunction(f) => {
                return f.call(arguments);
            }
            Value::Class(c) => {
                return c.call(arguments);
            }
            Value::Method(m) => {
                return m.call(arguments);
            }
            _ => {
                return Err(CallError::NotCallable);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Value;

    #[test]
    fn test_value_equal() {
        let lvals = [
            Value::Nil,
            Value::Bool(true),
            Value::Bool(false),
            Value::Number(1.0),
            Value::Number(100.001),
            Value::String("hello".to_string()),
            Value::String("world".to_string()),
        ];
        let rvals = [
            Value::Nil,
            Value::Bool(true),
            Value::Bool(false),
            Value::Number(1.0),
            Value::Number(100.001),
            Value::String("hello".to_string()),
            Value::String("world".to_string()),
        ];
        let equals = [
            [true, false, false, false, false, false, false],
            [false, true, false, false, false, false, false],
            [false, false, true, false, false, false, false],
            [false, false, false, true, false, false, false],
            [false, false, false, false, true, false, false],
            [false, false, false, false, false, true, false],
            [false, false, false, false, false, false, true],
        ];
        for (i, equals) in equals.iter().enumerate() {
            for (j, equal) in equals.iter().enumerate() {
                assert_eq!(lvals[i] == rvals[j], *equal);
            }
        }
    }

    #[test]
    fn test_value_is_truthy() {
        let tests = [
            (Value::Nil, false),
            (Value::Bool(true), true),
            (Value::Bool(false), false),
            (Value::Number(1.0), true),
            (Value::Number(0.0), false),
            (Value::String("hello".to_string()), true),
            (Value::String("".to_string()), false),
        ];
        for (value, truthy) in tests {
            assert_eq!(value.is_truthy(), truthy);
        }
    }
}
