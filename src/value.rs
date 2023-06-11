#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Bool(v) => write!(f, "{}", v),
            Value::Number(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
        }
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(v) => *v,
            Value::Number(v) => *v != 0.0,
            Value::String(v) => v.len() != 0,
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
