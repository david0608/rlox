use std::{
    time::{
        UNIX_EPOCH,
        SystemTime,
    },
    rc::Rc,
    cell::RefCell,
    collections::HashSet,
};
use crate::{
    value::{
        Value,
        native_function::NativeFunction,
    },
    environment::{
        Environment,
        EnvironmentT,
    },
    error::RuntimeErrorEnum,
    resolve_context::ResolveContext,
};

pub fn add_native_clock(
    resolve_context: &mut Vec<HashSet<String>>,
    environment: &Rc<RefCell<Environment>>,
) {
    resolve_context.declare("clock")
        .expect("Declare native function clock.");
    environment.declare(
        "clock",
        Value::NativeFunction(
            NativeFunction::new("clock", native_function_clock_handler),
        )
    )
        .expect("Declare native function clock.");
}

fn native_function_clock_handler(arguments: Vec<Value>) -> Result<Value, RuntimeErrorEnum> {
    let argn = arguments.len();
    if argn != 0 {
        return Err(RuntimeErrorEnum::ArgumentNumberMismatch(0, argn));
    }

    return Ok(
        Value::Number(
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map(|d| d.as_millis())
                .unwrap_or(0) as f64
        )
    );
}

#[cfg(test)]
mod tests {
    use std::{
        rc::Rc,
        cell::RefCell,
        thread::sleep,
        time::Duration,
        collections::HashSet,
    };
    use crate::{
        native::{
            add_native_clock,
            clock::native_function_clock_handler,
        },
        value::{
            Value,
            Call,
        },
        environment::{
            Environment,
            EnvironmentT,
        },
        error::RuntimeErrorEnum,
        resolve_context::ResolveContext,
    };

    #[test]
    fn test_native_function_clock() {
        let mut ctx = <Vec<HashSet<String>> as ResolveContext>::new();
        let env = <Rc<RefCell<Environment>> as EnvironmentT>::new();
        add_native_clock(&mut ctx, &env);

        assert_eq!(ctx.find("clock").unwrap(), 0);

        let nf = if let Some(Value::NativeFunction(nf)) = env.get("clock", 0) {
            nf
        }
        else {
            panic!("Native function clock should be declared.");
        };
        let start = if let Ok(Value::Number(ms)) = nf.call(vec![]) {
            ms
        }
        else {
            panic!("Call on native function clock should return timestamp number.");
        };
        sleep(Duration::from_millis(100));
        let end = if let Ok(Value::Number(ms)) = nf.call(vec![]) {
            ms
        }
        else {
            panic!("Call on native function clock should return timestamp number.");
        };
        assert_eq!((end - start) >= 100.0, true);
    }

    #[test]
    fn test_native_function_clock_call_argument_number_mismatch_error() {
        assert_eq!(
            native_function_clock_handler(vec![Value::Bool(true)]),
            Err(
                RuntimeErrorEnum::ArgumentNumberMismatch(0, 1)
            )
        );
    }
}
