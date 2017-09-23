# Nary - embedded scripting for Rust

Nary is, prolly because it's a fork of Rhai, an embedded scripting language for Rust that gives you a safe and easy way to add scripting to your applications.

Nary's current feature set:

* Easy integration with Rust functions and data types
* Fairly efficient (1 mil iterations in 0.75 sec on my 5 year old laptop)
* Low compile-time overhead (~0.6 sec debug/~3 sec release for script runner app)
* Easy-to-use language based on JS+Rust
* Support for overloaded functions
* No additional dependencies
* No unsafe code
* Unicorns

In comparison to Rhai:
* Accepts more whitespace (for example Rhai doesn't like tabs)
* More mathematical operations
* Slight changes to syntax
* More verbose error logging (maybe?)
* Optional tiny standard library

**Note:** Nary is not exactly backwards compatible with Rhai.

## Installation

TBD

# Hello world

To get going with Nary, you create an instance of the scripting engine and then run eval.

```Rust
extern crate nary;
use nary::Engine;

fn main()
{
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i64>("40 + 2")
    {
        println!("Answer: {}", result);  // prints 42
    }
}
```

You can also evaluate a script file:

```Rust
if let Ok(result) = engine.eval_file::<i64>("hello_world.nary") { ... }
```

# Nary Language guide

## Variables

```Rust
var x = 3;
```

## Operators

```Rust
var x = (1 + 2) * (6 - 4) / 2;
```

## If
```Rust
if true {
    print("it's true!");
}
else {
    print("It's false!");
}
```

## While
```Rust
var x = 10;
while x > 0 {
    print(x);
    if x == 5 {
        break;
    }
    x = x - 1;
}
```

## Functions

Nary supports defining functions in script:

```Rust
fn add(x, y) {
    return x + y;
}

print(add(2, 3))
```

Just like in Rust, you can also use an implicit return.

```Rust
fn add(x, y) {
    x + y
}

print(add(2, 3))
```
## Arrays

You can create arrays of values, and then access them with numeric indices.

```Rust
var y = [1, 2, 3];
y[1] = 5;

print(y[1]);
```

## Members and methods

```Rust
var a = new_ts();
a.x = 500;
a.update();
```

## Strings and Chars

```Rust
var name = "Bob";
var middle_initial = 'C';
```

