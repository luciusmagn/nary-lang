# Rhai - embedded scripting for Rust

Rhai is an embedded scripting language for Rust that gives you a safe and easy way to add scripting to your applications.  

Rhai's current feature set:

* Easy integration with Rust functions and data types
* Fairly efficient (1 mil iterations in 0.75 sec on my 5 year old laptop)
* Low compile-time overhead (~0.6 sec debug/~3 sec release for script runner app)
* Easy-to-use language based on JS+Rust
* Support for overloaded functions
* No additional dependencies
* No unsafe code

**Note:** Currently, it's version 0.2.0, so the language and APIs may change before they stabilize.*

## Installation

You can install Rhai using crates by adding this line to your dependences:

```
[dependencies]
rhai = "0.2.0"
```

## Related

Other cool projects to check out:
* [ChaiScript](http://chaiscript.com/) - A strong inspiration for Rhai.  An embedded scripting language for C++ that I helped created many moons ago, now being lead by my cousin.
* [Dyon](https://github.com/PistonDevelopers/dyon) - A scripting language for Rust that's part of the Piston game engine.

## Variables

```Rust
var x = 3;
```

## Operators

```Rust
var x = (1 + 2) * (6 - 4) / 2;
```

## Control blocks

### If
```Rust
if true { 
    print("it's true!");
}
else {
    print("It's false!");
}
```

### While
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

Rhai supports defining functions in script:

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

# Example: Hello world

```Rust
extern crate rhai;
use rhai::Engine;

fn main() {
    let mut engine = Engine::new();

    if let Ok(result) = engine.eval::<i32>("40 + 2") {
        println!("Answer: {}", *result);  // prints 42
    }
}
```
# Example: Working with functions

Rhai's scripting engine is very lightweight.  It gets its ability from the functions in your program.  To call these functions, you need to register them with the scripting engine.

```Rust
extern crate rhai;
use rhai::{Engine, FnRegister};

fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    let mut engine = Engine::new();

    engine.register_fn("add", add);
 
    if let Ok(result) = engine.eval::<i32>("add(40, 2)") {
       println!("Answer: {}", *result);  // prints 42
    }
}
```

# Example: Working with generic functions

Generic functions can be used in Rhai, but you'll need to register separate instances for each concrete type:

```Rust
use std::fmt::Display;

extern crate rhai;
use rhai::{Engine, FnRegister};

fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main() {
    let mut engine = Engine::new();

    engine.register_fn("print", showit as fn(x: &mut i32)->());
    engine.register_fn("print", showit as fn(x: &mut bool)->());
    engine.register_fn("print", showit as fn(x: &mut String)->());
}
```

You can also see in this example how you can register multiple functions (or in this case multiple instances of the same function) to the same name in script.  This gives you a way to overload functions and call the correct one, based on the types of the arguments, from your script.

# Example: Working with custom types and methods

Here's an more complete example of working with Rust.  First the example, then we'll break it into parts:

```Rust
extern crate rhai;
use rhai::{Engine, FnRegister};

#[derive(Clone)]
struct TestStruct {
    x: i32
}

impl TestStruct {
    fn update(&mut self) {
        self.x += 1000;
    }

    fn new() -> TestStruct {
        TestStruct { x: 1 }
    }
}

fn main() {
    let mut engine = Engine::new();

    engine.register_type::<TestStruct>();

    engine.register_fn("update", TestStruct::update);
    engine.register_fn("new_ts", TestStruct::new);

    if let Ok(result) = engine.eval::<TestStruct>("var x = new_ts(); x.update(); x") {
        println!("result: {}", result.x); // prints 1001
    }
}
```

First, for each type we use with the engine, we need to be able to Clone.  This allows the engine to pass by value and still keep its own state.

```Rust
#[derive(Clone)]
struct TestStruct {
    x: i32
}
```

Next, we create a few methods that we'll later use in our scripts.  Notice that we register our custom type with the engine.
```Rust
impl TestStruct {
    fn update(&mut self) {
        self.x += 1000;
    }

    fn new() -> TestStruct {
        TestStruct { x: 1 }
    }
}

let mut engine = Engine::new();

engine.register_type::<TestStruct>();
```

To use methods and functions with the engine, we need to register them.  There are some convenience functions to help with this.  Below I register update and new with the engine.

*Note: the engine follows the convention that methods use a &mut first parameter so that invoking methods can update the value in memory.*

```Rust
engine.register_fn("update", TestStruct::update);
engine.register_fn("new_ts", TestStruct::new);
```

Finally, we call our script.  The script can see the function and method we registered earlier.  We need to get the result back out from script land just as before, this time casting to our custom struct type.
```Rust
if let Ok(result) = engine.eval::<TestStruct>("var x = new_ts(); x.update(); x") {
    println!("result: {}", result.x); // prints 1001
}
```

# Example: Working with getters and setters

Similarly, you can work with members of your custom types.  This works by registering a 'get' or a 'set' function for working with your struct.  

For example:

```Rust
#[derive(Clone)]
struct TestStruct {
    x: i32
}

impl TestStruct {
    fn get_x(&mut self) -> i32 {
        self.x
    }

    fn set_x(&mut self, new_x: i32) {
        self.x = new_x;
    }

    fn new() -> TestStruct {
        TestStruct { x: 1 }
    }
}

let mut engine = Engine::new();

engine.register_type::<TestStruct>();

engine.register_get_set("x", TestStruct::get_x, TestStruct::set_x);
engine.register_fn("new_ts", TestStruct::new);

if let Ok(result) = engine.eval::<i32>("var a = new_ts(); a.x = 500; a.x") {
    println!("result: {}", result);
}
```

# Example: Maintaining state

By default, Rhai treats each engine invocation as a fresh one, persisting only the functions that have been defined but no top-level state.  This gives each one a fairly clean starting place.  Sometimes, though, you want to continue using the same top-level state from one invocation to the next.

In this example, we thread the same state through multiple invocations:

```Rust
extern crate rhai;
use rhai::{Engine, Scope};

fn main() {
    let mut engine = Engine::new();
    let mut scope: Scope = Vec::new();

    if let Ok(_) = engine.eval_with_scope::<()>(&mut scope, "var x = 4 + 5") { } else { assert!(false); }    

    if let Ok(result) = engine.eval_with_scope::<i32>(&mut scope, "x") {
       println!("result: {}", result);
    }
}
```

