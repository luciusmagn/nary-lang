# Rhai - embedded scripting for Rust

Rhai is an embedded scripting language for Rust that gives you a safe and easy way to add scripting to your applications.  

Rhai's current feature set:

* Easy integration with Rust functions and data types
* Fairly efficient (1 mil iterations in 0.75 sec on my 5 year old laptop)
* Low compile-time overhead (~4 secs for debug build, ~11 secs for release build)
* Simple, easy-to-use language features
* Support for overloaded functions
* No additional dependencies
* No unsafe code

**Note:** Currently, it's pre-0.1, and is likely to change a bit before it stabilizes enough for a crates.io release.*

## Variables

```Rust
var x = 3;
```

## Control blocks

```Rust
if true { 
    print("it's true!");
}
else {
    print("It's false!");
}
```

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


# Example 1: Hello world

```Rust
let mut engine = Engine::new();

if let Ok(result) = engine.eval("40 + 2".to_string()).unwrap().downcast::<i32>() {
    println!("Answer: {}", *result);  // prints 42
}
```
# Example 2: Working with functions

Rhai's scripting engine is very lightweight.  It gets its ability from the functions in your program.  To call these functions, you need to register them with the scripting engine.

```Rust
fn add(x: i32, y: i32) -> i32 {
    x + y
}

fn main() {
    let mut engine = Engine::new();

    &(add as fn(x: i32, y: i32)->i32).register(&mut engine, "add");
 
    if let Ok(result) = engine.eval("add(40, 2)".to_string()).unwrap().downcast::<i32>() {
       println!("Answer: {}", *result);  // prints 42
    }
}
```

# Example 3: Working with generic functions

Generic functions can be used in Rhai, but you'll need to register separate instances for each concrete type:

```Rust
fn showit<T: Display>(x: &mut T) -> () {
    println!("{}", x)
}

fn main() {
    let mut engine = Engine::new();

    &(showit as fn(x: &mut i32)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut bool)->()).register(&mut engine, "print");
    &(showit as fn(x: &mut String)->()).register(&mut engine, "print");
}
```

# Example 4: Working with custom types and methods

Here's an more complete example of working with Rust.  First the example, then we'll break it into parts:

```Rust
#[derive(Debug, Clone)]
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

let mut engine = Engine::new();

engine.register_type::<TestStruct>();

&(TestStruct::update as fn(&mut TestStruct)->()).register(&mut engine, "update");
&(TestStruct::new as fn()->TestStruct).register(&mut engine, "new_ts");

if let Ok(result) = engine.eval("var x = new_ts(); x.update(); x".to_string()).unwrap().downcast::<TestStruct>() {
    println!("result: {}", result.x); // prints 1001
}
```

First, for each type we use with the engine, we need to be able to Clone.  This allows the engine to pass by value and still keep its own state.

```Rust
#[derive(Debug, Clone)]
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
&(TestStruct::update as fn(&mut TestStruct)->()).register(&mut engine, "update");
&(TestStruct::new as fn()->TestStruct).register(&mut engine, "new_ts");
```

Finally, we call our script.  The script can see the function and method we registered earlier.  We need to get the result back out from script land just as before, this time casting to our custom struct type.
```Rust
if let Ok(result) = engine.eval("var x = new_ts(); x.update(); x".to_string()).unwrap().downcast::<TestStruct>() {
    println!("result: {}", result.x); // prints 1001
}
```
