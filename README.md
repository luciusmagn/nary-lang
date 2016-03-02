# Rhai - embedded scripting for Rust

Rhai is a simple embedded scripting language for Rust.  Thou that doesn't use any additional dependencies, unsafe code, or a set of APIs outside of what you provide in your program.  This allows you to have rich control over the functionality exposed to the scripting context.



Currently, it's pre-0.1, and is likely to change a bit before it stabilizes enough for a crates.io release.

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

Here's an example of working with Rust.  First, the full example, and then we'll break it down:

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
