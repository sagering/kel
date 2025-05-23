# Kel

Kel is a rust-embeddable typed configuration and templating language.

# Features

- Strong Typing: Includes basic types, user-defined structs, lists, optionals, and union types.
- Templates and Amending: Simplifies object creation and modification.
- Modules and Imports: Allows modular code organization and selective imports.
- Control Structures: Includes for/if generators and ternary expressions.
- Operators: Supports binary/unary math, logical, comparison, null coalescing, optional chaining, type testing, and casting.
- String Interpolation: Embeds variables directly into strings.

# Example

```
use kl::checker::{Compiler, Context, Scope, Stack};
use kl::eval::evaluate_module;

fn main() {
    let mut compiler = Compiler::new();
    let code = "hi = \"Hello World!\"";
    compiler.add_file("code.kl".to_string(), code.to_string());

    let mut context = Context::new();
    let mut scope = Scope::new();

    let _ = compiler
        .get_or_compile_module(&mut context, &mut scope, "code.kl")
        .and_then(|module_id| {
            let mut stack = Stack::new();
            let module = context.get_module(module_id);
            evaluate_module(&mut stack, &context, &module)
        });
}

```

# Language Tour

## Types

### Basic Types

```
int : Int = 4
float : Float = 4.0
string : String = "Indigo"
any : Any = "Pila"
```

### User defined struct types
```
struct Dog {
    name: String
    age: Int
    breed: String
}

// Initialize a new constant 'jimmy' with a 'Dog' instance.
jimmy = Dog { name = "Jimmy", age = 3, breed = "Beagle" }

// Arguments to struct and other initializer expressions
// can be seperated by comma or new lines and the order of
// arguments does not matter.
jester = Dog {
    breed = "Beagle"
    name = "Jester"
    age = 4
}
```

### Lists
```
list : List[Int] = [1, 2, 3, 4]

// Indexing into list
last = list[3] 
```

### Optionals
```
maybe : Int? = null
opt : Int? = 4
```

### Oneof union type
```
oneof FloatOrString {
    Float
    String
}

val : FloatOrString = "I am a String"
```

### String literal types
```
s : "42" = "42"

// The value of a string literal type can only be the type itself.
// Oneofs of only string literal types essentially enumerates all
// allowed values of this type.
oneof Colors {
    "red"
    "green"
    "blue"
    "white"
    "black"
}
```


## Operators

### Binary math operators
```
a0 = 1 + 4
a1 = 1 - 4
a2 = 1 / 4
a3 = 1 * 4
```

### Unary math operators
```
b = -1
```

### Binary logical operators
```
c0 = true || false
c1 = true && false
```

### Unary logical operator
```
d = !true
```

### Comparison operators
```
e0 = 1 < 14 
e1 = 1 <= 14
e2 = 1 > 14
e3 = 1 >= 14
```

### Struct field access '.'
```
jimmys_age = jimmy.age
```

### Null coalescing operator '??'
```
// to provide a default on the right hand side
// in case the left hand side optional is null.
another_int = maybe ?? opt ?? 0
```

### Optional chaining operator '?.'
```
// to safely access fields in optional structs
// and evaluate to null if the optional is null.
maybe_dog : Dog? = null
opt_dog : Dog? = jimmy

age = maybe_dog?.age ?? opt_dog?.age ?? 0
```

### Type test operator 'is'
```
is_float = a0 is Float
```

### Type cast operator 'as'
```
cast = 8.0 as Int
```

### Parenthesization of expressions
```
paren = (4 - 1) * 2 + 1
```

### ternary expression <cond> ?: <true_branch> : <false_branch>
```
ternary1 = 4 < 3 ?: "4 < 3" : "4 >= 3"

// ternary expressions are right associative, e.g. this...
ternary2 = 4 < 3 ?: "4 < 3" : 4 < 5 ?: "4 < 5" : "4 >= 15"

// ...is equal to this
ternary3 = 4 < 3 ?: ("4 < 3") : (4 < 5 ?: "4 < 5" : "4 >= 5")
```

## Amending and templates
```
// Amending allows you to create variations of existing
// instances without redefining all fields.
ayla = jimmy { breed = "German Shepherd", name = "Ayla" }

// Templates simplify the creation of multiple similar objects.
template Beagle { name: String, age: Int} = Dog {
    name
    age
    breed = "Beagle"
}

beagles = [
    Beagle { name = "Jolly", age = 5 }
    Beagle { name = "Josephine", age = 2 }
    Beagle { name = "Johanna", age = 8 }
]
```

## Consts
```
// const with explicit type
explicit : Int = 4

// const with inferred type
inferred = 4
```

## Modules and imports
```
import "math"
from "strings" import { contains }
from "basic_types.kl" import { int }
```

## Function calls
```
yes = strings.contains("hi", "i")
min = math.min
mo = min(2, 1)
```

## For and If generators
```
beagle_names = [
    for beagle in beagles {
        beagle.name
    }
]

beagle_puppies = [
    for beagle in beagles {
        if beagle.age <= 1 {
            beagle.name
        }
    }
]
```

## String interpolation
```
your_name = "Caleb"
interpolated = $"Hi there {your_name}"
```

## Comments
```
// This is a comment.
```

# Backlog

- WASM playground
- wildcard imports, e.g. 'from "mylib.kl" import { * }'
- default arguments in templates
- improved ergonomics around variant checking for oneofs
- better handling of "index out of rang" errors


# License

MIT License