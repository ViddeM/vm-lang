# VM_Lang
A basic interpreted programming language written in Rust as a fun exercise.

## Basic structure
A program written in the language is made up of a number of functions, one of which must be named `main`, take no arguments and have return type `void`.

### Functions
Has a name, a list of arguments and an optional return type followed by a block statement.

Ex:
```
fn some_func(a: int, b: [string]): boolean {
    // Statements
    return true
}
```

If a function has a return type it MUST use a `return` statement on every path through the function, otherwise a `return` is optional.

### Comments
Any text after `//` in a line is ignored (unless the `//` are within a string literal).

### Statements

#### Let
Declares a variable, type is inferred.

Ex:
```
let a = 0;
```

#### While
A loop that continues as long as the provided expression holds true.

Ex:
```
while true {
    // Statements
}
```

#### For
A for-each loop going iterating through a list.

Ex:
```
let some_list = [1, 4, 9];
for (a in some_list) {
    // Statements
}
```

#### If
A branching statement, runs the block if the expression holds true.

Ex:
```
if true {
    // Statements
}
```

#### If else
A branching statement, runs the first block if the expression holds true, otherwise runs the `else` block.

Ex:
```
let i = 4;
if i < 2 {
    // Statements
} else {
    // Statements
}
```

#### Return type
Returns from the current function with the provided expression, the type of the expression must be the same as the return type of the function.

Ex:
```
return 54;
```

#### Return void
Returns from the current function when the function has return type `void`.

Ex:
```
return;
```

#### Expressions
An expression ended with a semi-colon `;` is also a statement.

Ex:
```
i++;
```

#### Blocks
Wraps a list of statements creating a new scope.

Ex:
```
{
    // Statements
}
```

### Expressions
There are a number of expressions in the language, in order of priority they are:

#### Assignment
Reassigns a previously declared variable.

Ex:
```
a = 2;
```

#### Comparison
Compares two expressions of the same type returning a boolean.

Comparisons supporting only boolean expressions:
    - `||` | 'or' is true if either of the expressions are true.
    - `&&` | 'and' is true if both of the expressions are true.

Comparisons supporting all(?) types:
    - `==` | `equals` is true if the expressions are the same.
    - `!=` | `not equals` is true if the expressions are no the same.
    - `<=` | `Less than or equal to` is true if the first expression is less than or equal to the second expression.
    - `>=` | `Greater than or equal to` is true if the first expression is greater than or equal to the second expression.
    - `<` | `Less than` is true if the first expression is strictly less than the second expression.
    - `>` | `Greater than` is true if the first expression is strictly greater than the second expression.

Ex:
```
2 < 4
```

#### Arithmetic operations
Performs an arithmetic operation between two expressions of type `int`.

The currently suppported arithmetic operations are:
 - `+` | plus
 - `-` | minus
 - `*` | times
 - `/` | division

Ex:
```
2 + 4
```

#### Unary operations
Operations on singular expressions.

The currently supported unary operations are:
 - `!` | not
 - `++` | Increases the value by 1, depending on if put before or after the expression this will be done before or after the value is read.
 - `--` | Decreases the value by 1, depending on if put before or after the expression this will be done before or after the value is read.

Ex:
```
i++;
```

#### Function calls
Calls a function.

Ex:
```
some_func(42, "this function has two args");
```

#### List indexing
Retrieves a value from a list by its index.

Ex:
```
let some_list = [1, 5, 9];
some_list[1]; // Returns 5
```

#### Literals
A literal of any of the available types (except void).

Ex:
```
54
```

#### Parenthesised expression
Any expression wrapped in parenthesis.

Ex:
```
(1 + 2)
```

### Types
There are currently four types available in the language:
 - `int`, ex: `-54`
 - `bool`, ex: `true`
 - `string`, ex: `"Hello! This is a string"`
 - lists, declared as `[TYPE]` where `TYPE` can be any of the above types, ex: `[0, 1, 99, 200]`
 - `void`, never directly used but is used when no other type has been declared for e.g. functions that have no return type.

## Builtin functions
There are a few built-in functions available for specific tasks.

### print_number
Prints the number to standard out.

Args:
    - `number: int` | The number to print.

Returns:
    `Void

### print_string
Prints the string to standard out.

Args:
    - `text: string` | The string to print.

Returns:
    `Void`

### print_bool
Prints the boolean to standard out.

Args:
    - `val: bool` | The boolean to print.

Returns:
    `Void`

### read_number
Reads a number from standard in.

Args:
    None

Returns:
    - `number: int` | The number read.

### read_string
Reads a string from standard in.

Args:
    None

Returns:
    - `val: string` | The string read.

### read_bool
Reads a boolean from standard in.

Args:
    None

Returns:
    - `val: bool` | The boolean read.

### read_file
Given a path, reads a file to a string.

Args:
    - `file_path: string` | The filepath relative to the current directory.

Returns:
    - `file_content: string` | The content of the file.

### split_string
Given a string and a 'splitter', splits the string on each occurance of the 'splitter', returning a list of the parts.

Args:
    - `text: string` | The text to be split.
    - `splitter: string` | The text to split on.

Returns:
    - `parts: [string]` | A list containing every part of `text` split on each occurance of the `splitter`.

### parse_int
Parses a string to a number.

Args:
    - `text: string` | The text to parse.

Returns:
    - `num: int` | The parsed text.