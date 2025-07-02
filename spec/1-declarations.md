---
title: "LoxMocha Language Specification"
author: "Emma Tomlinson"
date: \today
documentclass: report
toc: true
toc-depth: 3
numbersections: true
secnumdepth: 4
mainfont: Open Sans
geometry: margin=1in
header-includes:
- \usepackage{fvextra}
- \DefineVerbatimEnvironment{Highlighting}{Verbatim}{breaklines,commandchars=\\\{\}}
- \usepackage{xcolor}
- \usepackage{mdframed}
- \let\oldShaded\Shaded
- \let\endoldShaded\endShaded
- |
  \renewenvironment{Shaded}{%
    \begin{mdframed}[%
      backgroundcolor=black!5,%   Set background color (5% black is a light grey)
      innerleftmargin=1.5em,%      Set left margin
      innerrightmargin=1.5em,%     Set right margin
      innertopmargin=1ex,%         Set top margin
      innerbottommargin=1ex,%      Set bottom margin
      skipabove=1.5em,
      skipbelow=1.5em,
      linewidth=1pt,%              Set the border line width to 0 (no border)
      nobreak=true%                Prevent the box from breaking across pages
    ]%
  }{%
    \end{mdframed}%
  }
---

# Declarations {#declarations}

A declaration is anything that introduces a new name into the current lexical scope for that declaration.
Type declarations declare their names in the current type local scope.
Function and variable declarations declare their names in the current value lexical scope.
See [Scope](5-scope.md) for more details.

```grammar
declaration ->
      typeDeclaration
    | functionDeclaration
    | variableDeclaration
```

## Type Declaration {#type-declaration}

```grammar
typeDeclaration -> "type" identifier "is" typeExpression
```

Any [type expression](3-types.md#types) can be given a name using a type declaration.
The type declaration creates a new name in the current type local scope.
Types with names are type checked nominally.
This means two expressions are compatible only if they are declared with the exact same name type.
Anonymous types, or literal expressions are type checked structurally.
This means two anonymous types are compatible if and only if their underlying structures are identical.
For example, two anonymous array types are compatible if and only if their element types are compatible and have the same number of elements.
Expressions with anonymous types are automatically coerced to a named type when required, as long as the named type and anonymous type share the same structural type.
Named types will not be automatically coerced due to their nominal type checking.
Instead, to convert from one named type to another, an explicit cast using the [as expression](2-expressions.md#type-cast-expression) must be used.
This is only permitted if the types are structurally compatible.

for example:

```loxm
type int1 is u32;
type int2 is u32;

// OK: `a` can be assigned to as literals always are type checked structurally.
let a : int1 = 5;
// TYPE ERROR: `a` and `b` are both nominal types, but not the same nominal type.
let b : int2 = a;
// OK: `a` is explicitly cast to int2 and they both have the same structural type.
let c : int2 = a as int2;

// This is a nominal array type.
type arr_ints1 is u32[5];
// This is another nominal array type.
type arr_ints2 is u32[5];

let arr_a : u32[5] = [1, 2, 3, 4, 5];
// OK: both array types here are anonymous, so they are structurally type checked.
let arr_b : u32[5] = arr_a;
// OK: even though arr_ints1 is a nominal type, because arr_b is anonymous it
// can be implicitly cast to arr_ints1 because they have the same structural type.
let arr_c : arr_ints1 = arr_b;
// TYPE ERROR: both types are nominal types they cannot be implicitly coerced
// between.
let arr_d : arr_ints2 = arr_c;
// OK: `arr_c` is explicitly cast to arr_ints2 and they both have the same
// structural type.
let arr_e : arr_ints2 = arr_c as arr_ints2;
```

## Function Declaration {#function-declaration}

```grammar
functionDeclaration -> "fun" identifier "(" parameters ")" ":" typeExpression body
parameter -> (itemDeclaration ("," itemDeclaration)*)?
```

Function declarations create a new name in the current value local scope.
Then a new value scope is created, where the names declared by the parameters are declared.

Functions act as values, and they have types.
When a function is declared it is automatically given a new anonymous [function type](3-types.md#function-type) matching the type expressions of the functions parameters and return type.
The names of functions can also be declared to variables that have types that are compatible with the type of the function declaration.

For example:

```loxm
fun add(a: u32, b: u32): u32 -> a + b;
```

The function `add` has an anonymous type that is structurally equivalent to `fun(u32, u32): u32`.

And can be interacted with like a variable in the following ways:

```loxm
// A simple function.
fun add(a: u32, b: u32): u32 -> a + b;

// A function that takes a function and its parameters as input and executes it.
fun do_function(f: fun(u32, u32): u32, a: u32, b: u32): u32 -> f(a, b);

// Assign add to a variable.
let f: fun(u32, u32): u32 = add;
// Pass add as a parameter.
do_function(add, 1, 2);
// Pass the function variable as a parameter.
do_function(f, 1, 2);
```

## Variable Declaration {#variable-declaration}

```grammar
variableDeclaration -> constantDeclaration | mutableDeclaration
```

```grammar
constantDeclaration -> "let" itemDeclaration
```

```grammar
mutableDeclaration -> "var" itemDeclaration
```

```grammar
itemDeclaration -> pattern ":" typeExpression ("=" expression)?
```

If this is a comma separated list the `typeExpression` must be either a tuple, array, or record type. Each identifier then refers to the type in the order as it would be in the tuple, array or record of the `typeExpression`.
