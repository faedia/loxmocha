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

A declaration is anything that introduces a new name into the current lexical scope for that declaration. Type declarations declare their names in the current type local scope. Function and variable declarations declare their names in the current value lexical scope. see [Scope](5-scope.md) for more details.

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

Any type expression can be given a name using a type declaration. The type declaration creates a new strongly typed type with the given name. It being strongly typed means that two declared types with different names, but with the same type expression (type structure), cannot be implicitly cast between.
Instead to cast between you will have to use the [as expression](2-expressions.md#type-cast-expression) to explicitly cast a value with same structural type to another strongly typed type.
Type expressions are always type-checked structurally, this allows for literals to be used with ease and for anonymous types, consider array types, to be cast between each other, or cast to a "strong" type with the same structural type.

for example:

```loxm
type int1 is u32;
type int2 is u32;

let a : int1 = 5;
// TYPE ERROR: They are not the "strong" type.
let b : int2 = a;
// OK: `a` is explicitly cast to int2 and they both have the same structural type.
let c : int2 = a as int2;

// This is a strong array type.
type arr_ints1 is u32[5];
// This is another strong array type.
type arr_ints2 is u32[5];

let arr_a : u32[5] = [1, 2, 3, 4, 5];
// OK: both array types here are anonymous, so they are structurally type checked.
let arr_b : u32[5] = arr_a;
// OK: even though arr_ints1 is a "strong" type, because arr_b is anonymous it can
// be implicitly cast to arr_ints1 because they have the same structural type.
let arr_c : arr_ints1 = arr_b;
// TYPE ERROR: both types are "strong" types they cannot be implicitly cast
// between.
let arr_d : arr_ints2 = arr_c;
// OK: `arr_c` is explicitly cast to arr_ints2 and they both have the same
// structural type.
let arr_e : arr_ints2 = arr_c as arr_ints2;
```

## Function Declaration {#function-declaration}

```grammar
functionDeclaration -> 
    "fun" identifier "(" parameters ")" ":" typeExpression body
parameter -> (itemDeclaration ("," itemDeclaration)*)?
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
