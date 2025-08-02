# Tokens

`LoxMocha` has a distinct set of tokens that are futher categorised into different groups.

```grammar
token ->
    punctuation
    literal
    comment
    keyword
    identifier
    whitespace
```

## Punctuation

The following are the punctuation token for `LoxMocha`:

```grammar
punctuation ->
    "("
    ")"
    "["
    "]"
    "{"
    "}"
    ":"
    ","
    "."
    "="
    "=="
    "!="
    "=>"
    ">"
    ">="
    ">>"
    "<"
    "<="
    "<<"
    "&"
    "&&"
    "|"
    "||"
    "+"
    "-"
    "*"
    "/"
    "%"
    "^"
    "~"
    "!"
```

| Punctuation | Name            | Description                |
|-------------|------           |-------------               |
| `(`         | `left_paren`    | Left parenthesis           |
| `)`         | `right_paren`   | Right parenthesis          |
| `[`         | `left_square`   | Left square bracket        |
| `]`         | `right_square`  | Right square bracket       |
| `{`         | `left_brace`    | Left curly brace           |
| `}`         | `right_brace`   | Right curly brace          |
| `:`         | `colon`         | Colon                      |
| `,`         | `comma`         | Comma                      |
| `.`         | `dot`           | Dot                        |
| `=`         | `equal`         | Equal sign                 |
| `==`        | `equal_equal`   | Double equal sign          |
| `!=`        | `not_equal`     | Not equal sign             |
| `=>`        | `arrow`         | Arrow sign                 |
| `>`         | `greater`       | Greater than sign          |
| `>=`        | `greater_equal` | Greater than or equal sign |
| `>>`        | `right_shift`   | Right shift sign           |
| `<`         | `less`          | Less than sign             |
| `<=`        | `less_equal`    | Less than or equal sign    |
| `<<`        | `left_shift`    | Left shift sign            |
| `&`         | `and`           | Ampersand sign             |
| `&&`        | `and_and`       | Double ampersand sign      |
| `\|`        | `pipe`          | Pipe sign                  |
| `\|\|`      | `pipe_pipe`     | Double pipe sign           |
| `+`         | `plus`          | Plus sign                  |
| `-`         | `minus`         | Minus sign                 |
| `*`         | `asterisk`      | Asterisk sign              |
| `/`         | `slash`         | Slash sign                 |
| `%`         | `percent`       | Percent sign               |
| `^`         | `caret`         | Caret sign                 |
| `~`         | `tilde`         | Tilde sign                 |
| `!`         | `bang`          | Exclamation mark           |

## Literal

There are 3 types of literal tokens in `LoxMocha`:

- `integer`
- `character`
- `string`

### Integer

At the moment `LoxMocha` only supports decimal integers.

```grammar
integer -> [0-9]+
```

### Character

A character literal is a character enclosed in single quotes.
The character can be any ASCII character and can also be an escape sequence.

```grammar
character -> "'" ([^'] | escapeSequence) "'"
```

### String

A string literal is a sequence of characters enclosed in double quotes.
A string can contain any ASCII character and can also include escape sequences.

```grammar
string -> '"' ([^"] | escapeSequence)* '"'
```

### Escape Sequence

An escape sequence is a backslash followed by a seqeuence of characters that may represent a special character.

```grammar
escapeSequence -> "\\" ("n" | "t" | "r" | "\\" | "'" | "\"" | "0" | "x" [0-9a-fA-F]{2})
```

## Comment

There are two types of comments in `LoxMocha`:

- Single-line comments
- Block comments

### Single-line Comment

A single-line comment starts with `//` and continues until the end of the line.

```grammar
singleLineComment -> "//" [^\n]* "\n"
```

### Block Comment

A block comment starts with `/*` and ends with `*/`.

```grammar
blockComment -> "/*" ([^*] | "*" [^/])* "*/"
```

## Keywords

A keyword is a reserved word in `LoxMocha` that has specific semantic meaning.
A keyword has the same structure as an identifier, but it is reserved and cannot be used as an identifier.

```grammar
keyword ->
    "as"
    "begin"
    "else"
    "end"
    "false"
    "for"
    "fun"
    "if"
    "in"
    "is"
    "let"
    "mut"
    "rec"
    "true"
    "type"
    "var"
    "while"
```

| Keyword | Name    | Description                                                              |
|---------|------   |-------------                                                             |
| `as`    | `as`    | Used for type casting in expressions.                                    |
| `begin` | `begin` | Marks the beginning of a block of code.                                  |
| `else`  | `else`  | Used in conditional expressions to specify an alternative block of code. |
| `end`   | `end`   | Marks the end of a block of code.                                        |
| `false` | `false` | Represents the boolean value false.                                      |
| `for`   | `for`   | Marks a for loop expression.                                             |
| `fun`   | `fun`   | Used to declare a function.                                              |
| `if`    | `if`    | Marks a conditional expression.                                          |
| `in`    | `in`    | Used in for loops to specify the collection to iterate over.             |
| `is`    | `is`    | Used to check the type of an expression.                                 |
| `let`   | `let`   | Declares an immutable binding.                                           |
| `mut`   | `mut`   | Declares a type as mutable.                                              |
| `rec`   | `rec`   | Declares a record type.                                                  |
| `true`  | `true`  | Represents the boolean value true.                                       |
| `type`  | `type`  | Declares a type.                                                         |
| `var`   | `var`   | Declares a mutable binding.                                              |
| `while` | `while` | Marks a while loop expression.                                           |

## Identifiers

An identifier is a name used to refer to some value or type in `LoxMocha`.

```grammar
identifier -> [a-zA-Z][a-zA-Z0-9_]*
```

## Whitespace

Whitespace is not significant in `LoxMocha` and is used to delimit tokens.

