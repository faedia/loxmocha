# Expressions

```grammar
expression ->
      literalExpression
    | identifierExpression
    | operatorExpression
    | arrayExpression
    | tupleExpression
    | indexExpression
    | callExpression
    | elementExpression
    | blockExpression
    | loopExpression
    | ifExpression
```

```grammar
literalExpression ->
      characterLiteral
    | stringLiteral
    | integerLiteral
    | trueLiteral
    | falseLiteral
```

```grammar
identifierExpression -> identifier
```

```grammar
operatorExpression ->
    | booleanExpression
    | comparisonExpression
    | arithmeticExpression
    | negationExpresssion
    | assignExpression
    | typeMemberExpression
    | typeCastExpression
    | referenceExpression
    | dereferenceExpression
```

```grammar
booleanExpression ->
      expression "&&" expression
    | expression "||" expression
```

```grammar
comparisonExpression ->
      expression "==" expression
    | expression "!=" expression
    | expression ">" expression
    | expression "<" expression
    | expression ">=" expression
    | expression "<=" expression
```

```grammar
arithmeticExpression ->
      expression "+" expression
    | expression "-" expression
    | expression "*" expression
    | expression "/" expression
    | expression "%" expression
    | expression "&" expression
    | expression "|" expression
    | expression "^" expression
    | expression "<<" expression
    | expression ">>" expression
```

```grammar
negationExpression ->
      "-" expression
    | "~" expression
    | "!" expression
```

```grammar
assignExpression -> expression "=" expression
```

```grammar
typeMemberExpression -> expression "is" (typeExpression | pattern)
```

## Type Cast Expression {#type-cast-expression}

```grammar
typeCastExpression -> expression "as" typeExpression
```

```grammar
referenceExpression -> "&" expression
```

```grammar
dereferenceExpression -> "*" expression
```

```grammar
arrayExpression -> "[" (expression ",")* expression? "]"
```

```grammar
tupleExpression -> "(" (expression ",")* expression? ")"
```

```grammar
recordExpression -> "{" (identifier ":" expression ",")* (identifier ":" expression ",")? "}"
```

```grammar
indexExpression -> expression "[" expression "]"
```

```grammar
fieldExpression -> expression "." identifier
```

```grammar
callExpression -> expression "(" arguments ")"
arguments -> (argument ("," argument)*)?
argument -> (identifier ":")? expression
```

```grammar
ifExpression -> "if" expression body ("else" (expression | ifExpression))?`
```

```grammar
loopExpression -> forExpression | whileExpression
```

```grammar
forExpression -> "for" itemDeclaration "in" expression body
```

```grammar
whileExpression -> "while" expression body
```

```grammar
blockExpression -> "begin" (declaration ";")* expression "end"
```

```grammar
closureExpression -> "(" parameters ")" ":" typeExpression body
```

```grammar
body -> ("->" expression) | blockExpression
```
