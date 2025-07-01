# Types

```grammar
typeExpression ->
      primitiveType
    | arrayType
    | tupleType
    | recordType
    | referenceType
    | taggedType
    | funtionType
    | mutableType
    | identifier
```

```grammar
arrayType -> typeExpression "[" expression "]"
```

The expression in an array type is a constant expression that evaluates to some `integer` type.

```grammar
tupleType -> "(" (typeExpression ("," typeExpression)*)? ")"
```

```grammar
recordType -> "rec" (itemDeclaration ";")+ "end"
```

```grammar
taggedType -> identifier typeExpression? ("|" identifier typeExpression?)*
```

```grammar
referenceType -> "&" typeExpression
```

```grammar
functionType -> "fun" "(" (typeExpression ("," typeExpression)*)? ")" ":" typeExpression
```

```grammar
mutableType -> "mut" typeExpression
```
