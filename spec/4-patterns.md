# Patterns

```grammar
pattern -> identifierPattern | constructorPattern
```

```grammar
identifierPattern -> "var"? identifier ("is" pattern)?
```

```grammar
constructorPattern -> identifier "::" identifier pattern
```
