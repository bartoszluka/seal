# Seal Language

Seal is a simple, C-like programming language made for fun.

## Name

The name is a pun that comes from having a C-like syntax.

C-like → sea like → seal

## Design

This is a work in progress.

The language is inspired by Rust and Haskell.

### Expression based

<!-- TODO: implement if expressions -->

`if` "statements" can be expressions and the returned value can be assigned to a variable. Example:

```seal
let number = if (condition) 69 else 420;
```

<!-- TODO: implement while expressions -->

The same can be said about `while` keyword.

```seal
let result = while (i < 10) {
    i = i * 10;
    i
}
```

Everything can be scoped.
That means the scope itself is an expression and can be used everywhere an expression is expected.

<!-- TODO: examples -->

## Progress

list of features:

- [x] parser
- [x] interpreter
- [ ] good error messages
- [x] compiler
- [ ] formatter
- [ ] language server LSP
- [ ] tree-sitter grammar/syntax highlighting
