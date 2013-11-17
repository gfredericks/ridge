# Ridge

Ridge is a clojuresque lisp interpreted in haskell. It is just a toy.

## Setup

```
cabal sandbox init
cabal install --only-dependencies
```

## Compile

```
cabal build
```

## Running the repl

```
dist/build/ridge/ridge
```

## Interesting differences from Clojure (at the moment)

- Haskell-style lazy evaluation
- Haskell-style tail recursion
- No vars

## TODO

- Sets
- Full data structure functions
- (defmacro let ...)
- Destructuring
- Better parser
  - E.g., '< is not parsed currently
  - No non-edn reader syntax
- Improve evaluation of locals
  - Don't do a hashmap lookup on each evaluation

## TODO??

Haven't really decided what the goal of this language is so the
following things may or may not be outstanding:

- Namespaces
- Various sorts of mutability:
  - Vars
  - Other references
  - Protocols
  - Multimethods??
- Haskell Interop...what would that even mean?
  - What facilities are there in haskell for doing this?
  - Would we have to start compiling to haskell src rather than
    interpreting?
- IO