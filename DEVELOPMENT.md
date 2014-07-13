# Starting

1. Do `cabal repl` to start GHCi on the project.
2. Use `:browse` to see what's available.
3. Use `:add` to load new Haskell sources if ever needed.
4. Use `:m + module` to import a new module, or `:m + *module` to also import
   its internals.
5. Use `:r` to reload PTS source if you change it.

# Tricks for working on this codebase

## Creating terms
You can use `[pts| ..your term... |]`. Note you can't add spaces in
`[pts|` or `|]`, or GHC will give bad errors. What's between pipes is much
freer-format because it's handled, in the end, by the PTS parser.

Example:
```
> [pts| * |]
> [pts|lambda x : * . x|]
lambda x : * . x
```

You can also use parseSimple, if the program is not statically known.

# Normalizing terms

Use `nbeClosed`, or `nbe` with a list of bindings.

# Viewing/decomposing terms

We're working on this.

# Typechecking terms

# Loading modules

Try:

    maybeMod <- processFileSimple "path/to/file"

But you won't be able to `show maybeMod` yet.
