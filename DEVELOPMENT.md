# Starting

If you want to explore the code base with ghci, here is a good setup which might
help you to get started.

1. Do `cabal repl` to start GHCi on the project. This will load
   `PTS.Interactive` by default.
2. Use `:browse` to see what's available. We try to give easy access to
   convenience functions by putting them in `PTS.Interactive` or re-exporting
   them from there, but YMMV.

3. Use `:add` to load new Haskell sources if ever needed.
4. Use `:m + module` to import a new module, or `:m + *module` to also import
   its internals.
5. Use `:r` to reload PTS source if you change it.

If you want to add packages for GHCi (say, lens), add the corresponding command
(e.g., `:set -package lens`) at the end of `.ghci`.

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

You can also use `parseSimple`, if the program is not statically known.

# Normalizing terms

Use `nbeClosed`, or `nbe` with a list of bindings.

# Viewing/decomposing terms

You can use the standard `Show` instance (created with `deriving Show`), or
`prettyShow` to invoke the pretty-printer.

We expect that `parse . prettyShow = id`, while `prettyShow . parse` will
canonicalize the text.

# Typechecking terms

Currently working example:

    wrapTypecheckPull [pts|lambda x : * . x|] [] Nothing

To apply pretty-printing to the term without needing to unwrap it, prepend:

    fmap showPretty `fmap`

Alternatively:

    wrapTypecheckPull [pts|*|] [] Nothing & mapped . mapped %~ showPretty

# Loading modules

You can use:

    r <- processFileSimple "path/to/file"

The output value will be interesting only if the source contains a module and
some exports. To see internals, use instead:

    r <- processFileSimpleInt "path/to/file"

Be careful though! Calling `show` on the result will easily loop with cyclic
terms, such as (by default) the constant `**` (which has itself as type in
Fomega*, the default PTS instance).

Either way, the code will autodetect whether to use literate parsing based on
the extension.

# Typechecking in the context of other modules

A pretty crude approach is this:

    q <- wrapTypecheckPull [pts|eval|] (getBindings r) Nothing

if `r` was bound as above.
