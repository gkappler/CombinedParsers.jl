# CombinedParsers.jl Documentation

*A package for combining parsers and transforming strings into julia types.*

Compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inferrence for transformations,
log conveniently for debugging, and let Julia compile your parser for good performance.


!!! note

    Please read through the
    [Documentation](https://docs.julialang.org/en/v1/manual/documentation/) section
    of the main Julia manual if this is your first time using Julia's documentation system.
    Once you've read through how to write documentation for your code then come back here.

## Package Features

- Clear syntax integrates grammar and transformations with Julia type inference.
- Higher-order parsers depending on the parsing state allow for not context-free parsers.
- All valid parsings can be iterated lazily.
- Interoperable with [TextParse.jl](https://github.com/queryverse/TextParse.jl): existing `TextParse.AbstractToken` implementations can be used with CombinedParsers. `CombinedParsers.AbstractParser` provide `TextParse.tryparsenext` and can be used e.g. in CSV.jl.
- Parametric parser and state types enable Julia compiler optimizations.
- Compiled regular expression parsers in pure julia are provided with the `re_str` macro.
- [AbstractTrees.jl](https://github.com/JuliaCollections/AbstractTrees.jl) interface provides colored and clearly layed out printing in the REPL.
- Convenient logging of the parsing process with `NamedParser`s and `SideeffectParser`s.
- CombinedParsers generalize from strings to parsing any type supporting `getindex`, `nextind`, `prevind` methods.


The [Package Guide](@ref) provides a tutorial explaining how to get started using CombinedParsers.

Some examples of packages using Documenter can be found on the [Examples](@ref) page.

See the [Index](@ref main-index) for the complete list of documented functions and types.

## Manual Outline

```@contents
Pages = [
    "man/guide.md",
    "man/examples.md",
    "man/syntax.md",
    "man/doctests.md",
    "man/hosting.md",
    "man/latex.md",
    "man/contributing.md",
]
Depth = 1
```

## Library Outline

```@contents
Pages = ["lib/public.md", "lib/internals/types.md", "lib/internals/iterate.md"]
```

### [Index](@id main-index)

```@index
Pages = ["lib/public.md"]
```


```@contents
```

```@docs
parse(parser, str)
```

## Index
```@index
```

# Acknowledgements

The work was inspired by Scala FastParse package and the Julia parsing packages
## Parsers.jl
For date and primitive types.
### TextParse
- used in CSV.jl
- uses Nullables
- CombinedParsers.AbstractParser <: TextParse.AbstractToken

### Automa.jl
- grammar based state machine compiler
- no UTF8 support

### ParserCombinator.jl
- old source base (pre 2016, fixed for Julia 1.0 in 2018)
    - using Nullables
- no iterator API
- performance 
    - mutable matcher types
    - matcher types not parametric
