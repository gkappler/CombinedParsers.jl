# CombinedParsers.jl Documentation

*A package for combining parsers and transforming strings into julia types.*

Compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inferrence for transformations,
log conveniently for debugging, and let Julia compile your parser for good performance.


!!! note

	`CombinedParsers.jl` is still an α release.	The first official released is prepared for JuliaCon2020.
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

## Package Guide

```@contents
Pages = [
    "man/guide.md",
    "man/user.md",
    "man/pcre.md",
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
Pages = [ "lib/public.md" ]
Depth = 5
```

### Internals
```@contents
Pages = ["lib/internals/types.md", "lib/internals/iterate.md"]
```

# Acknowledgements

The work was inspired by Scala [fastparse](https://github.com/lihaoyi/fastparse) package and the Julia parsing packages
## [Parsers.jl](https://github.com/JuliaData/Parsers.jl)
A collection of parsers for date and primitive types.

### [TextParse.jl](https://github.com/queryverse/TextParse.jl)
A bunch of fast text parsing tools.
- used in CSV.jl
- Nullables.jl

TextParse.jl integrates with CombinedParsers.jl both ways 
by type `CombinedParsers.AbstractParser <: TextParse.AbstractToken`
and providing a method for `TextParse.tryparsenext`.

```@docs
CombinedParsers.tryparsenext
```

### [Automa.jl](https://github.com/BioJulia/Automa.jl)
A Julia package for text validation, parsing, and tokenizing based on state machine compiler.
The package compiles a parser deterministic finite automaton.
Processing of parsed tokens is done with actions.
No UTF8 support.

### [ParserCombinator.jl](https://github.com/andrewcooke/ParserCombinator.jl)
- old source base (pre 2016, fixed for Julia 1.0 in 2018)
    - using Nullables
- no iterator API
- performance 
    - mutable matcher types
    - matcher types not parametric

### [Index](@id main-index)

```@index
Pages = ["lib/public.md"]
```

