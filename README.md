
# CombinedParsers in pure Julia
<!-- [![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://gkappler.github.io/CombinedParsers.jl/stable) -->
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://gkappler.github.io/CombinedParsers.jl/dev)
[![Build Status](https://travis-ci.org/gkappler/CombinedParsers.jl.svg?branch=master)](https://travis-ci.com/github/gkappler/CombinedParsers.jl)
[![Codecov](https://codecov.io/gh/gkappler/CombinedParsers.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/gkappler/CombinedParsers.jl)
A package for combining parsers and transforming strings into julia types.

Compose parsers parsimoneously within a functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inference for transformations,
log conveniently for debugging, and let Julia compile your parser for performance.

The `CombinedParsers` design 
- is fast due to Julia parametric types, and compiler optimizations with generated functions,
- parsing result transformations infer the domain data types,
- is composable and optimizable with Julia method dispatch,
- provides flexible public API for parsing, matching, iteration
- can be defined with PCRE and EBNF syntax.


## Getting started
`CombinedParsers.jl` is a registered package.
Install with
```julia
] add CombinedParsers
```

### Example: rational numbers arithmetics
This example demonstrates reading of arithmetical terms for rational numbers.
Reflecting operator precedence, `term` are `subterm`s, interleaved by */, 
and `subterm`s are [`Either`](@ref) integer numbers 
```julia
@syntax subterm = Either{Rational{Int}}([NumericParser(Int)]; convert=true)
```
or a `subterm` can also be an additive `term` in `parentheses`:
```julia
@syntax for parentheses in subterm
    mult = evaluate |> join(subterm, CharIn("*/"), infix=:prefix )
    @syntax term = evaluate |> join(mult,    CharIn("+-"), infix=:prefix )
    Sequence(2,'(',term,')')
end
```

This `CombinedParser` definition in 5,5 lines registers a `@term_string` macro for parsing and evaluating rational arithmetics:
```jldoctest
julia> term"4*10+2"
42//1
```

[Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))


Details in [the full documentation example](https://gkappler.github.io/CombinedParsers.jl/dev/man/example-arithmetic).

## Acknowledgements
This package leverages Julia's compiler and superior type system to parsing.

I am thankful for contributions and inspiration from many great packages:
### [TextParse.jl](https://github.com/queryverse/TextParse.jl)
> A bunch of fast text parsing tools, used in CSV.jl

`CombinedParsers` composes with fast
[TextParse.jl](https://github.com/queryverse/TextParse.jl) both ways 
because `CombinedParser <: TextParse.AbstractToken`
and by providing a method for `TextParse.tryparsenext`,
(leveraging the supreme Julia compiler, type and package architecture).

- If you seek support with a CSV example, please contact me (e.g. address text field parsing).

### Inspirations

- The work was strongly inspired by the great Scala [fastparse](https://github.com/lihaoyi/fastparse) package, and also the [elm parser](https://package.elm-lang.org/packages/elm/parser/latest/).
- [Parsers.jl](https://github.com/JuliaData/Parsers.jl), a collection of parsers for date and primitive types, inspired the `parse` methods.
- [Automa.jl](https://github.com/BioJulia/Automa.jl), a Julia package for text validation, parsing, and tokenizing based on state machine compiler.  The package compiles deterministic finite automata.  (Currently there is no inter-operation possible, because in `Automa` processing of parsed tokens is done with actions).
- [ParserCombinator.jl](https://github.com/andrewcooke/ParserCombinator.jl) was a great inspiration.
  Yet I decided for a new design with a focus on transformations and type inference with parametric types, instead of basing this work off `ParserCombinator`, written before 2016 (and fixed for Julia 1.0 in 2018).
  `CombinedParsers` integrates into the Julia 1.0 Iteration API, small `Union{Nothing,T} where T` types instead of using Nullables, compiler optimizations and generated functions.
  I want to provide benchmarks comparisons with `ParserCombinator.jl`.

## Next Steps
- [ ] Syntax freeze -- your comments are appreciated!
- [ ] decide for a error tracing strategy, backtracking. If you want to collaborate on stepping & debugging, please reach out to me.
- [ ] Performance optimizations
- [ ] streaming
- [ ] test coverage underestimated (PCRE tests are not included in travis)
- [ ] [![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle)

# Contributing and Questions
Contributions and feedback are very welcome, 
especially regarding brief syntax and constructor dispatch. 
Please open an issue if you encounter any problems or would just like to ask a question,
or contact me at mail@g-kappler.de.


