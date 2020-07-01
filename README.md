
# CombinedParsers in pure Julia
<!-- [![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://gkappler.github.io/CombinedParsers.jl/stable) -->
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://gkappler.github.io/CombinedParsers.jl/dev)
[![Build Status](https://travis-ci.org/gkappler/CombinedParsers.jl.svg?branch=master)](https://travis-ci.com/github/gkappler/CombinedParsers.jl)
[![Codecov](https://codecov.io/gh/gkappler/CombinedParsers.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/gkappler/CombinedParsers.jl)
A package for combining parsers and transforming strings into julia types.

Compose parsers parsimoneously within a functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inference for transformations,
log conveniently for debugging, and let Julia compile your parser for performance.


> `CombinedParsers.jl` is currently an α release.	The first official released is prepared for JuliaCon2020.

##### Package Features

- Speed
  - [write parsers faster than `Base.PCRE`](man/pcre-compliance.md), optimized by the Julia compiler for parametric parser and state types.
  - Fast `@generated function`s for sequences.
  - Fast Trie-based scanning ([example](man/example-either-trie.md))
  - Compile with your custom parsing algorithm ([example](man/example-palindromes.md))
  - (planned: memoization)
  - (planned: lazy transformations)
- Simplicity
  - Clear [`@syntax`](@ref) integrates [`map`](@ref) transformations with Julia [`result_type`](@ref) inference.
  - [AbstractTrees.jl](https://github.com/JuliaCollections/AbstractTrees.jl) interface provides colored and clearly layed out printing in the REPL.
  - Convenient logging of the parsing process [`with_name`](@ref)s and [`SideeffectParser`](@ref)s.
- Interoperability
  - [TextParse.jl](https://github.com/queryverse/TextParse.jl): existing `TextParse.AbstractToken` implementations can be used with CombinedParsers. `CombinedParser` provide `TextParse.tryparsenext` and can be used e.g. in CSV.jl.
  - Pure Julia regular expression parsers are provided with the [`@re_str`](@ref) macro, a plug-in replacement for `Base.@r_str`.
  - Tested on the [PCRE pattern test set](man/pcre-compliance.md).
- Generality
  - UTF8
  - All valid parsings can be [`Base.iterate`](@ref)d lazily.
  - Higher-order parsers depending on the parsing state allow for not context-free parsers ([`after`](@ref)).
  - CombinedParsers generalize from strings to parsing any sequence type supporting `getindex`, `nextind`, `prevind` methods.


## Getting started
The [Overview](@ref) provides a tutorial explaining how to get started using CombinedParsers.
The [User guide](man/user.md) provides a summary of CombinedParsers types.
Some examples of packages using CombinedParsers can be found on the [Examples](@ref) page.
See the [Index](@ref main-index) for the complete list of documented functions and types.

### Writing Parsers
Install with
```julia
] add https://github.com/gkappler/CombinedParsers.jl
```

CombinedParsers provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.
Combinator constructors are discussed in the [user guide](man/user.md).

Parsing is reading and transforming a sequence of characters.
This example reads and evaluates arithmetical terms for rational numbers.
Subterms can use algebraic operators `+-*/` that will be evaluated with 
```@repl session
function evaluate( (start, operation_values) )
    aggregated_value::Rational{Int} = start
    for (op,val) in operation_values
        aggregated_value = eval( Expr(:call, Symbol(op), 
			              aggregated_value, val
			              ))
    end
    return aggregated_value
end
evaluate( (0, [ ('+',1), ('+',1) ]) )
evaluate( (1, [ ('*',2), ('*',3) ]) )
```

A term expression has sub terms, e.g. fast `TextParse.Numeric(Int)` integer numbers, converted to `Rational{Int}`:
```@example session
using CombinedParsers
using TextParse
@syntax subterm = Either{Rational{Int}}(
	Any[ map(Rational{Int}, TextParse.Numeric(Int)) ])
nothing # hide
```



Terms are sequences of subterms interleaved with operators.
CombinedParsers.Sequence can be built with operator `*` like Julia string concatenation.
```@example session
rational_products = Sequence(
	evaluate, 
	subterm, Repeat(
		Sequence( CharIn("*/"), subterm ) )
)

@syntax term = map(
    evaluate, 
    rational_products*Repeat(
		CharIn("+-") * rational_products
	)
);
nothing # hide
```


A subterm can also be a nested term in parenthesis
```@example session
@syntax for parenthesis in subterm
    Sequence(2,"(",term,")")
end
nothing # hide
```

`@syntax` registers `@term_string` macro for parsing.
```@repl session
term"(1+2)/5"
```

The defined `CombinedParser` `term` can be used as a function for colorful logging of the parsing process.
```@repl session
term("1/((1+2)*4+3*(5*2))",log = [:parenthesis])
```
[Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))



# Acknowledgements

I am thankful for contributions and inspiration from many great packages:
## [TextParse.jl](https://github.com/queryverse/TextParse.jl)
> A bunch of fast text parsing tools, used in CSV.jl

`CombinedParsers` composes with fast
[TextParse.jl](https://github.com/queryverse/TextParse.jl) both ways 
because `CombinedParser <: TextParse.AbstractToken`
and by providing a method for `TextParse.tryparsenext`,
(leveraging the supreme Julia compiler, type and package architecture).

- If you seek support with a dates parser example, please contact me.
- If you seek support with a CSV example, please contact me (e.g. address text field parsing).

## Inspirations
- The work was strongly inspired by the great Scala [fastparse](https://github.com/lihaoyi/fastparse) package, and also the [elm parser](https://package.elm-lang.org/packages/elm/parser/latest/).
- [Parsers.jl](https://github.com/JuliaData/Parsers.jl), a collection of parsers for date and primitive types, inspired the [`parse`](@ref) methods.
- [Automa.jl](https://github.com/BioJulia/Automa.jl), a Julia package for text validation, parsing, and tokenizing based on state machine compiler.  The package compiles deterministic finite automata.  (Currently there is no inter-operation possible, because in `Automa` processing of parsed tokens is done with actions and UTF8 support is lacking).
- [ParserCombinator.jl](https://github.com/andrewcooke/ParserCombinator.jl) was a great inspiration.
  Yet I decided for a new design with a focus on transformations and type inference with parametric types, instead of basing this work off `ParserCombinator`, written before 2016 (and fixed for Julia 1.0 in 2018).
  `CombinedParsers` integrates into the Julia 1.0 Iteration API, small `Union{Nothing,T} where T` types instead of using Nullables, compiler optimizations and generated functions.
  I want to provide benchmarks comparisons with `ParserCombinator.jl`.


# Contributing and Questions
Contributions are very welcome, as are feature requests and suggestions. Please open an issue if you encounter any problems or would just like to ask a question.

<!-- CombinedParsers is a package by Gregor Kappler. If you use and appreciate CombinedParsers.jl, please support development at patreon. -->

## Next Steps
- [ ] test coverage
- [ ] Performance optimizations
    - parsing memoization
- [ ] [![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle)
- [ ] Publishing packages for parsing wikitext and orgmode markup
- [ ] error backtracking, stepping & debugging




