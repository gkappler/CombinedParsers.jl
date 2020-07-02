# CombinedParsers.jl Documentation

A package for combining parsers and transforming strings into julia types.

Compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inference for transformations,
log conveniently for debugging, and let Julia compile your parser for performance.


!!! note
    `CombinedParsers.jl` is currently an α release.	The first official released is prepared for JuliaCon2020.

## Package Features

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

Install with
```julia
] add https://github.com/gkappler/CombinedParsers.jl
```

## Example: read and evaluate arithmetical terms for rational numbers
Parsing is reading and transforming a sequence of characters.
This example reads and evaluates arithmetical terms for rational numbers.
Subterms can use algebraic operators `+-*/` that will be evaluated with 
```@setup session
function evaluate( (start, operation_values) )
    aggregated_value::Rational{Int} = start
    for (op,val) in operation_values
        aggregated_value = eval( Expr(:call, Symbol(op), 
			              aggregated_value, val
			              ))
    end
    return aggregated_value
end
```
```@repl session
evaluate( (0, [ ('+',1), ('+',1) ]) )
evaluate( (1, [ ('*',2), ('*',3) ]) )
```
The function definition is detailed in [the full example](man/example-arithmetic.md).

### `TextParse.Numeric(Int)`
`CombinedParsers` provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.
Combinator constructors are discussed in the [user guide](man/user.md).
A term expression has sub terms, either fast `TextParse.Numeric(Int)` integer numbers, converted to `Rational{Int}`, or something in parentheses, which is attended to below:
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

The parser representation can be printed as a tree
```@repl session
term
```

# Useful Design
- WikitextParser.jl is a `CombinedParser` for parsing [wikitext syntax](https://en.wikipedia.org/wiki/Help:Wikitext),
  quite comprehensibly and representing Wikipedia articles within Julia.
- OrgmodeParser.jl is a `CombinedParser` for parsing main [org mode](https://orgmode.org/) syntax,
  representing org files within Julia.
- CombinedParserTools.jl is currently more or less my own workspace to provide a set of re-useable parsers.
- Tries.jl is the abstract implementation of the fast prefix-tree matching in `CombinedParsers` (see [docs](https://gkappler.github.io/CombinedParsers.jl/dev/man/example-either-trie/))
If you want to work with any of these open source packages, I will gladly provide professional support.
If you are writing your own recursive `CombinedParser` and seek inspiration, you might find these comprehensive examples interesting.
(pre-\alpha, so beware, dragons!)

The `CombinedParsers` design 
- is fast due to Julia parametric types, and compiler optimizations with generated functions,
- its strictly typed parsing defines the domain data types,
- is composable and optimizable with Julia method dispatch,
- provides flexible public API for parsing, matching, iteration

Making Julia parametric types central for the parser design allows equal automation of the data pipeline after parsing!
- fast db-indexing of text streams (e.g. logging): If you need support indexing logging streams into a (SQL-)Database, the (currently) proprietary TypeGraphs.jl provides `CombinedParsers` plug and play: Table schemas are infered from your parser.
- fast HTTP-serving of parsed data: If you need support with a parsing server-client infrastructure, the (currently) proprietary GraphQLAlchemy.jl provides `CombinedParsers` plug and play: GraphQL schemas and resolver are infered from your parser.
- fast out-of core data science/AI on your parsed data: If you need support with storing parsed data in optimized memory-mapped JuliaDB, TypeDB.jl provides `CombinedParsers` plug and play. 
- fast scientific measurements in a data graph: FilingForest IA.jl provides `CombinedParsers` plug and play: even for recursively nested data.
All (currently) proprietary packages are default-over-configuration for fast integration, and are in active development.


## Optimization Strategy
CombinedParsers.jl is tested and benchmarked against the PCRE C library testset.
```@contents
Pages = [
    "man/pcre-compliance.md",
]
Depth = 5
```

This strategy allows for efficient benchmarking of code optimizations on many Regex and other syntax patterns.
Explorations for optimization are in git branches:
```@contents
Pages = [ "benchmarks/public.md", "benchmarks/internals.md" ]
Depth = 5
```

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


```@docs
CombinedParsers.tryparsenext
```

## Inspirations
- The work was strongly inspired by the great Scala [fastparse](https://github.com/lihaoyi/fastparse) package, and also the [elm parser](https://package.elm-lang.org/packages/elm/parser/latest/).
- [Parsers.jl](https://github.com/JuliaData/Parsers.jl), a collection of parsers for date and primitive types, inspired the [`parse`](@ref) methods.
- [Automa.jl](https://github.com/BioJulia/Automa.jl), a Julia package for text validation, parsing, and tokenizing based on state machine compiler.  The package compiles deterministic finite automata.  (Currently there is no inter-operation possible, because in `Automa` processing of parsed tokens is done with actions and UTF8 support is lacking).
- [ParserCombinator.jl](https://github.com/andrewcooke/ParserCombinator.jl) was a great inspiration.
  Yet I decided for a new design with a focus on transformations and type inference with parametric types, instead of basing this work off `ParserCombinator`, written before 2016 (and fixed for Julia 1.0 in 2018).
  `CombinedParsers` integrates into the Julia 1.0 Iteration API, small `Union{Nothing,T} where T` types instead of using Nullables, compiler optimizations and generated functions.
  I want to provide benchmarks comparisons with `ParserCombinator.jl`.

## Examples
```@contents
Pages = [
"man/example-person.md",
"man/example-number-ranges.md",
 "man/pcre.md",
 "man/example-palindromes.md",
 "man/json.md",
]
Depth = 5
```

# Library Outline

```@contents
Pages = [ "lib/public.md", "lib/internals.md" ]
Depth = 5
```
### [Index](@id main-index)

```@index
Pages = ["lib/public.md", "lib/internals.md"]
Modules = [ CombinedParsers, CombinedParsers.Regexp ]
Order = [ :function, :type ]
```

