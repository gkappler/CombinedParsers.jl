# CombinedParsers.jl Documentation

A package for combining parsers and transforming strings into julia types.

Compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inferrence for transformations,
log conveniently for debugging, and let Julia compile your parser for good performance.


!!! note
    `CombinedParsers.jl` is currently an α release.	The first official released is prepared for JuliaCon2020.

##### Package Features

- Clear syntax integrates grammar and transformations with Julia type inference.
- Higher-order parsers depending on the parsing state allow for not context-free parsers.
- All valid parsings can be iterated lazily.
- Interoperable with [TextParse.jl](https://github.com/queryverse/TextParse.jl): existing `TextParse.AbstractToken` implementations can be used with CombinedParsers. `CombinedParser` provide `TextParse.tryparsenext` and can be used e.g. in CSV.jl.
- Parametric parser and state types enable Julia compiler optimizations.
- Compiled regular expression parsers in pure julia are provided with the `re_str` macro.
- [AbstractTrees.jl](https://github.com/JuliaCollections/AbstractTrees.jl) interface provides colored and clearly layed out printing in the REPL.
- Convenient logging of the parsing process with `NamedParser`s and `SideeffectParser`s.
- CombinedParsers generalize from strings to parsing any type supporting `getindex`, `nextind`, `prevind` methods.


## Getting started
Install with
```julia
] add https://github.com/gkappler/CombinedParsers.jl
```

The [Overview](@ref) provides a tutorial explaining how to get started using CombinedParsers.
The [User guide](man/user.md) provides a summary of CombinedParsers types.
Some examples of packages using CombinedParsers can be found on the [Examples](@ref) page.
See the [Index](@ref main-index) for the complete list of documented functions and types.

## Writing Parsers
CombinedParsers provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.
Combinator constructors are discussed in the [user manual](man/user.md).

A term expression has
```@example session
using CombinedParsers
@syntax subterm = Either{Rational{Int}}()

# A integer subterm can be parsed with fast TextParse.jl parsers:
using TextParse
@syntax for number in subterm
    map(Rational{Int},TextParse.Numeric(Int))
end

# Subterms can use algebraic operators `+-*/` that will be evaluated
function evaluate( (start, operation_values) )
    aggregated_value::Rational{Int} = start
    for (op,val) in operation_values
        aggregated_value = eval( Expr(:call, Symbol(op), 
			              aggregated_value, val
			              ))
    end
    return aggregated_value
end

# Terms are sequences of subterms interleaved with operators.
# CombinedParsers.Sequence can be built with operator `*` like Julia string concatenation.
rational_products = Sequence(
	evaluate, 
	subterm, Repeat(CharIn("*/"), subterm ) 
)

@syntax term = map(
    evaluate, 
    rational_products*Repeat(CharIn("+-")*rational_products)
);


# A subterm can also be a nested term in parenthesis
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

The parser representation can be printed as a tree
```@repl session
term
```

[Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))

Parsing has been a painpoint in all of my work as a developer and a researcher with a focus on text.


## Examples
```@contents
Pages = [
    "man/json.md",
    "man/pcre.md",
]
Depth = 5
```

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

The work was inspired by Scala [fastparse](https://github.com/lihaoyi/fastparse) package and the Julia parsing packages
### [Parsers.jl](https://github.com/JuliaData/Parsers.jl)
A collection of parsers for date and primitive types.

### [TextParse.jl](https://github.com/queryverse/TextParse.jl)
A bunch of fast text parsing tools.
- used in CSV.jl
- Nullables.jl

I aim at making CombinedParsers compose with
`[TextParse.jl](https://github.com/queryverse/TextParse.jl)` both ways 
by type `CombinedParser <: TextParse.AbstractToken`
and providing a method for `TextParse.tryparsenext`.
I will contribute soon a CSV example with address text field parsing.


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

## Library Outline

```@contents
Pages = [ "lib/public.md", "lib/internals.md" ]
Depth = 5
```
### [Index](@id main-index)

```@index
Pages = ["lib/public.md"]
Pages = ["lib/internals.md"]
```

