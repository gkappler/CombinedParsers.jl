# CombinedParsers.jl
*faster write fast parsers in julia.*

```@docs
CombinedParsers
```	

## Package Features

Compose parsers parsimoneously within a functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inference for transformations,
log conveniently for debugging, and let Julia compile your parser for performance.

### Speed

  - parametric parser and state types
  - optimized julia `@generated function`s
  - fast trie-based scanning ([example](man/example-either-trie.md)), compile with your custom parsing algorithm ([example](man/example-palindromes.md))
  - often matches faster than C library `PCRE` regular expressions ([benchmarks](man/pcre-compliance.md))
  - lazy transformations (for [`Sequence`](@ref) and [`Repeat`](@ref))
  
### Simplicity

  - Define parser and result construction without redundancy: 
    Julia infers [`result_type`](@ref)(parser) in [`map`](@ref) and [`@syntax`](@ref).
  - [AbstractTrees.jl](https://github.com/JuliaCollections/AbstractTrees.jl) printing in the REPL. 
  - [`with_log`](@ref) provides colored logging of the parsing [`with_name`](@ref)s.
  
### Interoperability

  - [TextParse.jl](https://github.com/queryverse/TextParse.jl) can be composed with CombinedParsers and vice versa. 
  - [`@re_str`](@ref) provides pure Julia regular expressions as plug-in replacement for `Base.@r_str`
    ([PCRE pattern unit test set](man/pcre-compliance.md)).

### Generality

  - Lazily [`iterate`](@ref) all valid parsings.
  - Higher-order parsers: [`after`](@ref) matching a left-hand parser a right-hand parser is constructed in arbitrary function of the left-hand match.
  - Parse binary data `Vector{UInt8}` and any sequence type supporting `getindex`, `nextind`, `prevind` methods (cf. [bson example](man/bson.md)).

## Getting started

`CombinedParsers.jl` is a registered package.
Install with
```julia
] add CombinedParsers
```

- The [Overview](@ref) provides a tutorial explaining how to get started using CombinedParsers.
- The [User guide](man/user.md) provides a summary of CombinedParsers types and constructors.
- Some examples of packages using CombinedParsers can be found on the [Examples](@ref) page.
- [Matching and parsing](lib/public.md)
- [Parser Templates](lib/parsers.md)
- [Parser Construction](lib/constructors.md)
- [composing with regular expressions](lib/regexp.md)
- [Transformations](lib/transformation.md)

See the [Index](@ref main-index) for the complete list of documented functions and types.

If you prefer a video introduction:

| 8-min JuliaCon2020 talk                                                                                           | 3h JuliaCon2021 workshop                                                                                              |
|-------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------|
| [![JuliaCon2020 talk](https://img.youtube.com/vi/YBMJSKwwCT0/0.jpg)](https://www.youtube.com/watch?v=YBMJSKwwCT0) | [![JuliaCon2021 workshop](https://img.youtube.com/vi/RpCnP-S7txI/0.jpg)](https://www.youtube.com/watch?v=RpCnP-S7txI) |

### Example: rational numbers arithmetics
Parsing is reading and transforming a sequence of characters.
`CombinedParsers` provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.
```@example session
using CombinedParsers
using TextParse
nothing # hide
```

This example reads and evaluates arithmetical terms for rational numbers.
The following defines an evaluating parser for rational number terms as sequences of subterms interleaved with operators.
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
Subterms are [`Either`](@ref) integer numbers, `TextParse.Numeric(Int)` converted to `Rational{Int}`,
or subterms are written as parentheses around a nested term:
```@example session
@syntax subterm = Either{Rational{Int}}(Any[TextParse.Numeric(Int)]; convert=true);
@syntax for parenthesis in subterm
    mult         = evaluate |> join(subterm, CharIn("*/"), infix=:prefix )
    @syntax term = evaluate |> join(mult,    CharIn("+-"), infix=:prefix )
    Sequence(2,'(',term,')')
end;
nothing # hide
```
The [`@syntax`](@ref) definition in 5,5 lines is sufficient for parsing and evaluating arithmetics:
[`Base.join`](@ref)`(x, delimiter; infix=:prefix)` is shorthand for [`Sequence`](@ref)`(x ,`[`Repeat`](@ref)`( delimiter * x  ))`,
and `f |> parser` is shorthand for [`map`](@ref)`(f,parser)`.
That's all! [`@syntax`](@ref) registers a `@term_string` macro for parsing and transforming:
```@repl session
result_type(term)
term"(1+2)/5"
# The defined `CombinedParser` `term` function 
# provides optional logging of the parsing process.
term("1/((1+2)*4+3*(5*2))",log = [:parenthesis])
```
[Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))


Note: The `evaluate` function definition is detailed in [the full example](man/example-arithmetics.md).
```@repl session
evaluate( (0, [ ('+',1), ('-',2) ]) )
evaluate( (1, [ ('*',4), ('/',3) ]) )
```


## Acknowledgements

This package is enabled only due to the Julia's compiler and superior type system.
Thankfully: a really concise language for powerful computing!

I am thankful for contributions and inspiration from many great packages:
### [TextParse.jl](https://github.com/queryverse/TextParse.jl)
> A bunch of fast text parsing tools, used in CSV.jl

`CombinedParsers` composes with 
[TextParse.jl](https://github.com/queryverse/TextParse.jl) both ways 
(`CombinedParser <: TextParse.AbstractToken` and provides a method for [`CombinedParsers.tryparsenext`](@ref))


### Inspirations

- The work was strongly inspired by the great Scala [fastparse](https://github.com/lihaoyi/fastparse) package, and also the [elm parser](https://package.elm-lang.org/packages/elm/parser/latest/).
- [Parsers.jl](https://github.com/JuliaData/Parsers.jl), a collection of parsers for date and primitive types, inspired the [`parse`](@ref) methods.
- [Automa.jl](https://github.com/BioJulia/Automa.jl), a Julia package for text validation, parsing, and tokenizing based on state machine compiler.  The package compiles deterministic finite automata.  (Currently there is no inter-operation possible, because in `Automa` processing of parsed tokens is done with actions).
- [ParserCombinator.jl](https://github.com/andrewcooke/ParserCombinator.jl) was a great inspiration.
  Yet I decided for a new design with a focus on transformations and type inference with parametric types, instead of basing this work off `ParserCombinator`, written before 2016 (and fixed for Julia 1.0 in 2018).
  `CombinedParsers` integrates into the Julia 1.0 Iteration API, small `Union{Nothing,T} where T` types instead of using Nullables, compiler optimizations and generated functions.
  I want to provide benchmarks comparisons with `ParserCombinator.jl`.

# Contributing and Questions
Contributions and feedback are very welcome, 
especially regarding brief syntax and constructor dispatch. 
Please open an issue if you encounter any problems or would just like to ask a question,
or contact me at mail@g-kappler.de.


## Next Steps
- [ ] Syntax freeze -- your comments are appreciated!
- [ ] decide for a error tracing strategy, backtracking. If you want to collaborate on stepping & debugging, please reach out to me.
- [ ] Performance optimizations
- [ ] streaming
- [ ] test coverage underestimated (PCRE tests are not included in travis)
- [ ] [![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle)


## Outline
### Manual
```@contents
Pages = [
	"man/guide.md",
	"man/user.md",
	"man/example-either-trie.md",
	]
Depth = 5
```

### Examples
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


### Library API

```@contents
Pages = [ "lib/public.md", "lib/parsers.md", "lib/constructors.md", "lib/transformation.md", "lib/regexp.md", "lib/internals.md" ]
Depth = 5
```

## [Index](@id main-index)

```@index
Pages = [ "lib/public.md", "lib/parsers.md", "lib/constructors.md", "lib/transformation.md", "lib/regexp.md", "lib/internals.md" ]
Modules = [ CombinedParsers, CombinedParsers.Regexp ]
Order = [ :function, :type ]
```

