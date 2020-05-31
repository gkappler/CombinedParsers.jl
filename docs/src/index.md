# CombinedParsers.jl Documentation

A package for combining parsers and transforming strings into julia types.

Compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inferrence for transformations,
log conveniently for debugging, and let Julia compile your parser for good performance.


!!! note
    `CombinedParsers.jl` is currently an α release.	The first official released is prepared for JuliaCon2020.


The [Overview](@ref) provides a tutorial explaining how to get started using CombinedParsers.
The [User guide](man/user.md) provides a summary of CombinedParsers types.
Some examples of packages using CombinedParsers can be found on the [Examples](@ref) page.
See the [Index](@ref main-index) for the complete list of documented functions and types.

## Package Features

- Clear syntax integrates grammar and transformations with Julia type inference.
- Higher-order parsers depending on the parsing state allow for not context-free parsers.
- All valid parsings can be iterated lazily.
- Interoperable with [TextParse.jl](https://github.com/queryverse/TextParse.jl): existing `TextParse.AbstractToken` implementations can be used with CombinedParsers. `CombinedParser` provide `TextParse.tryparsenext` and can be used e.g. in CSV.jl.
- Parametric parser and state types enable Julia compiler optimizations.
- Compiled regular expression parsers in pure julia are provided with the `re_str` macro.
- [AbstractTrees.jl](https://github.com/JuliaCollections/AbstractTrees.jl) interface provides colored and clearly layed out printing in the REPL.
- Convenient logging of the parsing process with `NamedParser`s and `SideeffectParser`s.
- CombinedParsers generalize from strings to parsing any type supporting `getindex`, `nextind`, `prevind` methods.


CombinedParsers.jl is tested and benchmarked against the PCRE C library testset.
```@contents
Pages = [
    "man/pcre-compliance.md",
]
Depth = 5
```


## Writing Parsers
CombinedParsers provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.
Combinator constructors are discussed in the [user manual](man/user.md).

```@repl

# Julia eval for +-*/ operators
function eval_ops((l,opr))
    for (op,val) in opr
        l = eval( Expr(:call, Symbol(op), l, val))
    end
    l::Rational{Int}
end

using CombinedParsers
using TextParse

@with_names begin
	number = map(Rational{Int}, TextParse.Numeric(Int))
	factor = Either(number)  # or expression in parenthesis, see push! below
	divMul = map(eval_ops, Sequence( factor, Repeat( CharIn("*/"), factor ) ) )
	addSub = map(eval_ops, divMul * Repeat( CharIn("+-") * divMul ) )
	parens = Sequence(2, "(",addSub,")" )
	push!(factor, parens)
	expr = (addSub * AtEnd())[1]
end

parse(expr, "1/((1+2)*4+3*(5*2))", log=[:parens])
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


## Library Outline

```@contents
Pages = [ "lib/public.md", "lib/internals.md" ]
Depth = 5
```


# Acknowledgements

The work was inspired by Scala [fastparse](https://github.com/lihaoyi/fastparse) package and the Julia parsing packages
### [Parsers.jl](https://github.com/JuliaData/Parsers.jl)
A collection of parsers for date and primitive types.

### [TextParse.jl](https://github.com/queryverse/TextParse.jl)
A bunch of fast text parsing tools.
- used in CSV.jl
- Nullables.jl

TextParse.jl integrates with CombinedParsers.jl both ways 
by type `CombinedParser <: TextParse.AbstractToken`
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
Pages = ["lib/internals.md"]
```

