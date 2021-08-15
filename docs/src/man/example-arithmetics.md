# Rational Numbers
Arithmetic expressions are sequences of integer numbers interleaved with operators +-/*, possibly in parentheses.
Evaluating an arithmetic expression results in a rational number.
Expressions in parentheses are evaluated first, the operators * and / take precedence over + and -.

Parsing is reading and processing a sequence of characters, e.g. an arithmetic expression.

## `CombinedParsers` 
provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.
This example demonstrates reading of arithmetical terms for rational numbers.
```@meta
DocTestSetup = quote
    using BenchmarkTools
    using CombinedParsers
    using TextParse
    function evaluate( (start, operation_values) )
        aggregated_value::Rational{Int} = start
        for (op,val) in operation_values
            aggregated_value = eval( Expr(:call, Symbol(op), 
    			              aggregated_value, val
    			              ))
        end
        return aggregated_value
    end
    @syntax subterm = Either{Rational{Int}}([NumericParser(Int)]; convert=true)
    @syntax for parentheses in subterm
        mult = evaluate |> join(subterm, CharIn("*/"), infix=:prefix )
        @syntax term = evaluate |> join(mult,    CharIn("+-"), infix=:prefix )
        Sequence(2,'(',term,')')
    end;
end
```

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

!!! note
    No need to roll our own integer parser, we can use [`NumericParser`](@ref) composing `TextParse.Numeric(Int)`, automatically converted to `Rational{Int}`.
    If `convert=false` an error would be raised on construction, the default.

    
!!! note
    `CombinedParsers` provides constructors and operators:
    - [`Base.join`](@ref)`(x,infix; infix=:prefix)`:  shorthand for `x `[`(*)`](@ref)` `[`Repeat`](@ref)`( infix * x  )`,
    - `a*b`: shorthand to [`Sequence`](@ref)`(a,b)`, 
    - `f |> parser`: shorthand for [`map`](@ref)`(f,parser)`, and
    - `evaluate::Function` is detailed at the end of the page.

You can investigate the matching process with logging.
The defined `CombinedParser` `term` can be used as a function with a `log` keyword option.
```jldoctest
julia> term("(1+2)/5", log=true)
   match subterm@2-3: (1+2)/5
                       ^
   match subterm@4-5: (1+2)/5
                         ^
   match term@2-5: (1+2)/5
                    ^_^
   match parentheses@1-6: (1+2)/5
                          ^___^
   match subterm@1-6: (1+2)/5
                      ^___^
   match subterm@7-8: 1+2)/5
                           ^
   match term@1-8: (1+2)/5
                   ^_____^
3//5
```

Logging technically rewrites a parser with annotation side-effects (see [`deepmap_parser`](@ref)).

You can flexibly fine-tune logging by name, type or any labeling function.
```jldoctest
julia> term("1/((1+2)*4+3*(5*2))",log = [:parentheses])
   match parentheses@4-9: 1/((1+2)*4+3*(
                             ^___^
   match parentheses@14-19: *4+3*(5*2))
                                 ^___^
   match parentheses@3-20: 1/((1+2)*4+3*(5*2))
                             ^_______________^
1//42
julia> term("4*10+2", log = NumericParser)
   match <Int64>@1-2: 4*10+2
                      ^
   match <Int64>@3-5: 4*10+2
                        ^^
   match <Int64>@6-7: 4*10+2
                           ^
42//1
```

[Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))

## Parser printing
The parser representation can be printed as a tree.
Each tree node starts with a brief oriented at PCRE regular expression syntax (blue in REPL).
The node then lists parser constructors, delimited by ` |> `.
This is especially useful for understanding PCRE regular expressions and BNF grammars: the tree parser representation is really clear about how you would construct the parser with `CombinedParsers`.
```jldoctest
julia> term
🗄 Sequence |> map(evaluate) |> with_name(:term)
├─ 🗄 Sequence |> map(evaluate)
│  ├─ |🗄 Either |> with_name(:subterm)
│  │  ├─ 🗄 Sequence |> map(#54) |> with_name(:parentheses)
│  │  │  ├─ \( 
│  │  │  ├─ 🗄 Sequence |> map(evaluate) |> with_name(:term) # branches hidden
│  │  │  └─ \) 
│  │  └─  <Int64> |> map(Rational{Int64})
│  └─ 🗄* Sequence |> Repeat
│     ├─ [\*/] ValueIn
│     └─ |🗄 Either |> with_name(:subterm) # branches hidden
└─ 🗄* Sequence |> Repeat
   ├─ [\+\-] ValueIn
   └─ 🗄 Sequence |> map(evaluate) # branches hidden
::Rational{Int64}
```


## Benchmarks
Parsing times for Int, operators, brackets are
```repl
@benchmark match(term,"(1+2)/5") 
```

in unfair benchmark-comparison with the more expressive Julia syntax parser
```repl
julia> @benchmark Meta.parse("(1+2)/5")
```

Parsing and transforming (here `eval`)
```repl
julia> @benchmark term("(1+2)/5") 
```

compared to Julia 
```repl
julia> @benchmark eval(Meta.parse("(1+2)/5"))
```

## Transformation by `evaluate`
Subterms can use algebraic operators `+-*/` that will be evaluated with 
```julia
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

```jldoctest
julia> evaluate( (0, [ ('+',1), ('+',1) ]) )
2//1

julia> evaluate( (1, [ ('*',2), ('*',3) ]) )
6//1
```
