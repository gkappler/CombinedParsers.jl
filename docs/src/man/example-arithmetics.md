```@meta
EditURL = "https://github.com/gkappler/CombinedParsers.jl/docs/../docs/src/man/example-arithmetics.jl"
```

# Arithmetical terms for rational numbers
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

```
The function definition is detailed in [the full example](man/example-arithmetic.md).

```@repl session
using CombinedParsers
```

`CombinedParsers` provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.
Combinator constructors are discussed in the [user guide](man/user.md).

```@repl session
using TextParse
```

Term expressions are sequences of subterms interleaved with operators.
Sub terms are [`Either`](@ref) fast `TextParse.Numeric(Int)` integer numbers, converted to `Rational{Int}`,

```@repl session
@syntax subterm = Either{Rational{Int}}(TextParse.Numeric(Int));
nothing #hide
```

A subterm can also be a nested term in parentheses

```@repl session
@syntax for parenthesis in subterm
    mult = evaluate |> join(subterm, CharIn("*/"), infix=:prefix )
    adds = evaluate |> join(mult,    CharIn("+-"), infix=:prefix )
    Sequence(2,'(',adds,')')
end;
nothing #hide
```

This `CombinedParser` definition in 5,5 lines is sufficient for doing arithmetics:
[`Base.join`](@ref)(x,infix; infix=:prefix) is shorthand for `x `[`*`](@ref)` `[`Repeat`](@ref)`( infix * x  )`,
and `f |> parser` is shorthand for [`map`](@ref)(f,parser)`.

```@repl session
@syntax term = sums;
nothing #hide
```

registers a `@term_string` macro for parsing and transforming.

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

### Benchmarks
Parsing times for Int, operators, brackets are

```@repl session
using BenchmarkTools
@benchmark match(term,"(1+2)/5")
```

in unfair benchmark-comparison with the more expressive Julia syntax parser

```@repl session
@benchmark Meta.parse("(1+2)/5")
```

Parsing and transforming (here `eval`)

```@repl session
@benchmark term("(1+2)/5")
```

compared to Julia

```@repl session
@benchmark eval(Meta.parse("(1+2)/5"))
```

---

*This page was generated using [Literate.jl](https://github.com/fredrikekre/Literate.jl).*

