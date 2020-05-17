CombinedParsers is a package for parsing into julia types.
Parsimoneously compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inferrence for transformations,
log conveniently for debugging, and let Julia compile your parser for good performance.

Features:
- Clear syntax integrates grammar and transformations with Julia type inference.
- Higher-order parsers depending on the parsing state allow for not context-free parsers.
- All valid parsings can be iterated lazily.
- Interoperable with [TextParse.jl](https://github.com/queryverse/TextParse.jl): existing `TextParse.AbstractToken` implementations can be used with CombinedParsers. `CombinedParsers.AbstractParser` provide `TextParse.tryparsenext` and can be used e.g. in CSV.jl.
- Parametric parser and state types enable Julia compiler optimizations.
- Compiled regular expression parsers in pure julia are provided with the `re_str` macro.
- [AbstractTrees.jl](https://github.com/JuliaCollections/AbstractTrees.jl) interface provides colored and clearly layed out printing in the REPL.
- Convenient logging of the parsing process with `NamedParser`s and `SideeffectParser`s.
- CombinedParsers generalize from strings to parsing any type supporting `getindex`, `nextind`, `prevind` methods.


```julia
"Julia eval for +-*/ operators."
function eval_ops((l,opr))
    for (op,val) in opr
        l = eval( Expr(:call, Symbol(op), l, val))
    end
    l::Rational{Int}
end

using TextParse
@with_names begin
    number = map(Rational{Int}, TextParse.Numeric(Int))
    factor = Either(number)  # or expression in parens, see push! below
    divMul = map(eval_ops,
                 Sequence( factor, Repeat( CharIn("*/"), factor ) ) )
    addSub = map(eval_ops,
		 divMul * Repeat( CharIn("+-") * divMul ) )
    parens = Sequence(2, "(",addSub,")" )
    push!(factor, parens)
    expr = (addSub * AtEnd())[1]
end;
parse(log_names(expr), "1/((1+2)*4+3*(5*2))")

```
1//42

[Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))
