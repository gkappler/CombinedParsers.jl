# Getting Started

## Installation
Install with
```julia
] add https://github.com/gkappler/CombinedParsers.jl
```

## Writing Parsers
CombinedParsers provides constructors to combine parsers and transform (sub-)parsings arbitrarily with julia syntax.

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
```
1//42
```

[Is every rational answer ultimately the inverse of a universal question in life?](https://en.wikipedia.org/wiki/Phrases_from_The_Hitchhiker%27s_Guide_to_the_Galaxy#Answer_to_the_Ultimate_Question_of_Life,_the_Universe,_and_Everything_(42))

Combinator constructors are discussed in the [user manual](user.html).

## Regular expression syntax
CombinedParsers provides the ```@re_str``` macro as a plug-in replacement for the base Julia ```@r_str``` macro.

Base Julia PCRE regular expressions:
```julia
pattern = r"reg(ular )?ex(?<a>p(ression)?)?\??"i
mr = match(pattern,"regexp")
```
```
RegexMatch("regexp", 1=nothing, 2="p", 3=nothing)
```


CombinedParsers.Regexp regular expression:
```julia
using CombinedParsers
using CombinedParsers.Regexp
pattern = re"reg(ular )?ex(?<a>p(ression)?)?\??"i
mre = match(pattern,"regexp")
```
```
ParseMatch("Regular Expression?", 1="ular ", 2="pression", 3="ression")
```

The ParseMatch type has `getproperty` and `getindex` methods for handling like `RegexMatch`.
```julia
mre.match == mr.match
mre.captures == mr.captures
mre[2] == mr[2]
mre[:a] == mr[:a]
```

The `@re_str` supports the following PCRE features
- [X] fundamentals: sequences, alternations, repetitions optional, matches
    (`*`,`+`,`{n}`, `{min,}`, `{min,max}`, `?`)
- [X] escaped characters and generic character types
- [X] character ranges (`[]`)
- [X] non-capturing groups,
- [X] capturing groups, backreferences, subroutines (all by index, relative index and name)
- [X] atomic groups
- [X] lazy repetitions
- [X] conditional expressions
- [X] internal and pattern options setting
- [X] simple assertions (`\A`, `\z`, `\Z`, `\b`, `\B`, `^`, `$`), 
- [X] lookaheads and lookbehinds
- [X] comments

CombinedParsers.jl is tested against the PCRE C library testset.

PCRE functionality that is currently not supported:
- [ ] capture groups in lookbehinds.
- [ ] ACCEPT, SKIP, COMMIT, THEN, PRUNE, \K


## Parsing 

```match``` searches for the first match of the Regex in the String and return a `RegexMatch`/`Parsematch` object containing the match and captures, or nothing if the match failed.
```Base.parse``` methods parse a String into a Julia type.
A CombinedParser `p` will parse into an instance of `result_type(p)`.
For parsers defined with the `@re_str` the `result_type`s are nested Tuples and Vectors of SubString, Chars and Missing.


```julia
p = re"(a)*bc?"
parse(p,"aaab")
```
```
(['a','a','a'],'b',missing)
```


## Iterating
If a parsing is not uniquely defined different parsings can be lazily iterated.
```julia
# Backtracking and listing all matches
collect(parse_all(re"^(a|ab|b)+$","abab"))
```
```
4-element Array{Tuple{AtStart,Array{Union{Char, Tuple{Char,Char}},1},AtEnd},1}:
 (^, ['a', 'b', 'a', 'b'], $)    
 (^, ['a', 'b', ('a', 'b')], $)  
 (^, [('a', 'b'), 'a', 'b'], $)  
 (^, [('a', 'b'), ('a', 'b')], $)
```



## Transformations
Transform the result of a parsing with `map`.
```julia
parse(map(length,re"(ab)*"),"abababab") == 4
```
`map` uses julia type inference to infer the `result_type` automatically.
A supertype `T >: result_type(map(f,p))` can be set as `result_type` with `map(f, T, p)`.

Calling `map(::Integer,::AbstractParser)` or `getindex(::AbstractParser)` creates a transforming parser selecting from the result of the parsing.
```julia
parse(map(2,re"abc"),"abc") == 'b'
parse(re"abc"[2],"abc") == 'b'
```
