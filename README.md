
# CombinedParsers in pure Julia
<!-- [![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://gkappler.github.io/CombinedParsers.jl/stable) -->
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://gkappler.github.io/CombinedParsers.jl/dev)
[![Build Status](https://travis-ci.org/gkappler/CombinedParsers.jl.svg?branch=master)](https://travis-ci.com/github/gkappler/CombinedParsers.jl)
[![Codecov](https://codecov.io/gh/gkappler/CombinedParsers.jl/branch/master/graph/badge.svg)](https://codecov.io/gh/gkappler/CombinedParsers.jl)

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



### Limitations
This is an alpha release.
Collaborations are very welcome!

CombinedParsers.jl was tested against the PCRE C library testset.
Some tests did not pass: 3130 passed, 123 failed, 0 errored, 0 broken.
Failing tests are capture related.


#### Benchmarks
Performance is acceptable although optimizations are currently disabled/incomplete.
```julia
using BenchmarkTools

## PCRE
re = r"a*"
@btime match(re,"a"^3);
#      146.067 ns (5 allocations: 272 bytes)


## CombinedParsers
pc = re"a*"
@btime match(pc,"a"^3);
#      1.049 μs (10 allocations: 480 bytes)
@btime _iterate(pc,"a"^3);
#      283.928 ns (5 allocations: 272 bytes)
```

Preliminary performance tests were even more encouraging, in trivial example above 10x faster than PCRE.


### Next Steps
- [-] Documentation
- [ ] test coverage
- [ ] Performance optimizations
    - generated functions
    - parsing memoization
    - backtracking optimization with multiple dispatch on parser and state type.
- [ ] [![Code Style: Blue](https://img.shields.io/badge/code%20style-blue-4495d1.svg)](https://github.com/invenia/BlueStyle)
- [ ] Publishing packages for parsing wikitext and orgmode markup
- [ ] error backtracking, stepping & debugging


<!-- Implementation Notes: -->
<!-- - parametric immutable matcher types for compiler optimizations with generated functions (currently inactive) -->
<!-- - small Union{Nothing,T} instead of Nullable{T} -->

<!-- CombinedParsers is a package by Gregor Kappler. If you use and appreciate CombinedParsers.jl, please support development at patreon. -->

## Getting Started

Install with
```julia
] add https://github.com/gkappler/CombinedParsers.jl
```

## Writing Parsers
### Regular expression syntax
CombinedParsers provides the ```@re_str``` macro as a plug-in replacement for the base Julia ```@r_str``` macro.

```julia
# Base Julia PCRE regular expressions
mr = match(r"reg(ular )?ex(?<a>p(ression)?)?\??"i,"regexp")
```
RegexMatch("regexp", 1=nothing, 2="p", 3=nothing)

```julia
using CombinedParsers
using CombinedParsers.Regexp
mre = match(re"reg(ular )?ex(?<a>p(ression)?)?\??"i,"regexp")
```
ParseMatch("Regular Expression?", 1="ular ", 2="pression", 3="ression")

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


### Parsing

```match``` searches for the first match of the Regex in the String and return a `RegexMatch`/`Parsematch` object containing the match and captures, or nothing if the match failed.
```Base.parse``` methods parse a String into a Julia type.
A CombinedParser `p` will parse into an instance of `result_type(p)`.
For parsers defined with the `@re_str` the `result_type`s are nested Tuples and Vectors of SubString, Chars and Missing.


```julia
p = re"(a)*bc?"
parse(p,"aaab")
```
(['a','a','a'],'b',missing)



## Iterating matches
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



### Transformations
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

### Basics
The simplest parser matches a `String` or `Char` iterator.
```julia
parse_a = parser("aa")

parse(parse_a,"aa")
# "aa"

parse(parse_a,"ab")
# ArgumentError: expected re"a" in "ab" at index 2 (todo!)
```



#### Character Sets
```julia
parse(CharIn('a':'z'),"c") =='c'
parse(CharIn(isuppercase),"A") =='A'
parse(CharNotIn('a':'z'),"A") =='A'
parse(CharNotIn(isuppercase),"c") =='c'
```

#### Sequence
Several parsers can be combined with the `Sequence` constructor and the `*` operator.
The `result_type` of a `Sequence` is the Tuple of the `result_type`s of its parts.
```julia
parse(Sequence(CharIn(isuppercase) * CharIn(islowercase)),"Ab") == ('A','b')
parse(CharIn(isuppercase) * CharIn(islowercase),"Ab") == ('A','b')
```

`getindex` on a sequence creates a transforming parser selecting from the result of the parsing.

Sequence keyword argument constructors transform the parsing into a named tuple.
```julia
parse(Sequence(first = CharIn(isuppercase), second = CharIn(islowercase)),"Ab") == 
	(first='A',second='b')
```

If some Sequence arguments are <:`Pair{Symbol}`, only those are retained in a NamedTuple.
```julia
parse(Sequence(CharIn(isuppercase), :second => CharIn(islowercase)),"Ab") == 
	(second='b',)
```



#### Either
The `|` operator and constructor `Either` try matching the provided parsers in order, accepting the first match, and fails if all parsers fail.

```julia
parse(("a"|"ab"),"ab")
# "a"
```

Feedback inquiry:

#### Assertions
Parsers that do not advance the parsing position can be used to assert conditions during parsing.
##### AtStart() and AtEnd()
The `AtStart()` only succeeds if at the start of the input, and similarly the `AtEnd()` succeeds only at the end of the input.
By default, `parse` does not need to consume the full input but succeeds with the first match.
With `AtEnd()` the parser can be forced to consume the full input or fail otherwise.
```julia
parse(("a"|"ab")*AtEnd(),"ab")
# "ab"
```

##### Looking around
A `Lookaround` parser wraps a parser `p`, succeeds if `p` matches without advancing the position, and fails if `p` fails.


The `@re_str` macro has a regex parser for lookahead and lookbehind expressions (simplified):
```julia
@with_names lookahead=Sequence(
    2,
    "(?",
    Either(Sequence(v->PositiveLookahead(v[2]), "=", alternation),
           Sequence(v->NegativeLookahead(v[2]), "!", alternation)),
           Sequence(v->PositiveLookbehind(v[2]), "<=", alternation)),
           Sequence(v->NegativeLookbehind(v[2]), "<!", alternation)),
    ")");
```

#### Repeat
The `Repeat(p)` constructor creates a new parser repeating its argument zero or more times, and by default transforming to
`Vector{result_type(p)}`.
Repeating a specified number of times can be achieved with `Repeat(p,min=1,max=2)`, or `Repeat(1,p)` or `Repeat(1,2,p)`.
Equivalently the `^` operator can be used similar as for String, e.g. `p^2`, 
and like in regular expressions `p^(+)`, `p^(*)`.

```julia
parse(join(parser('a')^(*)," "),"a a") == 
	['a','a']
```

#### Optional
Similar to Repeat, `Optional(p)` creates a parser, repeating 0 or 1 times. 
The `result_type(Optional(p, default=d))` is `promote_type` (or `Union` type is type promotion results in Any).

```julia
option = Optional('a') * join(Repeat('b'),"-")
```

Feedback appreciated:

```julia
option = ( CharIn('a':'z') | missing ) * join(Repeat('b'),"-")
```


#### Lazy repetitions and optional parsers
Repetition and optional parsers are greedy by default, and can be switched to lazy matching by wrapping in `Lazy(Repeat(p))`.




#### Atomic groups
Backtracking of a parser `p` can be prevented by wrapping in `Atomic(Repeat(p))`.
An atomic parser fails if `p` fails or if the first successfull parsing with `p` leads to a failing later in the parsing process.


```julia
parse(Either("a","ab","ac")*AtEnd(),"ab") == ("ab", AtEnd())
parse(Atomic(Either("a","ab","ac"))*AtEnd(),"ab") ## fails
```



## Acknowledgements

The work was inspired by Scala FastParse package and the Julia parsing packages
##### Parsers.jl
For date and primitive types.
##### TextParse
- used in CSV.jl
- uses Nullables
- CombinedParsers.AbstractParser <: TextParse.AbstractToken

##### Automa.jl
- grammar based state machine compiler
- no UTF8 support

##### ParserCombinator.jl
- old source base (pre 2016, fixed for Julia 1.0 in 2018)
    - using Nullables
- no iterator API
- performance 
    - mutable matcher types
    - matcher types not parametric






