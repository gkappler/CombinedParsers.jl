# CombinedParsers in pure Julia


Parsing is a recurring annoyance when preparing data for computing...



### Regular languages
- regular expressions: syntax of the standard parsing language
- parser generators: compiled grammars and state machines

separated grammar and transformation (start, stop hooks)


### Parser Combinators
- syntax and transformation in a functional paradigm
- Julia-compiler: parametric types and multiple dispatch

type-safe, integrated grammar and transformation



#### Parsing the audience
- regular expressions?
- grammar based parser generators (ANTLR,...)?
- hand-coded parsers?

Did you experience some despair?

## CombinedParsers.jl
I needed a flexible fast parsing machinery, and wrote a optimizable parser combinator from scratch to learn Julia parametric types and multiple dispatch.

Design goals:
- easy syntax
- transformation @ grammar, type safety and validation
- iterating possible parsings
- fast

Install with
```
] add https://github.com/gkappler/CombinedParsers.jl
```

Following performance recommendations of Julia 1.0
- small Union{Nothing,T} instead of Nullable{T}
- iterable interface
- parametric immutable matcher types for compiler optimizations with generated functions

Recurring point of this presentation: recursion and regular languages:

#### The presentation outlines a CombinedParser for PCRE syntax, which generates julia-compiled parsers for regular expressions equivalent with the PCRE C library.

You will 
- refresh your regular expression knowledge while you
- learn the matching, transformation and logging API of CombinedParsers.jl.


## Regular expressions: ```match``` Strings

A regular expression is a String, most characters define a matcher for a Char identical to that character (```r"a"``` matches ```"a"```).  Meta-characters like `?` for optional matching need to be escaped with ```\```. Parenthesis define subexpressions.

CombinedParsers provides ```@re_str``` macro as a plug-in replacement for the base Julia ```@r_str``` macro.



```julia
# Base Julia PCRE regular expressions
match(r"reg(ular )?ex(p(ression)?)?\??"i,"regexp")
```




    RegexMatch("regexp", 1=nothing, 2="p", 3=nothing)




```julia
using CombinedParsers
using CombinedParsers.Regexp
match(re"reg(ular )?ex(p(ression)?)?\??"i,"Regular Expression?")
```




    ParseMatch("Regular Expression?", 1="ular ", 2="pression", 3="ression")



## Parsing and Transformations
```match``` searches for the first match of the Regex in the String and return a `RegexMatch`/`Parsematch` object containing the match and captures, or nothing if the match failed.
This is very limiting.

CombinedParser also can ```parse``` a string, resulting in an instance of ```result_type(p)```, by default nested Tuple and Vector of Chars and Missing.



```julia
p = re"reg(ular )?ex(p(ression)?)?\??"i
@show result_type(p)
parse(p,"Regexp")
```

    result_type(p) = Tuple{Char,Char,Char,Union{Missing, NTuple{5,Char}},Char,Char,Union{Missing, Tuple{Char,Union{Missing, NTuple{7,Char}}}},Union{Missing, Char}}





    ('R', 'e', 'g', missing, 'e', 'x', ('p', missing), missing)



## Regular expressions: Either, Characters, and logging

Lets build the major scaffold of the parser for regular expressions from scratch now!


```julia
import CombinedParsers.Regexp: meta_chars

char_matcher =  Either(
    
    with_name("character", 
        CharNotIn([ c for c in meta_chars])),
    
    with_name("escaped meta", 
        Sequence(2, # emit unescaped
            '\\', CharIn(meta_chars)))
    
)
```








```julia
@show pcre = Regex(regex_string(char))

@show match(pcre,"a")
@show match(pcre,"\\[")

@show match(log_names(char),"a")
@show match(char,"\\["; log=true)
```

### Transforming a Parsing
The Julia ```map``` method for `CombinedParsers.AbstractParser` creates a transforming parser.
The parser for escaped and not escaped regular expression characters ```char``` can be transformed to emit not the `Char` but an `CharIn<:AbstractParser` matching the unescaped character.


```julia
@show result_type(char_matcher)

@show parsed_obracket = parse(char_matcher,"\\[")
```

    result_type(char_matcher) = Char
    parsed_obracket = parse(char_matcher, "\\[") = '['





    '[': ASCII/Unicode U+005b (category Ps: Punctuation, open)




```julia
char_parser_parser = map(CharIn,char_matcher)
@show result_type(char_parser_parser)

@show parsed_obracket_parser = parse(char_parser_parser,"\\[")

parse(parsed_obracket_parser,"[")
```

    result_type(char_parser_parser) = CharIn
    parsed_obracket_parser = parse(char_parser_parser, "\\[") = re"\["





    '[': ASCII/Unicode U+005b (category Ps: Punctuation, open)



### Repeat, Transformation, and Sequence
A basic feature of regular expression is whether a repeatable pattern (e.g. a char) is optional `?`, or should be repeated at least once `+`, any times `*`, or `{min,max}` times.

The CombinedParser to parse the `(min, max)` tuple from PCRE syntax:


```julia
import CombinedParsers.Regexp: integer

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC17
@with_names repetition = Either(
    "?" => (0,1), #  `Pair` syntax to map a match to a constant
    
    "+" => (1,typemax(Int)),
    "*" => (0,typemax(Int)),
    Sequence(
        "{", 
        
        integer,
        Optional( Sequence(2,",",
            Optional(integer, default=typemax(Int)))),
        
        "}") do v  # the do syntax implicitly calling map on the sequence
            if v[3] isa Missing
                (v[2],v[2])
            else
                (v[2],v[3])
            end::Tuple{Int,Int}
    end
)

@show result_type(repetition)
@show parse(repetition, "?")
@show parse(repetition, "{1}");
```

    result_type(repetition) = Tuple{Int64,Int64}
    parse(repetition, "?") = (0, 1)
    parse(repetition, "{1}") = (1, 1)


### Repeatable patterns, Optional, Lazy and Atomic
The character matcher is the simplest repeatable pattern.

The CombinedParser to parse a repeated matcher from PCRE syntax


```julia
repeatable = Either{AbstractParser}(
    Any[char_parser_parser])
## we will add groups to repeatable later

@with_names quantified=Sequence(
        repeatable,
        Optional(repetition, default=(1,1)),
        Optional(map(v->convert(Char,v),CharIn('+','?'))) # possessive quantifier
    ) do v
        pat = v[1]
        result = if v[2]==(1,1)
            pat
        elseif v[2]==(0,1)
            Optional(pat)
        else
            Repeat(pat,v[2])
        end
        if v[3] === missing
            result
        elseif v[3]=='+'
            Atomic(result)
        elseif v[3]=='?'
            Lazy(result)
        else
            result
        end
    end


parse(log_names(quantified), raw"\++?")
```



### Sequences and Alternations
Regular expression patterns can be written in sequence, delimited by `|` for matching either one of several alternatives.


```julia
@with_names begin
    sequence = map( v->sSequence(v...),
        Repeat( quantified ))

    alternation = Sequence(
        sequence, 
        Repeat(Sequence(2, '|',sequence))) do v
            sEither(v[1],v[2]...)::AbstractParser
        end
end;

```

### Captures
Parentheses allow groupings that are repeatable.


```julia
import CombinedParsers.Regexp: name

@with_names begin
    noncapture_group=Sequence( 2, "(?:",alternation,")")

    capture_group=Sequence("(",alternation,")") do v
                Capture(v[2])
    end;
end
push!(repeatable,capture_group);
push!(repeatable,noncapture_group);
```


```julia
alternation
## is a parser
```








```julia
# the alternation parser transforms a regex string into a parser

@show result_type(alternation)

ab_parser = parse(log_names(alternation),"(?:a|b)+")
```






```julia
## the ab_parser
match(ab_parser,"aab")

```




    ParseMatch("aab")




```julia
# We quickly wrote a parser for regular expressions that generates a PCRE equivalent CombinedParser.

@show parse(parse(alternation,raw"reg(ular )?ex(p(ression)?)?\??"), "regex")
parse(re"reg(ular )?ex(p(ression)?)?\??", "regex")
```

    parse(parse(alternation, #= In[32]:3 =# @raw_str("reg(ular )?ex(p(ression)?)?\\??")), "regex") = ('r', 'e', 'g', missing, 'e', 'x', missing, missing)





    ('r', 'e', 'g', missing, 'e', 'x', missing, missing)



### Looking around




```julia

@with_names lookahead=Sequence(
    2,
    "(",
    Either(Sequence(
            v -> look_ahead(true,v[2]),
            Either("?=","*positive_lookahead:","*pla:"),
            alternation),
        Sequence(
            v -> look_ahead(false,v[2]),
            Either("?!","*negative_lookahead:","*nla:"),
            alternation)),
    ")");
push_first!(repeatable,lookahead);



@with_names lookbehind=Sequence(
    2,
    "(",
    Either(Sequence(v -> look_behind(true,v[2]),
            Either("?<=","*positive_lookbehind:","*plb:"),alternation),
        Sequence(v -> look_behind(false,v[2]),
            Either("?<!","*negative_lookbehind:","*nlb:"),alternation)),
    ")");
push_first!(repeatable,lookbehind);


```


```julia
import CombinedParsers.Regexp: bracket
push!(repeatable,bracket);
bracket
```

## Iterating matches



```julia
# Backtracking and listing all matches
parse_all(re"^(a|ab|b)+$","abab")
```




    4-element Array{Any,1}:
     (^, Union{Char, Tuple{Char,Char}}['a', 'b', 'a', 'b'], $)    
     (^, Union{Char, Tuple{Char,Char}}['a', 'b', ('a', 'b')], $)  
     (^, Union{Char, Tuple{Char,Char}}[('a', 'b'), 'a', 'b'], $)  
     (^, Union{Char, Tuple{Char,Char}}[('a', 'b'), ('a', 'b')], $)



### Unit Tests
The package was tested with unit tests of the PCRE C library.

Some tests did not pass: 3130 passed, 123 failed, 0 errored, 0 broken.

### Limitations
- alpha release
- no capture groups in look-behind.
- limited PCRE functionality ACCEPT, SKIP, COMMIT, THEN, PRUNE, \K



### Performance
Preliminary tests were encouraging, in part better than PCRE, optimization is next step.


### Next Steps
- Performance optimizations
    - parsing memoization
    - backtracking optimization for special cases with multiple dispatch
- Documentation
- Publishing packages for parsing wikitext and orgmode markup
- error backtracking, stepping & debugging
- test coverage



```julia
using BenchmarkTools

# julia PCRE regex support
pc = re"a*"
re = r"a*"
@btime match(pc,"a"^3);
@btime _iterate(pc,"a"^3);
@btime match(re,"a"^3);
```

      1.049 μs (10 allocations: 480 bytes)
      283.928 ns (5 allocations: 272 bytes)
      146.067 ns (5 allocations: 272 bytes)


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

## CombinedParsers.jl
- more expressive than regular expressions
- provides compiled regular expression parsers in pure julia
- arbitrary transformations with convenient syntax
     - parsing and transformation are decoupled for optimized performance.
     - Julia type inference for result_types
- logging helps debugging complex parsers.
- higher-order parsers depending on the parsing state
- iterate interface to lazily generate all valid parsings
- TextParse interface to include ParserIterators e.g. in CSV.jl.
- generalize from strings to parsing any iterator type
- MIT licenced, https://github.com/gkappler/CombinedParsers.jl



### Collaborations are very welcome!

### Evaluating Parser for arithmetic expressions


```julia

function eval_(v)
    x = v[1]
    for (op,val) in v[2]
        x = if op=='+'
            x + val
        elseif op=='-'
            x - val
        elseif op=='*'
            x * val
        elseif op=='/'
            x // val
        end
    end
    x::Rational
end


@with_names begin
    number = map(Rational,integer)
    factor = Either{Rational}( Any[number] )
    divMul = map( eval_, 
        Sequence( factor, Repeat( CharIn("*/"), factor ) ) )
    addSub = map( eval_,
        Sequence( divMul, Repeat( CharIn("+-"), divMul ) ) )
    parens = Sequence(2, "(",addSub,")" )
    push!(factor, parens)
end;

expr = Sequence(1, addSub, AtEnd() )
parse(expr, "1/((1+2)*4+3*(5*2))")

```




    1//42




```julia
Ultimately, is every rational answer the inverse of a universal question in life?
```
