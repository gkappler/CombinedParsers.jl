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

