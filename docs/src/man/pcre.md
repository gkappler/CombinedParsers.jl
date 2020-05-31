# Parsing Regular Expressions and constructing an equivalent [`CombinedParser`](@ref)
This guide demonstrates constructing a recursive parser with the 
**[`push!`](@ref) to [`Either`](@ref)** technique by means of a similified parser for regular expressions. 

This page outlines parts of the `CombinedParser` used in the `re_str` macro.
`CombinedParsers.jl` is tested and benchmarked against the PCRE C library testset, see [compliance report](pcre-compliance.md).

## Characters and Escaped Characters
The simplest regular expression pattern is a character matcher.
```@example session
using CombinedParsers
using CombinedParsers.Regexp
import CombinedParsers.Regexp: meta_chars

char_matcher =  Either(
    
    with_name("character", 
        CharNotIn([ c for c in meta_chars])),
    
    with_name("escaped meta", 
        Sequence(2, # emit unescaped
            '\\', CharIn(meta_chars)))
    
)
```

The `char_matcher` unescapes regex `meta_chars`:
```@repl session
parse(char_matcher,"a"; log=true)
parse(char_matcher,"\\["; log=true)
```

## Transforming a Parsing
The parser for escaped and not escaped regular expression characters `char_matcher` emits `Char`s
```@repl session
result_type(char_matcher)
```

The Julia [`map`](@ref) method for [`CombinedParser`](@ref) creates a [`Transformation`](@ref) parser.
The `char_matcher` is transformed to emit an [`CharIn`](@ref) matching the unescaped character:
```@repl session
char_matcher_parser = map(CharIn,char_matcher);
result_type(char_matcher_parser)
```

A parser for a single character is constructed that can then be used to `parse` that specific `Char`
```@repl session
p = parse(char_matcher_parser,"\\[")
parse(p,"[")
```
## Repeated patterns

## Number of Repetitions
A basic feature of regular expression is whether a repeatable pattern (e.g. a char) is optional `?`, or should be repeated at least once `+`, any times `*`, or `{min,max}` times.

The `Pair` syntax abbreviates [`map`](@ref)ping a match to a constant value.
The `do` syntax is supported for all CombinedParsers constructors to implicitly call [`map`](@ref).
```@example session
import CombinedParsers.Regexp: integer

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC17
@with_names repetition = Either(
    "?" => (0,1), # == map(v->(0,1), parser("?"))
    "+" => (1,typemax(Int)),
    "*" => (0,typemax(Int)),
    Sequence(
        "{", 
        integer,
        Optional( Sequence(2,",",
            Optional(integer, default=typemax(Int)))),
        "}") do v
            if v[3] isa Missing
                (v[2],v[2])
            else
                (v[2],v[3])
            end::Tuple{Int,Int}
    end
)
```

`repetition` parses PCRE repetition syntax into a `(min, max)`:
```@repl session
parse(repetition, "?")
parse(repetition, "*")
parse(repetition, "+")
parse(repetition, "{2}")
parse(repetition, "{5,}")
```

## [`Either`](@ref)
Regular expressions can repeat character matchers and other sub-patterns when appending the `repetition` suffix.
```@example session
repeatable = Either{CombinedParser}(char_matcher_parser)
nothing # hide
```
We will add capture groups and other sub-patterns to `repeatable` later.




## Repeatable patterns, [`Optional`](@ref), [`Lazy`](@ref) and [`Atomic`](@ref)
The CombinedParser to parse a repeated pattern from PCRE syntax
```@example session
@with_names quantified=Sequence(
        repeatable,
        Optional(repetition, default=(1,1)),
        Optional(map(v->convert(Char,v),with_name("possessive_quantifier",CharIn('+','?'))))
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
nothing # hide
```

`quantified` parses PCRE repetition syntax:
```@repl session
p = parse(quantified, raw"\++?", log=true)
parse(p, "+++")
```



## [`Sequence`](@ref)s and Alternations
Regular expression patterns can be written in sequence, delimited by `|` for matching either one of several alternatives.


```@example session
@with_names begin
    sequence = map( v->sSequence(v...),
        Repeat( quantified ))

    alternation = Sequence(
        sequence, 
        Repeat(Sequence(2, '|',sequence))) do v
            sEither(v[1],v[2]...)::CombinedParser
        end
end
nothing # hide
```

The alternation parser transforms a regex string into a parser
```@repl session
result_type(alternation)
p = parse(alternation, "ab|c", log=(:sequence, :alternation))
parse(p, "c")
```

## [`Capture`](@ref)s
Parentheses allow groupings that are repeatable.
```@example session
import CombinedParsers.Regexp: name

@with_names begin
    noncapture_group=Sequence( 2, "(?:",alternation,")")

    capture_group=Sequence("(",alternation,")") do v
                Capture(v[2])
    end;
end
nothing # hide
```

The [`push!`](@ref) to [`Either`](@ref) technique allows for the construction of recursive parsers.
```@example session
push!(repeatable,capture_group);
push!(repeatable,noncapture_group);
nothing # hide
```


```@repl session
p = parse(alternation, "(ab|c)+", log=(:capture_group, repeatable))
parse(p, "cabcabab")
```


## [`Lookahead`](@ref) and [`Lookbehind`](@ref)


```@example session
@with_names lookahead=Sequence(
    2,
    "(",
    Either(Sequence(
            v -> LookAhead(true,v[2]),
            Either("?=","*positive_lookahead:","*pla:"),
            alternation),
        Sequence(
            v -> LookAhead(false,v[2]),
            Either("?!","*negative_lookahead:","*nla:"),
            alternation)),
    ")");
@with_names lookbehind=Sequence(
    2,
    "(",
    Either(Sequence(v -> LookBehind(true,v[2]),
            Either("?<=","*positive_lookbehind:","*plb:"),alternation),
        Sequence(v -> LookBehind(false,v[2]),
            Either("?<!","*negative_lookbehind:","*nlb:"),alternation)),
    ")");

pushfirst!(repeatable,lookahead);
pushfirst!(repeatable,lookbehind);

nothing # hide
```

## Regular Expression Brackets
```@example session
import CombinedParsers.Regexp: bracket
push!(repeatable,bracket);
bracket
```


