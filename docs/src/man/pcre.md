# Regular expressions: Either, Characters, and logging


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

# Transforming a Parsing
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



# Repeat, Transformation, and Sequence
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


# Repeatable patterns, Optional, Lazy and Atomic
The character matcher is the simplest repeatable pattern.

The CombinedParser to parse a repeated matcher from PCRE syntax


```julia
repeatable = Either{AbstractParser}(
    Any[char_parser_parser])
# we will add groups to repeatable later

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



# Sequences and Alternations
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

# Captures
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
# is a parser
```








```julia
# the alternation parser transforms a regex string into a parser

@show result_type(alternation)

ab_parser = parse(log_names(alternation),"(?:a|b)+")
```






```julia
# the ab_parser
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



# Looking around




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

