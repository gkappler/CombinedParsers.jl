"""
# (Extended) Backus-Naur Form [`CombinedParser`](@ref)
Defining a EBNF parser can be done with the [`ebnf`](@ref) string macro.
[`substitute`](@ref) is used to combine parts of the definition.

!!! warn
    Left recursion is not yet supported (will lead to a stack overflow).
"""
module BNF
using CombinedParsers

# todo 
# paddedSequence(x...) = Sequence(trim.(x)...)
trimhv(x; whitespace=Atomic(Repeat(CharIn(horizontal_space_char,vertical_space_char)))) =
    CombinedParsers.trim(x; whitespace=whitespace)

@generated function separatedSequence(x...; whitespace=Atomic(Repeat(CharIn(horizontal_space_char,vertical_space_char))))
    f = Expr(Symbol("->"), :v, Expr(:tuple, Any[Expr(:ref, :v, i) for i in 2*1:length(x)]))
    a = Iterators.flatten([ [:i, QuoteNode(whitespace)] for i in x ])
    quote
        (
        # map($f,
            Sequence(whitespace, $a...))
    end
end

function separatedTriple(f::Function, open,body,close; whitespace=Atomic(Repeat(CharIn(horizontal_space_char,vertical_space_char))))
    Sequence(whitespace, open, whitespace, body, whitespace, close, whitespace) do v
        f(v[4])
    end
end


letter = CharIn('A':'Z','a':'z')
decimal_digit = CharIn('0':'9')
"""
Supports BNF and EBNF variants
```jldocs
julia> CombinedParsers.BNF.concatenate_symbol
```
"""
concatenate_symbol = ',' # Either(',', Always())

"""
Supports BNF and EBNF variants
```jldocs
julia> CombinedParsers.BNF.defining_symbol
```
"""
defining_symbol = '=' # !Either('=', "::=", ":=")
definition_separator_symbol = CharIn("|/!")
start_comment_symbol = "(*"
start_group_symbol = "("
end_comment_symbol = "*)"
end_group_symbol = ")"
end_option_symbol = Either(']', "/)")
end_repeat_symbol = Either('}', ":)")

except_symbol = '-'
first_quote_symbol = '''
repetition_symbol = '*'
second_quote_symbol = '"'
special_sequence_symbol = '?'

start_option_symbol = Either('[', "(/")
start_repeat_symbol = Either('{', "(:")
terminator_symbol = CharIn(";.")

other_character = CharIn(" :+_%@&#\$<>\\^`~") # is it `?
space_character = ' '
horizontal_tabulation_character = horizontal_space_char#  "\t" #^-?
new_line = '\n' #?
vertical_tabulation_character = vertical_space_char# Never() #?
form_feed = '\f' # Never() #?

terminal_character = !Atomic(Either(
    letter,
    decimal_digit,
    concatenate_symbol,
    defining_symbol,
    definition_separator_symbol,
    end_comment_symbol,
    end_group_symbol,
    end_option_symbol,
    end_repeat_symbol,
    except_symbol,
    first_quote_symbol,
    repetition_symbol,
    second_quote_symbol,
    special_sequence_symbol,
    start_comment_symbol,
    start_group_symbol,
    start_option_symbol,
    start_repeat_symbol,
    terminator_symbol,
    other_character
))

first_terminal_character =
    Sequence(NegativeLookahead(first_quote_symbol), terminal_character)

second_terminal_character =
    Sequence(NegativeLookahead(second_quote_symbol), terminal_character)

terminal_string =
    Either(Sequence(2,first_quote_symbol,
                    with_name(:terminal_string, !!Repeat1(first_terminal_character)),
                    first_quote_symbol),
           Sequence(2,second_quote_symbol,
                    with_name(:terminal_string, !!Repeat1(second_terminal_character)),
                    second_quote_symbol))

gap_separator = CharIn(space_character, horizontal_tabulation_character,
                       new_line, vertical_tabulation_character, form_feed)

_integer = CombinedParsers.Numeric(Int) #!Repeat1(decimal_digit)

meta_identifier_character = CharIn(letter, decimal_digit, "-_ ") # optimize!
@with_names meta_identifier = map(Symbol,Sequence(1, 
    !join(!Sequence(letter, Lazy(Repeat(meta_identifier_character))), gap_separator),
    NegativeLookahead(meta_identifier_character .& CharNotIn(' '))
))

special_sequence_character =
    Sequence(NegativeLookahead(special_sequence_symbol), terminal_character)
@with_names special_sequence =
    Sequence(2, special_sequence_symbol, trimhv(!!Lazy(Repeat(special_sequence_character))), special_sequence_symbol)

commentless_symbol = Either(
    Sequence(
        NegativeLookahead(Either(
            letter,
            decimal_digit,
            first_quote_symbol,
            second_quote_symbol,
            start_comment_symbol,
            end_comment_symbol,
            special_sequence_symbol,
            other_character)),
        terminal_character
    ),
    meta_identifier,
    _integer,
    terminal_string,
    special_sequence
)

comment_symbol = Either{Any}(Any[
    other_character,
    commentless_symbol
])
@with_names bracket_textual_comment = Sequence(start_comment_symbol, !Repeat(comment_symbol), end_comment_symbol)
pushfirst!(comment_symbol, bracket_textual_comment)

empty_sequence = parser(Always() => Always())

@with_names syntactic_primary = Either{CombinedParser}(
    Any[map(n->substitute(n), meta_identifier),
        map(parser,terminal_string),
        map(s->Never(),special_sequence),
        empty_sequence])

import ..CombinedParsers: Repeat_max
@with_names syntactic_factor = Sequence(
    Either(
        Sequence(v->v[1]:v[3], _integer, trimhv(repetition_symbol), _integer),
        Sequence(v->v[1]:v[1], _integer, trimhv(repetition_symbol)),
        Always() => 1:1),
    trimhv(syntactic_primary)) do v 
        v[1] == 1:1 ? v[2] : Repeat(v[1], v[2])
    end

syntactic_exception = syntactic_factor
syntactic_term = Sequence(1, syntactic_factor,
                          # todo: handle exceptions
                          Optional(Sequence(except_symbol, syntactic_exception)))

@with_names single_definition = map(p -> sSequence(p...)::CombinedParser,
                                    join(syntactic_term, trimhv(concatenate_symbol)))

definitions_list = map(p -> sEither(p...)::CombinedParser,
                       join(single_definition, trimhv(definition_separator_symbol)))

@syntax for optional_sequence in syntactic_primary
    separatedTriple(start_option_symbol, definitions_list, end_option_symbol) do v
        Optional(v)
    end
end
@syntax for repeated_sequence in syntactic_primary
    separatedTriple(start_repeat_symbol, definitions_list, end_repeat_symbol) do v
        Repeat(v)
    end
end
@syntax for grouped_sequence in syntactic_primary
    separatedTriple(identity, start_group_symbol, definitions_list, end_group_symbol)
end

@syntax syntax_rule = Sequence(meta_identifier, trimhv(defining_symbol), definitions_list, trimhv(terminator_symbol)) do v
    with_name(v[1], v[3])
end;

export @ebnf_str, ebnf
@syntax ebnf = map(v->(substitute(Either(reverse(v)...))),Repeat1(syntax_rule));


"""
    ebnf

Parser to create a `CombinedParser` from EBNF syntax:
```jldocs
julia> p = ebnf\"\"\"
       digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
       digit                = "0" | digit excluding zero ;
       natural number       = digit excluding zero, { digit } ;
       integer              = "0" | [ "-" ], natural number ;
       \"\"\"
|🗄 Either
├─ |🗄 Either |> with_name(:integer)
│  ├─ 0 
│  └─ 🗄 Sequence
│     ├─ \\-? |
│     └─ 🗄 Sequence |> with_name(:natural number) # branches hidden
├─ 🗄 Sequence |> with_name(:natural number)
│  ├─ |🗄 Either |> with_name(:digit excluding zero) # branches hidden
│  └─ |🗄* Either |> with_name(:digit) |> Repeat
│     ├─ 0 
│     └─ |🗄 Either |> with_name(:digit excluding zero) # branches hidden
├─ |🗄 Either |> with_name(:digit)
│  ├─ 0 
│  └─ |🗄 Either |> with_name(:digit excluding zero) # branches hidden
└─ |🗄 Either |> with_name(:digit excluding zero)
   ├─ 1 
   ├─ 2 
   ├─ 3 
   ├─ 4 
   ├─ 5 
   ├─ 6 
   ├─ 7 
   ├─ 8 
   └─ 9 
::Union{SubString{String}, Tuple{SubString{String}, Vector{SubString{String}}}, Tuple{AbstractString, Tuple{SubString{String}, Vector{SubString{String}}}}}

julia> p[:integer]("42")
("", ("4", SubString{String}["2"]))
```

A (complicated) result type is derived implicitly.

You can map transform results of parts of a EBNF parser with the [`deepmap`](@ref) function (not the changed `result_type`)).
```jldocs
julia> pmatch = deepmap(JoinSubstring, p, :integer)
|🗄 Either
├─ |🗄 Either |> ! |> with_name(:integer)
│  ├─ 0 
│  └─ 🗄 Sequence
│     ├─ \\-? |
│     └─ 🗄 Sequence |> with_name(:natural number) # branches hidden
├─ 🗄 Sequence |> with_name(:natural number)
│  ├─ |🗄 Either |> with_name(:digit excluding zero) # branches hidden
│  └─ |🗄* Either |> with_name(:digit) |> Repeat
│     ├─ 0 
│     └─ |🗄 Either |> with_name(:digit excluding zero) # branches hidden
├─ |🗄 Either |> with_name(:digit)
│  ├─ 0 
│  └─ |🗄 Either |> with_name(:digit excluding zero) # branches hidden
└─ |🗄 Either |> with_name(:digit excluding zero)
   ├─ 1 
   ├─ 2 
   ├─ 3 
   ├─ 4 
   ├─ 5 
   ├─ 6 
   ├─ 7 
   ├─ 8 
   └─ 9 
::Union{SubString{String}, Tuple{SubString{String}, Vector{SubString{String}}}}

julia> pmatch[:integer]("42")
"42"
```

!!! note
    I want to support more BNF variants.  Contributions of test cases are welcome!
    A EBNF Syntax draft built from Wikimedia
    [Ebnf-syntax-diagram](https://upload.wikimedia.org/wikipedia/commons/0/0c/Ebnf-syntax-diagram.png).

!!! warn
    Left recursion is not yet supported (will lead to a stack overflow).
"""
ebnf

end
