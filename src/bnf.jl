"""
# (Extended) Backus-Naur Form [`CombinedParser`](@ref)
Defining a EBNF parser can be done with the [`CombinedParsers.BNF.ebnf`](@ref) string macro.
[`substitute`](@ref) is used to combine parts of the definition.

!!! warn
    Left recursion is not yet supported (will lead to a stack overflow).
"""
module BNF
using CombinedParsers
import ..CombinedParsers: Repeat_max

const whitespace_options = Either{Any}([CharIn(
    CombinedParsers.horizontal_space_char,
    CombinedParsers.vertical_space_char)])

const skip_whitespace =
    with_name(
        :whitespace,
        Atomic(Repeat(CharIn(
            CombinedParsers.horizontal_space_char,
            CombinedParsers.vertical_space_char))))

# todo 
trimhv(x; whitespace=skip_whitespace) =
    CombinedParsers.trim(x; whitespace=whitespace)

function separatedTriple(
    f::Function, open,body,close;
    whitespace=skip_whitespace)
    mSequence(whitespace, open,
              whitespace, body,
              whitespace, close,
              whitespace) do v
                  f(v[4])
              end
end
const decimal_digit = CharIn('0':'9')
const letter = CharIn('A':'Z','a':'z')



"""
Supports BNF and EBNF variants
```jldocs
julia> CombinedParsers.BNF.concatenate_symbol
```
"""
const concatenate_symbol = ',' # Either(',', Always())

"""
Supports BNF and EBNF variants
```jldocs
julia> CombinedParsers.BNF.defining_symbol
```
"""
const defining_symbol = '=' # !Either('=', "::=", ":=")
const definition_separator_symbol = CharIn("|/!")
const start_comment_symbol = "(*"
const start_group_symbol = "("
const end_comment_symbol = "*)"
const end_group_symbol = ")"
const end_option_symbol = with_name(:end_option_symbol,  Either(']', "/)"))
const end_repeat_symbol = with_name(:end_repeat_symbol,  Either('}', ":)"))

const except_symbol = '-'
const first_quote_symbol = '''
const repetition_symbol = '*'
const second_quote_symbol = '"'
const special_sequence_symbol = '?'

const start_option_symbol = with_name(:start_option_symbol,  Either('[', "(/"))
const start_repeat_symbol = with_name(:start_repeat_symbol,  Either('{', "(:"))
const terminator_symbol = CharIn(";.")
const other_character = CharIn(" :+_%@&#\$<>\\^`~") # is it `?
const space_character = ' '
const new_line = '\n' #?
const form_feed = '\f' # Never() #?

const horizontal_tabulation_character =  CombinedParsers.horizontal_space_char#  "\t" #^-?
const vertical_tabulation_character =  CombinedParsers.vertical_space_char# Never() #?

const meta_identifier_character = CharIn(letter, decimal_digit, "-_") # optimize!
const gap_separator =  with_name(:gap_separator,  CharIn(space_character, horizontal_tabulation_character,
                                   new_line, vertical_tabulation_character, form_feed))
const meta_identifier =
    with_name(:meta_identifier,
              map(
                  Symbol,Atomic(mSequence(
                      1, 
                      !join(!Sequence(letter, (Repeat(meta_identifier_character))), gap_separator),
                      NegativeLookahead(meta_identifier_character .& CharNotIn(' '))
                  ))))


const terminal_character =  with_name(:terminal_character,  !Atomic(Either(
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
)))

const first_terminal_character = 
    Sequence(NegativeLookahead(first_quote_symbol), terminal_character)

const second_terminal_character = 
    Sequence(NegativeLookahead(second_quote_symbol), terminal_character)

const terminal_string =  with_name(:terminal_string, 
    Either(mSequence(2,first_quote_symbol,
                     !!Repeat1(first_terminal_character),
                     first_quote_symbol),
           mSequence(2,second_quote_symbol,
                     !!Repeat1(second_terminal_character),
                     second_quote_symbol)))


const _integer =  CombinedParsers.Numeric(Int) #!Repeat1(decimal_digit)


const special_sequence_character = 
    Sequence(NegativeLookahead(special_sequence_symbol), terminal_character)
const special_sequence =  with_name(:special_sequence, 
    mSequence(2, special_sequence_symbol, trimhv(!!Lazy(Repeat(special_sequence_character))), special_sequence_symbol))

const commentless_symbol =  Either(
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

const comment_symbol =  Either(Any[
    other_character,
    commentless_symbol
])
const bracket_textual_comment =  with_name(:bracket_textual_comment,  Sequence(start_comment_symbol, !Repeat(comment_symbol), end_comment_symbol))
pushfirst!(comment_symbol, bracket_textual_comment)

# skip comments wherever whitespace is skipped!
push!(whitespace_options, bracket_textual_comment) 

const empty_sequence =  parser(Always() => Always())

const syntactic_primary =  with_name(:syntactic_primary,  Either{CombinedParser}(
    Any[map(n->substitute(n), meta_identifier),
        map(parser,terminal_string),
        map(s->Never(),special_sequence),
        empty_sequence]))

const syntactic_factor =  mSequence(
    Either(
        mSequence(v->v[1]:v[3], _integer, trimhv(repetition_symbol), _integer),
        mSequence(v->v[1]:v[1], _integer, trimhv(repetition_symbol)),
        Always() => 1:1),
    trimhv(syntactic_primary)) do v 
        (v[1] == 1:1 ? v[2] : Repeat(v[1], v[2]))::CombinedParser
    end

const syntactic_exception =  syntactic_factor
const syntactic_term =  with_name(:syntactic_term,  mSequence(1, syntactic_factor,
                           # todo: handle exceptions
                           Optional(Sequence(except_symbol, syntactic_exception))))

const single_definition =  with_name(:single_definition,  map(p -> sSequence(p...)::CombinedParser,
                                    join(syntactic_term, trimhv(concatenate_symbol))))

const definitions_list =  with_name(:definitions_list,  map(p -> Either(p...; simplify=true)::CombinedParser,
                                   join(single_definition, trimhv(definition_separator_symbol))))

const optional_sequence =  with_name(:optional_sequence,  separatedTriple(
    start_option_symbol, definitions_list, end_option_symbol) do v
    Optional(v)
end)
pushfirst!(syntactic_primary, optional_sequence)

const repeated_sequence =  with_name(:repeated_sequence,   separatedTriple(
    start_repeat_symbol, definitions_list, end_repeat_symbol) do v
    Repeat(v)
end)
pushfirst!(syntactic_primary, repeated_sequence)

const grouped_sequence =  with_name(:grouped_sequence,  separatedTriple(
    identity, start_group_symbol, definitions_list, end_group_symbol))
pushfirst!(syntactic_primary, grouped_sequence)



Tuple{Tuple{Vector{Char}, DataType, Vector{Char}}, Vector{Tuple{Vector{Char}, DataType, Vector{Char}}}}


const syntax_rule =  with_name(:syntax_rule,  mSequence(
    meta_identifier, trimhv(defining_symbol), definitions_list, trimhv(terminator_symbol)) do v
    with_name(v[1], v[3])
end)

# export @ebnf_str, ebnf
bnf_parser=map(v->(substitute(Either(reverse(v)...))),mSequence(2,skip_whitespace, Repeat1(syntax_rule),AtEnd()));


export @ebnf_str
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

A (too complicated) result type is derived implicitly.
You can map transform results of parts of a EBNF parser with the [`deepmap`](@ref) function:
```jldocs
julia> deepmap(MatchedSubSequence, p, :integer)[:integer]("42")
"42"
```

!!! note
    I want to support more BNF variants.  Contributions of test cases are welcome!
    A EBNF Syntax draft built from Wikimedia
    [Ebnf-syntax-diagram](https://upload.wikimedia.org/wikipedia/commons/0/0c/Ebnf-syntax-diagram.png).

!!! warn
    Left recursion is not yet supported (will lead to a stack overflow).
"""
macro ebnf_str(x)
    parse(CombinedParsers.BNF.bnf_parser,x;trace=true)
end

end
