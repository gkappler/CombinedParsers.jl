"""
A EBNF Syntax draft built from Wikimedia
[Ebnf-syntax-diagram](https://upload.wikimedia.org/wikipedia/commons/0/0c/Ebnf-syntax-diagram.png).
"""
module BNF
using ..CombinedParsers

# todo 
# paddedSequence(x...) = Sequence(trim.(x)...)
trimhv(x) = CombinedParsers.trim(x;whitespace=Atomic(Repeat(CharIn(horizontal_space_char,vertical_space_char))))

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


letter                              = CharIn('A':'Z','a':'z')
decimal_digit                       = CharIn('0':'9')
concatenate_symbol                  = ','
defining_symbol                     = '='
definition_separator_symbol         = CharIn("|/!")
start_comment_symbol                = "(*"
start_group_symbol                  = "("
end_comment_symbol                  = "*)"
end_group_symbol                    = ")"
end_option_symbol                   = Either(']', "/)")
end_repeat_symbol                   = Either('}', ":)")

except_symbol                       = '-'
first_quote_symbol                  = '''
repetition_symbol                   = '*'
second_quote_symbol                 = '"'
special_sequence_symbol             = '?'

start_option_symbol                 = Either('[', "(/")
start_repeat_symbol                 = Either('{', "(:")
terminator_symbol                   = CharIn(";.")

other_character                     = CharIn(" :+_%@&#\$<>\\^`~") # is it `?
space_character                     = ' '
horizontal_tabulation_character     = horizontal_space_char#  "\t" #^-?
new_line                            = '\n' #?
vertical_tabulation_character       = vertical_space_char# Never() #?
form_feed                           = '\f' # Never() #?

terminal_character                  = !Atomic(Either(
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

first_terminal_character            = Sequence(NegativeLookahead(first_quote_symbol),
                                               terminal_character)

second_terminal_character           = Sequence(NegativeLookahead(second_quote_symbol),
                                               terminal_character)

@with_names terminal_string         = Either(Sequence(2,first_quote_symbol,
                                                      !!Repeat1(first_terminal_character),
                                                      first_quote_symbol),
                                             Sequence(2,second_quote_symbol,
                                                      !!Repeat1(second_terminal_character),
                                                      second_quote_symbol))

gap_separator                       = CharIn(space_character, horizontal_tabulation_character,
                                             new_line, vertical_tabulation_character, form_feed)

@with_names _integer                = CombinedParsers.Numeric(Int) #!Repeat1(decimal_digit)

meta_identifier_character           = CharIn(letter, decimal_digit, "-_ ") # optimize!
@with_names meta_identifier         = map(Symbol,Sequence(1, 
    !join(!Sequence(letter, Lazy(Repeat(meta_identifier_character))), gap_separator),
    PositiveLookahead(Either(space_character,horizontal_tabulation_character,AtEnd()))
))

special_sequence_character          = Sequence(NegativeLookahead(special_sequence_symbol),
                                               terminal_character)
@with_names special_sequence        = Sequence(2,
                                               special_sequence_symbol,
                                               trimhv(!!Lazy(Repeat(special_sequence_character))),
                                               special_sequence_symbol)

commentless_symbol                  = Either(
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

comment_symbol                      = Either{Any}(Any[
    other_character,
    commentless_symbol
])
@with_names bracket_textual_comment = Sequence(start_comment_symbol, !Repeat(comment_symbol), end_comment_symbol)
pushfirst!(comment_symbol, bracket_textual_comment)


empty_sequence                      = parser(Always() => Always())


syntactic_primary                   = Either{CombinedParser}(
    Any[map(n->Substitution(n), meta_identifier),
        map(parser,terminal_string),
        map(s->Never(),special_sequence),
        empty_sequence])

syntactic_factor                    = Sequence(
    Optional(
        Sequence(1, _integer, trimhv(repetition_symbol));
        default                         = 1),
    syntactic_primary) do v 
        v[1]                        == 1 ? v[2] : Repeat(v[1], v[2])
    end

syntactic_exception                 = syntactic_factor
syntactic_term                      = Sequence(1, syntactic_factor,
                                               # todo: handle exceptions
                                               Optional(Sequence(except_symbol, syntactic_exception)))

@with_names single_definition       = map(p -> sSequence(p...)::CombinedParser,
                                          join(syntactic_term, trimhv(concatenate_symbol)))

definitions_list                    = map(p -> sEither(p...)::CombinedParser,
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

@syntax syntax_rule                 = Sequence(meta_identifier, trimhv(defining_symbol), definitions_list, trimhv(terminator_symbol)) do v
    with_name(v[1], v[3])
end;

@syntax ebnf  = map(Either,Repeat1(syntax_rule));
end
