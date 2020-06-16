# # Number lists (wikitext references)
#
# Wonder what parsing is useful for?
# This example presents how to read a convenient number list format, e.g.  `[1-3,9]`, of the Wikipedia wikitext format.
# This makes a simple example for what parsing into Julia types means, too.
#
# To reflect on the amazing Julia type system, the example shows
# - different ways to represent such number `Iterator`s in Julia,
1:3 == [1,2,3]
# - how you parse into any such representation
using CombinedParsers
# - inter-operation with brief regular expression (PCRE) syntax
using CombinedParsers.Regexp
dash = re" *- *"
# - inter-operation with
import TextParse
# ## Number ranges
@syntax int_range = Sequence(
    Numeric(Int), # 1
    dash,    # 2
    Numeric(Int)  # 3
) do v
    v[1]:v[3]
end;

# to match cases like "8  - 11".
# Julias number range format is `1:3`.
# The string macro `@int_range_str` is defined in `@syntax`.

int_range"1-3"
int_range"8-11"


# Julia Base.collect can be used to convert 
@syntax int_vector = map(collect, int_range);
int_vector"8  - 11"

# ## Numbers
# Without `@syntax`, you can parse
int = map(x -> [x], Numeric(Int));

parse(int,"19")
int("19")

# The tree displays how a number is read and transformed to a Vector with length 1.
int

# ## Joining numbers and ranges
@syntax numbers = map(join(
    Repeat(Either(int_vector, int)),
    re" *, *"
)) do v
    vcat(v...)::Vector{Int}
end;

numbers"1-3,9"

# Prepend another parser by
(re"no *"*numbers)[end]("no 2-4,19")

# ## Inclusion in a wikitext parser
# Long and complicated texts like the Wikipedia can be parsed with `CombinedParsers.jl`.
# The parsers are less pain to write and execute at speeds comparably to PCRE implemented in C, the regular expressions industry standard.
# `CombinedParsers.jl` can inter-operate with Julia packages `TextParse.jl`.

@syntax wiki_references = Sequence(2,"[",numbers,"]");

wiki_references"[1, 7-9, 2]"

# The tree displays how a bracketed comma separated sequence of numbers and number ranges is read and transformed to a Vector.
wiki_references


# ## PCRE papercuts when parsing number sequences
# The same parser as a regular expression will be tedious to understand and write (though writing the regular expression `re" *- *"` is clear).
# PCRE matching does recognize the match but makes not all required parsing parts accessible (7 is not captured).
re = "\\[([[:digit:]]+ *- *[[:digit:]]+|[[:digit:]]+)(?: *, *([[:digit:]]+ *- *[[:digit:]]+|[[:digit:]]+))*\\]"
match(Regex("^"*re*"\$"), "[1-3,7,9]")
### TODO: regex_string(wiki_references)

# To make the parsing work with regular expressions you would choose a stepwise strategy, handling `[` and `]` and stepping though `,` separated parts.
# Parsing was pain.
