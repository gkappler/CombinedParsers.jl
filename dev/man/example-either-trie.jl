# # A fast [Trie](https://github.com/gkappler/Tries.jl)-based parser for a collection of literal Strings.
# Matching any of many words (e.g. names) is slow in PCRE with `|` alternatives.
using Random
s = [ randstring(10) for _ in 1:1000 ];
re = Regex(join(s,"|"));
@benchmark match(re,s[end])

# A optimized state-machine parser will perform much better.
#
# TODO: Benchmark Automa.jl here
#
# `CombinedParsers` does not use state machines, but can 
# `CombinedParsers` provides fast `_iterate` implementation for `Either{Trie{Char}}`,
# that is constructed with `Either(::Vector{<:AbstractString})`
pc = Either(s);
@benchmark match(pc,s[end])

# Note that `pc` does not capture the result by default with `parse`.
parse(!pc,s[end])

# For large word collections, PCRE fails
s = [ randstring(10) for _ in 1:10000 ];
re = Regex(join(s,"|"));

# `CombinedParsers` displays stable performance.
pc = Either(s);
@benchmark match(pc,s[end])

# `Either{Trie{Char}}` can be used to perform fast text search in Julia.
