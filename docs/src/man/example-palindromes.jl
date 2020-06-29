# # `Palindromes<:CombinedParser`: a Tutorial for writing your combinable Parser
# Palindromes are an interesting example for parsing because
# intuitively programmers as well as laymen understand the problem: 
# the text is identical when read from left to right, as we are used to do,
# or when read from right to left in reverse, 
# when we read only the letters and discard all non-word characters.
#
# This example enables you to write your own custom `CombinedParser` based off a minimal template.
# ## 1. A non-word skipping palindrome regex
# The PCRE test case contains nice examples of non-trivial palindromes.
# The tested regular expression matching these palindromes is cryptic and requires arcane reasoning even to the initiated.
#
using CombinedParsers
using CombinedParsers.Regexp
## defines parsers for pcre tests:
CombinedParsers.Regexp.@pcre_tests; 

pt = pcre_test"""
/^\W*+(?:((.)\W*+(?1)\W*+\2|)|((.)\W*+(?3)\W*+\4|\W*+.\W*+))\W*+$/i
    1221
 0: 1221
 1: 1221
 2: 1
    Satan, oscillate my metallic sonatas!
 0: Satan, oscillate my metallic sonatas!
 1: <unset>
 2: <unset>
 3: Satan, oscillate my metallic sonatas
 4: S
    A man, a plan, a canal: Panama!
 0: A man, a plan, a canal: Panama!
 1: <unset>
 2: <unset>
 3: A man, a plan, a canal: Panama
 4: A
    Able was I ere I saw Elba.
 0: Able was I ere I saw Elba.
 1: <unset>
 2: <unset>
 3: Able was I ere I saw Elba
 4: A
\= Expect no match
    The quick brown fox
No match
"""


# It is interesting that this case ignoring PCRE pattern matches palindromes:
re = Regex(pt.pattern...)
# I figure the expression is hard to construct and come up with.
# The easy part is that the pattern needs to ignore case and whitespace `\W`.
## TODO: re"\W" show `UnicodeClass`
#
# The pattern makes intense use of backreferences and subroutines.

# 
s=pt.test[3].sequence

# PCRE matching example 3 is fast
using BenchmarkTools
@time match(re, s)

# ### Tree display of regex
# I find it hard to understand the compact captures `(.)`, even in a nested tree display:
cp = Regcomb(pt.pattern...)
# Why no backreference `\1`, why no subroutine `(?2)`?
# Theoretical linguists, I wonder, is the minimum number of capture groups 4, for a regular expression matching palindromes?

# Writing a palindrome parser should be easier.
# And with julia compiler it should be faster.
#
# In practice `CombinedParsers` [`Regcomb`](@ref) of the regular expression will detect palindromes too.
# Palindrome matching provides an interesting cross-parser performance benchmark.
@time match(cp, s)
# `CombinedParsers.Regexp.Subroutine` matching is slow because the current implementation is using state-copies of captures.
# (TODO: could be a stack?).

# ## 2. A non-word skipping `Palindrome<:CombinedParser`
# This example of `Palindrome<:CombinedParser` is a much faster palindrome parser and more interesting and more easy to write.
# It mimics the human readable palindrome rule that is clear and quite easy to comprehend:
#
# the text is identical when read from left to right, as we are used to do,
# or when read from right to left in reverse,
# when we read only the letters and skip all non-word characters.
#
# This rule is efficient programming in natural language.
# After defining the parser, the third part of the example discusses the design of match iteration in `CombinedParsers`.
#
# #### Parsing strategy
# A custom parser needs a method to determine if there is a match and its extent at a position.
# How can this be implemented for a palindrome?
# There are two strategies:
# 1. inside-out: start at a position as `center`
#    - expand `left` and `right` from `center` until they are at word characters 
#    - until word character left does not match word character at right.
#    - Succeed if a minimal length is met. Fail otherwise.
# 2. outside-in: start `left` and `right`,
#    - move positions towards `center` until they are at word characters and
#    - succeed if left and right positions meet at the center,
#    - compare these characters, and proceed to the next positions if the word characters match or fail if there is a mismatch.
#    (This might be [the-fastest-method-of-determining-if-a-string-is-a-palindrome](https://stackoverflow.com/questions/21403782/the-fastest-method-of-determining-if-a-string-is-a-palindrome).  But I figure finding all palindrome matches in a string is slow because you would be required to test for all possible substrings.)
# The inside out strategy seems easier and faster.
#
# #### Prerequisite: Skipping whitespace
# For the string `"two   words"`,  from the point of index 4 (`' '` after "from") the next word character after skipping whitespace left and right are indices of 3 (tw`o`) and 7 (`w`ords).
# In Julia syntax, this is expressed in terms of `direction` (functions `Base.prevind` and `Base.nextind` return next index to left or right), and `word_char::T`, what makes up a word character (provided method `CombinedParser.ismatch(char,parser::T)::Bool`.)

@inline function seek_word_char(direction,str,i,
                                till=lastindex(str),
                                word_char=UnicodeClass(:L))
    i=direction(str,i)
    while i>0 && i<=till && !CombinedParsers.ismatch((@inbounds str[i]),word_char)
        i=direction(str,i)
    end
    return i
end
( prev_index=seek_word_char(prevind, "two   words", 4),
  next_index=seek_word_char(nextind, "two   words", 4) )

# #### Subtyping `<: CombinedParser{STATE,RESULT}`.
# Subtyping requires you to define the type of the parsing state (for julia compiler optimizations)
# and the type of the parsing result.
STATE = NamedTuple{(:left,:center,:right),Tuple{Int,Int,Int}}
RESULT = SubString
struct Palindrome{P} <: CombinedParser{STATE,RESULT}
    word_char::P
    Palindrome() = Palindrome(UnicodeClass(:L))
end

# ### Matching
# With the inside-out stratedy, the implementation greedily expands over non-word characters.
# Computing the first match at `posi`tion is done by this method dispatch
function CombinedParsers._iterate(x::Palindrome,
                                  str, till,
                                  posi, after,
                                  state::Nothing)
    right_ = left_ = left = right = posi
    while left>0 && right<=till &&
          lowercase(@inbounds str[left])==lowercase(@inbounds str[right])
        ## if we cannot expand, (left_,right_) succeeded
        right_ = right 
        left_ = left
        left =  seek_word_char(
            prevind,str,
            left,till,x.word_char)        
        right = seek_word_char(
            nextind,str,
            right,till,x.word_char)
    end
    left, left_, right_, right
    if left_ == right_ 
        nothing
    else
        tuple(nextind(str,right_),
              (left=left_, center=posi, right=right_))
    end
end
# `_iterate` matches the right part of the palindrome if and only if `posi` at the center of a palindrome. 
#
# The internal API calls (for the center index 18):
state = _iterate(Palindrome(),s,lastindex(s),18,18,nothing)

# ### `prevind` and `nextind`
# `CombinedParsers` iterates through matches based on the parsing position and state.
Base.nextind(str,i::Int,p::Palindrome,state) =
    nextind(str,state.right)
# Note that for the inside-out strategy the `Palindrome<:CombinedParser` matches from `center` and looks behind until `right`, possibly overlapping with the last match(es).
# The start index of a palindrome match is its center.
Base.prevind(str,after::Int,p::Palindrome,state) =
    state.center
 

# ### `match` and `get`
# [`_iterate`](@ref) is called when the public API `match` or `parse` is used.
# Match searches for a center index and then matched the `state.center:state.right` part of the palindrome. 
p = Palindrome()
m = match(p,s)


# The result of a parsing is the matching substring from `state.left:state.right`, 
# implementing `Base.get` with the full argument range:
Base.get(x::Palindrome, str, till, after, posi, state) =
    SubString(str,state.left,state.right)

# The match result is matching the first palindrome, which is short and simple - but not yet what we want.
get(m)

# ### Iterating through matches
# The longest palindrome is matched too:
p = Palindrome()
[ get(m) for m in match_all(p,s) ]


# To skip trivial short palindromes we can use `Base.filter` 
long_palindrome = filter(Palindrome()) do sequence, till, posi, after, state
    state.right-state.left+1 > 5
end
get(match(long_palindrome,s))

# ## Iteration of smaller Sub-palindromes
# The set of all palindromes in a text includes the shorter palindromes contained in longer ones.
# Provide a method to iterate the previous state:
"Shrinking `state` match"
function CombinedParsers._iterate(x::Palindrome, str, till, posi, after, state)
    left_, posi_, right_ = state
    left =  seek_word_char(
        nextind,str,
        left_,till,x.word_char)
    right = seek_word_char(
        prevind,str,
        right_,till,x.word_char)
    if left >= right # left == posi
        nothing
    else
        tuple(nextind(str,right),
              (left=left, center=posi_, right=right))
    end
end

[ get(m) for m in match_all(p,s) ]

# Note that the greedy-only behaviour was atomic in terms of regular expression, which can be restored with [`Atomic`](@ref)
p = Atomic(Palindrome())
get.(match_all(p,s)) |> collect

# ## Padding and combining
# Note that the PCRE pattern included outside non-words, specifically the tailing `!`.
re = Regex(pt.pattern...)
match(re,s)

# ``CombinedParsers` are designed with iteration in mind, and a small match set reduces computational time when iterating through all matches.
# `Palindrome` matches palindromes with word-char boundaries.
# The PCRE pattern includes non-words matches in the padding of palindromes, a superset of `Palindrome`.
# PCRE-equivalent matching can be achieved by combining the stricly matching `Palindrome` with parsers for the padding.
padding=Repeat(CharNotIn(p.parser.word_char))
match(p*padding*AtEnd(),s) |> get

# TODO: Memoization here!
#
# `Palindrome` matches from center to right, like a lookbehind parser.
# A prefix parser to the left requires a parser for the left-part coupled by filter:
palindrome = filter(
    Sequence(
        2,
        Lazy(Repeat(AnyChar())),
        Atomic(Palindrome()))) do sequence, till, posi, after, state
            ## posi is the start of the sequence
            posi==state[2].left
        end

# Now we can express the full pattern
p = AtStart() * padding * (palindrome) * padding * AtEnd()
match(p,s)

# Or with
parse(p,s)
# ## Next...
# - match also palindromes with odd number of letters
# - elaborate on iteration documentation
# - comparative benchmarking, conditional on palindrome length
