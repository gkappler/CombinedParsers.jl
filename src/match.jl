export MatchesIterator, ParseMatch

"""
    MatchesIterator(parser::P, sequence::S[, start=firstindex(sequence)[, stop=lastindex(sequence), [till=lastindex(sequence)]]])

Iterator type for [`match_all`](@ref) and [`parse_all`](@ref) with `eltype` [`ParseMatch`](@ref)`{P,S,state_type(P)}`.

Iteration looks for matches beginning between `start` and `stop` and ending at most at `till`.
"""
@auto_hash_equals struct MatchesIterator{P<:CombinedParser,S}
    "parser"
    parser::P
    "sequence"
    sequence::S
    "First index for searching [`match`](@ref)."
    start::Int
    "Last index for searching [`match`](@ref)."
    stop::Int
    "Last index for end of [`match`](@ref)."
    till::Int
end
result_type(::Type{<:MatchesIterator{P}}) where P =
    result_type(P)
Base.eltype(T::Type{<:MatchesIterator{P,S}}) where {P,S} =
    ParseMatch{P,S,state_type(P)}
Base.IteratorSize(::Type{<:MatchesIterator}) =
    Base.SizeUnknown()

@inline _iterate(mi::MatchesIterator,a...) =
    _iterate(mi.parser, mi.sequence, mi.till, a...)

Base.get(x::MatchesIterator, a...)=
    get(x.parser,x.sequence,x.till, a...)

"""
    MatchesIterator(parser::CombinedParser, sequence, start=firstindex(sequence),stop=lastindex(sequence),till=lastindex(sequence))


`MatchesIterator` dispatch can be used for parsers that require a special sequence type.
"""
MatchesIterator(parser, sequence, start::Int=firstindex(sequence),stop::Int=lastindex(sequence),till::Int=lastindex(sequence)) =
    MatchesIterator{typeof(parser),typeof(sequence)}(parser,sequence,start,stop,till)

export match_all

"""
    match_all(parser::CombinedParser, sequence, a...; kw...)

Returns an iterator over all matches of [`CombinedParsers.wrap`](@ref)`(parser; kw...)`.
Constructs a [`MatchesIterator`](@ref) defining match index range with with `a...`.
"""
function match_all(parser, sequence, a...; kw...)
    MatchesIterator(wrap(parser; kw...), sequence, a...)
end

export ParseMatch
"""
    ParseMatch(p::MatchesIterator{P,S}, offset::Integer, after::Integer, state::ST) where {P,S,ST}

You can extract the following info from a `m::ParseMatch` object 
(like [Julia RegexMatch](https://docs.julialang.org/en/v1/manual/strings/#Regular-Expressions),
):

- the entire substring matched: `m.match`
- the offset at which the whole match begins: `m.offset`

If `P<:`[`CombinedParsers.Regexp.ParserWithCaptures`](@ref) and `S<:`[`CombinedParsers.Regexp.SequenceWithCaptures`](@ref)

- the captured substrings as an array of strings: `m.captures`
- the offsets of the captured substrings as a vector: `m.offsets`
"""
@auto_hash_equals struct ParseMatch{P,S,State}
    parsings::MatchesIterator{P,S}
    offset::Int
    after::Int
    state::State
    function ParseMatch(p::MatchesIterator{P,S}, offset=p.start, after=p.start, state=nothing) where {P,S}
        new{P,S,typeof(state)}(p, offset, after, _copy(state))
    end
end

_getproperty(x::ParseMatch,key) =
    if key == :parsings
        getfield(x,1)
    elseif key == :offset
        getfield(x,2)
    elseif key == :after
        getfield(x,3)
    elseif key == :state
        getfield(x,4)
    else
        error("no property $key")
    end

function Base.getproperty(x::ParseMatch{<:Any,<:AbstractString,<:Any},key::Symbol)
    if key==:captures
        AbstractString[ ]
    elseif key==:match
        SubString(x.parsings.sequence,
                  x.offset,
                  _prevind(x.parsings.sequence,x.after))
    else
        _getproperty(x,key)
    end
end

Base.getproperty(x::ParseMatch{<:Any,<:AbstractString,Nothing},key::Symbol) = 
    if key==:captures
        AbstractString[ ]
    elseif key==:match
        ""
    else
        _getproperty(x,key)
    end

# ParseMatch(parser,s,offset=1,after=offset,state=nothing) =
#     ParseMatch(match_all(parser,s,offset), offset, after, state)

export parse_all
"""
    parse_all(parser::CombinedParser, sequence, idx=1)

Returns an iterator over all parsings of the sequence offset at `idx`.
"""
function parse_all(parser::CombinedParser, sequence, idx=firstindex(sequence); kw...)
    ( get(p) for p=ParseMatch(MatchesIterator(parser,sequence, idx, idx)) )
end

# """
#     iterate(m::CombinedParser,s::AbstractString)

# Returns `iterate(ParseMatch(m,s,1,1,nothing))`.


result_type(::Type{<:ParseMatch{P}}) where P = result_type(P)
@inline _iterate(m::ParseMatch) =
    _iterate(m.parsings,m.offset,m.after,m.state)

"""
    Base.get(x::ParseMatch{<:MatchTuple})

Get the result of a match result.

```jldoctest
julia> m = match(re"(?<a>so)+ (or)", "soso or")
ParseMatch("soso or", a="so", 2="or")

julia> get(m)
([('s', 'o'), ('s', 'o')], ' ', ('o', 'r'))

julia> m[2]
"or"

julia> m.match, m.captures
("soso or", SubString{String}["so", "or"])
```
"""
Base.get(x::ParseMatch)=
    get(x.parsings,x.after,x.offset,x.state)

Base.eltype(T::Type{<:ParseMatch}) = ParseMatch
Base.IteratorSize(::Type{<:ParseMatch}) = Base.SizeUnknown()

"""
    Base.iterate(x::ParseMatch[, m::ParseMatch=x])

Returns next [`ParseMatch`](@ref) at `m.offset` after `m.state`, see [`_iterate`](@ref)(m).
"""
function Base.iterate(x::ParseMatch, m=x)
    i = _iterate(m)
    parsematch_tuple(m.parsings,m.offset,i)
end

"""
    parsematch_tuple(m,offset,state)

ParseMatch iteration has the first match as iterator, the last match as a state.
(Turned out to be fastest.)
"""
parsematch_tuple(m,offset,state) =
    let r = ParseMatch(m,offset,tuple_pos(state),tuple_state(state))
        return tuple(r,r)
    end
parsematch_tuple(m,offset,state::Nothing) = nothing



import Base: iterate
"""
    Base.iterate(x::MatchesIterator[, s::ParseMatch=ParseMatch(x)])

Iterate match `s` at current position.
While no match is found and `s.offset<=x.stop`, `s.offset` is incremented to search.

Return first next [`ParseMatch`](@ref) (as return value and state) or `nothing` when at `x.stop`.
"""
@inline Base.iterate(x::MatchesIterator) =
    iterate(x,ParseMatch(x))

@inline function Base.iterate(m::MatchesIterator, s::ParseMatch)
    offset,after = s.offset, s.after
    stop = m.stop
    state = _iterate(m,offset,after,s.state)
    while offset <= stop+1 && state===nothing
        # state = iterate(m.parsings,(offset,nothing))
        offset > stop && break
        offset = _nextind(m.sequence,offset)
        state = _iterate(m,offset,offset,nothing)
    end
    parsematch_tuple(m,offset,state)
end





function Base.show(io::IO,m::ParseMatch{<:Any,<:Any,Nothing})
    print(io,"no match")
end

function Base.show(io::IO,m::ParseMatch{<:Any,<:AbstractString,<:Any})
    print(io,"ParseMatch(\"",escape_string(m.match),"\"")
    print(io,")")
end

"""
    wrap(x::CombinedParser; log = nothing, trace = false)

transform a parser by wrapping sub-parsers in logging and tracing parser types.
"""
function wrap(x::CombinedParser; log = nothing, trace = false)
    p = if log === nothing || log == false
        x
    else
        log_names(x,log)
    end 
    if trace
        CombinedParsers.trace(p)
    else
        p
    end
end
wrap(x::CombinedParser) = x

"""
    Base.match(parser::CombinedParser,sequence::AbstractString[, idx::Integer]; log=nothing)

Search for the first match of `parser` in `sequence` and return a [`ParseMatch`](@ref) object containing the match, 
or `nothing` if the match failed. 

The optional `idx` argument specifies an index at which to start the search.

If `log!==nothing`, parser is transformed with [`log_names`](@ref)`(p, log)`.

The matching substring can be retrieved by accessing m.match.

!!! note 
    If `parser isa CombinedParsers.Regexp.ParserWithCaptures`,
    `match` behaves like a plug-in replacement for equivalent `match(::Regex,sequence)`:

    ```jldoctest
    julia> m = match(re"(?<a>so)+ (or)", "soso or")
    ParseMatch("soso or", a="so", 2="or")

    julia> m[:a]
    "so"

    julia> m[2]
    "or"

    julia> m.match, m.captures
    ("soso or", SubString{String}["so", "or"])

    ```

"""
function Base.match(parser::CombinedParser, sequence, pos...; kw...)
    i = iterate(MatchesIterator(wrap(parser;kw...), sequence, pos...))
    i === nothing && return nothing
    i[1]
end

import Base: tryparse, parse
export tryparse_pos
function Base.parse(p::AbstractToken, s, pos...; kw...)
    i = tryparse_pos(p, s, pos...; kw...)
    i === nothing && throw(ArgumentError("no successfull parsing."))
    i[1]
end

function Base.tryparse(p::AbstractToken, s, pos...; kw...)
    i = tryparse_pos(p, s, pos...; kw...)
    i === nothing && return nothing
    i[1]
end

function tryparse_pos(p,s, idx=firstindex(s), till=lastindex(s); kw...)
    i = _iterate(wrap(p; kw...),s,till,idx,idx,nothing)
    i === nothing && return nothing
    get(p,s,till,tuple_pos(i),1,tuple_state(i)), tuple_pos(i)
end

"""
    parse(parser::CombinedParser, sequence[, idx=firstindex(sequence)[, till=lastindex(sequence)]]; log=nothing)

Parse `sequence` with `parser` at start and produce an instance of `result_type(parser)`.
If `log!==nothing`, parser is transformed with [`log_names`](@ref)`(p, log)` before matching.

    tryparse(parser::CombinedParser, sequence[, idx=firstindex(sequence)[, till=lastindex(sequence)]]; log=nothing)

returns either a result value or `nothing` if sequence does not start with with a match.

    tryparse_pos(parser::CombinedParser, str::AbstractString[, idx=firstindex(sequence)[, till=lastindex(s)]]; log=nothing)
returns either a tuple of result value and the position after the match, or `nothing` if sequence does not start with with a match.

# Example

```jldoctest
julia> using TextParse

julia> p = ("Number: "*TextParse.Numeric(Int))[2]
🗄 Sequence[2]
├─ Number\\:\\
└─ <Int64>
::Int64


julia> parse(p,"Number: 42")
42

```

"""
parse, tryparse, tryparse_pos
