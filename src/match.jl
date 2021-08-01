export MatchesIterator, ParseMatch

"""
    MatchesIterator(parser::P, sequence::S, from=firstindex(sequence), till=lastindex(sequence))

Iterator type for [`match_all`](@ref) and [`parse_all`](@ref) with `eltype` [`ParseMatch`](@ref)`{P,S,state_type(P)}`.
"""
@auto_hash_equals struct MatchesIterator{P,S}
    "parser"
    parser::P
    "sequence"
    sequence::S
    "First index for [`match`](@ref)."
    from::Int
    "Last index for [`match`](@ref)."
    till::Int
    MatchesIterator(parser::P,sequence::S, from=firstindex(sequence),till=lastindex(sequence)) where {P,S} =
        new{P,S}(parser,sequence,from,till)
end
result_type(::Type{<:MatchesIterator{P}}) where P =
    result_type(P)
Base.eltype(T::Type{<:MatchesIterator{P,S}}) where {P,S} =
    ParseMatch{P,S,state_type(P)}
Base.IteratorSize(::Type{<:MatchesIterator}) =
    Base.SizeUnknown()

export match_all

"""
    match_all(parser::CombinedParser, sequence[, idx=firstindex(sequence)[, till=lastindex(sequence)]]; log=nothing)

Returns an iterator over all matches at all indices in the sequence.
"""
function match_all(parser::CombinedParser, sequence, idx=firstindex(sequence), till=lastindex(sequence); log=nothing)
    p = (log === nothing || log == false ) ? parser : log_names(parser,log)
    MatchesIterator(p, sequence, idx, till)
end

export parse_all
"""
    parse_all(parser::CombinedParser, sequence, idx=1)

Returns an iterator over all parsings of the sequence starting at `idx`.
"""
function parse_all(parser::CombinedParser, sequence, a...; kw...)
    ( get(p) for p=ParseMatch(parser,sequence, a...) )
end




export ParseMatch
"""
    ParseMatch(p::MatchesIterator{P,S}, start::Integer, stop::Integer, state::ST) where {P,S,ST}

Wrapper type for [`CombinedParsers.Regexp.SequenceWithCaptures`](@ref), providing
`getindex` and `getproperty` behavior like `RegexMatch`.
"""
@auto_hash_equals struct ParseMatch{P,S,State}
    parsings::MatchesIterator{P,S}
    start::Int
    stop::Int
    state::State
    function ParseMatch(p::MatchesIterator{P,S},start::Integer,stop::Integer,state::ST) where {P,S,ST}
        new{P,S,ST}(
            p,
            start,stop,state)
    end
end

ParseMatch(parser,s,start=1,stop=start,state=nothing) = ParseMatch(MatchesIterator(parser,s), start, stop, state)

# """
#     iterate(m::CombinedParser,s::AbstractString)

# Returns `iterate(ParseMatch(m,s,1,1,nothing))`.


result_type(::Type{<:ParseMatch{P}}) where P = result_type(P)
@inline _iterate(m::ParseMatch) =
    _iterate(m.parsings,m.start,m.stop,m.state)
@inline _iterate(mi::MatchesIterator,a...) =
    _iterate(mi.parser, mi.sequence, mi.till, a...)

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
    get(x.parsings,x.stop,x.start,x.state)

Base.get(x::MatchesIterator, a...)=
    get(x.parser,x.sequence,x.till, a...)

Base.eltype(T::Type{<:ParseMatch}) = ParseMatch
Base.IteratorSize(::Type{<:ParseMatch}) = Base.SizeUnknown()

"""
    Base.iterate(x::ParseMatch[, m::ParseMatch=x])

Returns next [`ParseMatch`](@ref) at `m.start` after `m.state`, see [`_iterate`](@ref)(m).
"""
function Base.iterate(x::ParseMatch, m=x)
    i = _iterate(m)
    parsematch_tuple(m.parsings,m.start,i)
end

"""
    parsematch_tuple(m,start,state)

ParseMatch iteration has the first match as iterator, the last match as a state.
(Turned out to be fastest.)
"""
parsematch_tuple(m,start,state) =
    let r = ParseMatch(m,start,tuple_pos(state),tuple_state(state))
        return tuple(r,r)
    end
parsematch_tuple(m,start,state::Nothing) = nothing



import Base: iterate
"""
    Base.iterate(x::MatchesIterator[, s::ParseMatch=ParseMatch(x,x.from,x.from,nothing)])

Iterate match `s` at current position.
While no match is found and `s.start<=x.till`, increase s.start.
Return first next [`ParseMatch`](@ref) (as return value and state) or `nothing` when at `m.till`.
"""
@inline Base.iterate(x::MatchesIterator) =
    iterate(x,ParseMatch(x,x.from))

function Base.iterate(m::MatchesIterator, s::ParseMatch)
    start,stop = s.start, s.stop
    till = m.till
    state = _iterate(m,start,stop,s.state)
    while start <= till+1 && state===nothing
        # state = iterate(m.parsings,(start,nothing))
        start > till && break
        start = _nextind(m.sequence,start)
        state = _iterate(m,start,start,nothing)
    end
    parsematch_tuple(m,start,state)
end





function Base.show(io::IO,m::ParseMatch{<:Any,<:Any,Nothing})
    print(io,"no match")
end

function Base.show(io::IO,m::ParseMatch{<:Any,<:AbstractString,<:Any})
    print(io,"ParseMatch(\"",escape_string(m.match),"\"")
    print(io,")")
end

_getproperty(x::ParseMatch,key) =
    if key == :parsings
        getfield(x,1)
    elseif key == :start
        getfield(x,2)
    elseif key == :stop
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
                  x.start,
                  _prevind(x.parsings.sequence,x.stop))
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
    get(p,s,lastindex(s),tuple_pos(i),1,tuple_state(i)), tuple_pos(i)
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
