export MatchesIterator, ParseMatch

"""
Iterator type for `match_all` and `parse_all`.
"""
@auto_hash_equals struct MatchesIterator{P,S}
    parser::P
    sequence::S
    from::Int
    till::Int
    MatchesIterator(parser::P,sequence::S,from=1,till=lastindex(sequence)) where {P,S} =
        new{P,S}(parser,sequence,from,till)
end
result_type(::Type{<:MatchesIterator{P}}) where P = result_type(P)
Base.eltype(T::Type{<:MatchesIterator{P,S}}) where {P,S} =
    ParseMatch{P,S,state_type(P)}
Base.IteratorSize(::Type{<:MatchesIterator}) = Base.SizeUnknown()

export ParseMatch
"""
Wrapper type for [`CombinedParsers.Regexp.SequenceWithCaptures`](@ref), providing
`getindex` and `getproperty` behavior like `RegexMatch`.

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
function ParseMatch(parser,s,start=1,stop=start,state=nothing)
    ParseMatch(MatchesIterator(parser,s),
               start,stop,state)
end
result_type(::Type{<:ParseMatch{P}}) where P = result_type(P)
Base.eltype(T::Type{<:ParseMatch}) = ParseMatch
Base.IteratorSize(::Type{<:ParseMatch}) = Base.SizeUnknown()
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

function Base.show(io::IO,m::ParseMatch{<:Any,<:AbstractString,<:Any})
    print(io,"ParseMatch(\"",escape_string(m.match),"\"")
    print(io,")")
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

function Base.show(io::IO,m::ParseMatch{<:Any,<:AbstractString,Nothing})
    print(io,"no match")
end
function Base.show(io::IO,m::ParseMatch{<:Any,<:Any,Nothing})
    print(io,"no match")
end

Base.getproperty(x::ParseMatch{<:Any,<:AbstractString,Nothing},key::Symbol) = 
    if key==:captures
        AbstractString[ ]
    elseif key==:match
        ""
    else
        _getproperty(x,key)
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

"""
    iterate(m::CombinedParser,s::AbstractString)

Returns `iterate(ParseMatch(m,s,1,1,nothing))`.

See also [`ParseMatch`](@ref).
"""
Base.iterate(m::CombinedParser,s::AbstractString) =
    iterate(ParseMatch(m,s,1,1,nothing))
import Base: iterate
"""
    Base.iterate(x::ParseMatch[, m::ParseMatch=x])

Returns next match at `m.start` after `m.state`, see [`_iterate`](@ref)(m).

See also [`ParseMatch`](@ref).
"""
function Base.iterate(x::ParseMatch, m=x)
    i = _iterate(m)
    parsematch_tuple(m.parsings,m.start,i)
end
_iterate(m::ParseMatch) =
    _iterate(m.parsings,m.start,m.stop,m.state)
export match_all

"""
    match_all(parser::ParserTypes, sequence, idx=1; log=nothing)

Returns an iterator over all matches at all indices in the sequence.
"""
function match_all(parser::ParserTypes, sequence, idx=1; log=nothing)
    p = (log === nothing || log == false ) ? parser : log_names(parser,log)
    MatchesIterator(p,sequence,idx)
end

import Base: iterate
"""
    Base.iterate(x::MatchesIterator[, s::ParseMatch=ParseMatch(x,x.from,x.from,nothing)])

Iterate match `s` at current position.
While no match is found and `s.start<=x.till`, increase s.start.
Return first next [`ParseMatch`](@ref) (as return value and state) or `nothing` when at `m.till`.
"""
@inline Base.iterate(x::MatchesIterator) =
    iterate(x,ParseMatch(x,x.from,x.from,nothing))
function Base.iterate(m::MatchesIterator,
                      s::ParseMatch)
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

_iterate(p::ParserTypes,s) =
    _iterate(MatchesIterator(p,s),1,1,nothing)
_iterate(mi::MatchesIterator,a...) =
    _iterate(mi.parser, mi.sequence, mi.till, a...)

"""
    Base.match(parser::ParserTypes,sequence::AbstractString[, idx::Integer]; log=nothing)

Plug-in replacement for `match(::Regex,sequence)`.

If `log!==nothing`, parser is transformed with `log_names(p, log)`.
See also [`log_names`](@ref).
"""
function Base.match(parser::ParserTypes, sequence, idx=1; log=nothing)
    p = (log === nothing || log == false ) ? parser : log_names(parser,log)
    i = iterate(MatchesIterator(p,sequence,idx))
    i === nothing && return nothing
    i[1]
end

import Base: tryparse, parse
"""
    parse(parser::ParserTypes, str::AbstractString; log=nothing)

Parse a string with a CombinedParser as an instance of `result_type(parser)`.

If `log` is a `Vector{Symbol}`, parser is transformed with `log_names(p, log)`.
See also [`log_names`](@ref).

```@meta
DocTestFilters = r"map\\(.+\\)"
```

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
function Base.parse(p::AbstractToken, s; log=nothing)
    i = tryparse(log !== nothing ? log_names(p,log) : p,s)
    i === nothing && throw(ArgumentError("no successfull parsing."))
    i
end

"""
    tryparse(parser::ParserTypes, str::AbstractString[, idx=1])

Like `parse`, but returns either a value of `result_type(parser)` or `nothing` if string does not start with with a match.
"""
function Base.tryparse(p::AbstractToken, s, idx=1)
    i = iterate(ParseMatch(p,s,idx))
    i === nothing && return nothing
    get(tuple_state(i))
end

export tryparse_pos
"""
    tryparse_pos(parser::ParserTypes, str::AbstractString[, idx=1, till=lastindex(s)])

Like `parse`, but returns either a tuple of `result_type(parser)` and the position after the match, or `nothing` if string does not start with with a match.
"""
function tryparse_pos(p,s, idx=1, till=lastindex(s))
    i = _iterate(p,s,till,idx,idx,nothing)
    i === nothing && return nothing
    get(p,s,lastindex(s),tuple_pos(i),1,tuple_state(i)),tuple_pos(i)
end

export parse_all
"""
    parse_all(parser::ParserTypes, sequence, idx=1)

Returns an iterator over all parsings of the sequence starting at `idx`.
"""
function parse_all(parser::ParserTypes, sequence, idx=1)
    ( get(p) for p=ParseMatch(parser,sequence,idx) )
end


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

Base.get(x::MatchesIterator,a...)=
    get(x.parser,x.sequence,x.till,a...)




