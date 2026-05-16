import Base: findnext

"""
    Base.findnext(parser::CombinedParser, sequence::AbstractString, idx::Integer)

Seamless integration with Julia's native standard library (e.g., `replace()`). 
Allows passing any `CombinedParser` directly as a matching pattern.
"""
function Base.findnext(parser::CombinedParser, sequence::AbstractString, idx::Integer)
    m = match(parser, sequence, idx)
    m === nothing && return nothing
    # Maps matched ParseMatch bounds to the native UnitRange Julia expects
    return m.offset : prevind(sequence, m.after)
end

"""
    wrap(x::CombinedParser; log = nothing, trace = false)

transform a parser by wrapping sub-parsers in logging and tracing parser types.
"""
function wrap(x::CombinedParser; log = nothing, trace = false)
    1
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

export MatchesIterator, ParseMatch

"""
    MatchesIterator(parser::P, sequence::I[, start=firstindex(sequence)[, stop=lastindex(sequence), [till=lastindex(sequence)]]])

Iterator type for [`match_all`](@ref) and [`parse_all`](@ref) with `eltype` [`ParseMatch`](@ref)`{P,I}`.

Iteration looks for matches beginning between `start` and `stop` and ending at most at `till`.
"""
@auto_hash_equals struct MatchesIterator{P<:CombinedParser,I}
    "parser"
    parser::P
    "sequence"
    sequence::I
    "First index for searching [`match`](@ref)."
    start::Int
    "Last index for searching [`match`](@ref)."
    stop::Int
    "Last index for end of [`match`](@ref)."
    till::Int
end
result_type(x::MatchesIterator,sequence; kw...) =
    result_type(x.parser, sequence; kw...)
Base.eltype(T::Type{<:MatchesIterator{P,I}}) where {P,I} =
    ParseMatch{P,I,state_type(P)}
Base.IteratorSize(::Type{<:MatchesIterator}) =
    Base.SizeUnknown()

@inline iterate_state(mi::MatchesIterator, posi, a...) =
    iterate_state(mi.parser, mi.sequence, mi.till, posi, a...)

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
    ParseMatch(p::MatchesIterator{P}, offset::Integer, after::Integer, state::ST) where {P,ST}

You can extract the following info from a `m::ParseMatch` object 
(like [Julia RegexMatch](https://docs.julialang.org/en/v1/manual/strings/#Regular-Expressions),
):

- the entire substring matched: `m.match`
- the offset at which the whole match begins: `m.offset`

If `P<:`[`CombinedParsers.Regexp.ParserWithCaptures`](@ref) and `S<:`[`CombinedParsers.Regexp.SequenceWithCaptures`](@ref)

- the captured substrings as an array of strings: `m.captures`
- the offsets of the captured substrings as a vector: `m.offsets`
"""
@auto_hash_equals struct ParseMatch{P,I,State}
    parsings::MatchesIterator{P,I}
    offset::Int
    after::Int
    state::State
    function ParseMatch(p::MatchesIterator{P,I}, offset=p.start, after=p.start, state=nothing) where {P,I}
        new{P, I, typeof(state)}(p, offset, after, _copy(state))
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


result_type(x::ParseMatch, s...; kw...) =
    result_type(x.parsings, s...; kw...)
@inline iterate_state(m::ParseMatch) =
    iterate_state(m.parsings,m.offset,m.after,m.state)

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

Returns next [`ParseMatch`](@ref) at `m.offset` after `m.state`, see [`iterate_state`](@ref)(m).
"""
function Base.iterate(x::ParseMatch, m=x)
    i = iterate_state(m)
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
    state = iterate_state(m,offset,after,s.state)
    while offset <= stop+1 && state===nothing
        # state = iterate(m.parsings,(offset,nothing))
        offset > stop && break
        offset = _nextind(m.sequence,offset)
        state = iterate_state(m,offset,offset,nothing)
    end
    parsematch_tuple(m,offset,state)
end


function Base.show(io::IO, m::ParseMatch{<:Any, <:AbstractString})
    print(io,"ParseMatch(\"",
          m.state === nothing ? "no match" : escape_string(m.match),
          "\"")
    print(io,")")
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
function Base.parse(p::CombinedParser, s, pos...;kw...)
    i = tryparse_pos(p, s, pos...; sentinel = NoMatch(), kw...)
    i === NoMatch() && throw(ArgumentError("no successfull parsing."))
    i[1]
end
function Base.tryparse(p::CombinedParser, s, pos...; sentinel=nothing, kw...)
    i = tryparse_pos(p, s, pos...; sentinel=sentinel, kw...)
    i === sentinel ? sentinel : i[1]
end





function tryparse_pos(p_,sequence, idx=firstindex(sequence), till=lastindex(sequence); trace_pos = nothing, trace=false, log=false, sentinel = nothing, delta =10, io=stdout, kw...)
    p = if log === nothing || log == false
        p_
    else
        log_names(p_,log; io=io)
        
    end
    if trace == true
        tp = p isa Tracer{<:CombinedParser,TracingStat} ? empty_tracer!(p) : tracer(TracingStat,p)
        i = iterate_state(tp,sequence,till,idx,idx,nothing) 
        if i === nothing || tuple_pos(i) <= lastindex(sequence)
            print_tree(io, tp; 
                       trace_pos=trace_pos,
                       printnode_kw=(
                           sequence=sequence,
                           hide=true,
                           delta=delta, kw...))
            printstyled(io,"partly successfull until furthest attempt at", color=:magenta)
            printstyled(io," [$trace_pos].\n", color=:light_red)
        end
        i === nothing && return sentinel
        get(p,sequence,till,tuple_pos(i),1,tuple_state(i)), tuple_pos(i)
        #tree, tuple_pos(i)
    else
        i = iterate_state(p,sequence,till,idx,idx,nothing)
        i === nothing && return sentinel
        get(p,sequence,till,tuple_pos(i),1,tuple_state(i)), tuple_pos(i)
    end
end
function _tryparse_pos(p_::Tracer,sequence, idx=firstindex(sequence), till=lastindex(sequence); delta =10, kw...)
    i = iterate_state(tp,sequence,till,idx,idx,nothing) 
    if i === nothing || tuple_pos(i) <= lastindex(sequence)
        print_tree(io::IO, tp; trace_pos=trace_pos,
                   printnode_kw=(
                       sequence=sequence,
                       hide=true,
                       delta=delta, kw...))
        printstyled(io,"partly successfull until furthest attempt at", color=:magenta)
        printstyled(io," [$trace_pos].\n", color=:light_red)
    end
    i === nothing && return sentinel
    get(p,sequence,till,tuple_pos(i),1,tuple_state(i)), tuple_pos(i)
end

"""
    parse(parser::CombinedParser, sequence[, idx=firstindex(sequence)[, till=lastindex(sequence)]]; log=nothing)

Parse `sequence` with `parser` at start and produce an instance of `result_type(parser)`.
If `log!==nothing`, parser is transformed with [`log_names`](@ref)`(p, log)` before matching.

Throws an `ArgumentError` if parsing is not successful (like `Base.parse`).

    tryparse(parser::CombinedParser, sequence[, idx=firstindex(sequence)[, till=lastindex(sequence)]]; sentinel=nothing, log=nothing)

returns either a result value or `sentinel` if sequence does not start with with a match.
(Consider `sentinel=CombinedParsers.NoMatch()` for parsers what might result in `nothing` after success.)

    tryparse_pos(parser::CombinedParser, str::AbstractString[, idx=firstindex(sequence)[, till=lastindex(s)]]; sentinel=nothing, log=nothing)
returns either a tuple of result value and the position after the match, or `sentinel` if sequence does not start with with a match.

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
