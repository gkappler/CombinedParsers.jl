"""
Wrapper for stepping with ncodeunit length.

```jldoctest
julia> parser("constant") isa CombinedParsers.ConstantParser
true

julia> parser('c') isa CombinedParsers.ConstantParser
true

julia> parser(1) isa CombinedParsers.ConstantParser
true
```
"""
@auto_hash_equals struct ConstantParser{P} <: LeafParser
    parser::P
    function ConstantParser(x)
        new{typeof(x)}(x)
    end
end
@inline state_type(::Type{<:CombinedParsers.ConstantParser}) =
    MatchState

result_type(p::ConstantParser{P}, sequence; kw...) where P =
    P


@inline _ncodeunits(x::Union{Char,AbstractString}) = ncodeunits(x)
@inline _ncodeunits(x) = 1
@inline _ncodeunits(x::ConstantParser) = _ncodeunits(x.parser)
@inline _rightof(str,i,parser::ConstantParser,x) =
    i+_ncodeunits(parser)
@inline _leftof(str,i,parser::ConstantParser,x) = 
    i-_ncodeunits(parser)

_lowercase(x::CombinedParser) = x

_lowercase(x::ConstantParser) = ConstantParser(lowercase(x.parser))

@inline function iterate_state(parser::ConstantParser, sequence, till, posi, next_i, state::Nothing)
    j,s = iterate_state_constant(parser,sequence,till,posi, next_i, state)
    s === nothing ? nothing : (j,s)
end

@inline iterate_state_constant(parser::ConstantParser, sequence, till, posi, next_i, state) =
    iterate_state_constant(parser.parser,sequence,till,posi, next_i, state, _ncodeunits(parser))

@inline iterate_state_constant(p::AbstractChar, sequence, till, posi, next_i, state,L) = next_i, nothing
@inline function iterate_state_constant(p::AbstractChar, sequence, till, posi, next_i, state::Nothing,L)
    # till, posi, next_i
    j::Int = next_i
        (j > till) && return j, nothing
        @inbounds sc=sequence[j]
        j_last = j
        j = _nextind(sequence,j)
        !ismatch(sc,p) && return j_last, nothing
    return j, MatchState()
end
@inline iterate_state_constant(p::AbstractString, sequence, till, posi, next_i, state,L) = next_i, nothing
@inline function iterate_state_constant(p::AbstractString, sequence, till, posi, next_i, state::Nothing,L)
    # till, posi, next_i
    j::Int = next_i
    k::Int = 1
    while k<=L
        (j > till) && return j, nothing
        @inbounds pc=p[k]
        k=_nextind(p,k)
        @inbounds sc=sequence[j]
        j_last = j
        j = _nextind(sequence,j)
        !ismatch(sc,pc) && return j_last, nothing
    end
    return j, MatchState()
end

@inline function iterate_state_constant(parser, sequence, till, posi, next_i, state::Nothing, L)
    state !== nothing || next_i>till || next_i < 1 && return nothing
    if next_i<=till && ismatch(sequence[next_i],parser)
        next_i+L, MatchState()
    else
        posi, nothing
    end
end

