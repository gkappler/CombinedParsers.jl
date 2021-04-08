_ncodeunits(x::Union{Char,AbstractString}) = ncodeunits(x)
_ncodeunits(x) = 1

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
@auto_hash_equals struct ConstantParser{N,P,T} <: NIndexParser{N, T}
    parser::P
    function ConstantParser(x::T) where {T<:AbstractString}
        new{_ncodeunits(x),T,SubString}(x)
    end
    function ConstantParser(x)
        new{_ncodeunits(x),typeof(x),typeof(x)}(x)
    end
end
_ncodeunits(x::ConstantParser{N}) where N = N

children(x::ConstantParser) = ()
regex_prefix(x::ConstantParser) = ""
print_constructor(io::IO,x::ConstantParser) = print(io,"")
# caveat
regex_string(x) = "$x::$(typeof(x))"
regex_inner(x::ConstantParser) = regex_string(x.parser)
regex_suffix(x::ConstantParser) = ""


reversed(x::ConstantParser{N,<:AbstractString}) where N =
    ConstantParser(reverse(x.parser))

lowercase(x::ConstantParser) = ConstantParser(lowercase(x.parser))

deepmap_parser(f::Function,mem::AbstractDict,x::ConstantParser,a...;kw...) =
    get!(mem,x) do
        f(x,a...;kw...)
    end

@inline _nextind(str,i::Int,parser::ConstantParser{L},x) where L =
    i+L
@inline _prevind(str,i::Int,parser::ConstantParser{L},x) where L = 
    i-L

@inline function _iterate(parser::ConstantParser, sequence, till, posi, next_i, state::Nothing)
    _iterate(parser.parser,sequence,till,posi, next_i, nothing)
end

"""
    _iterate(parser, sequence, till, posi, next_i, states)

Note: `next_i` is the index in `sequence` after `parser` match according to `state` (and not the start of the match), 
such that `start_index(sequence,after,parser,state)` returns the start of the matching subsequence,
and sequence[start_index(sequence,after,parser,state):_prevind(sequence,next_i)] is the matched subsequence.
"""
@inline function _iterate(p::AbstractString, sequence, till, posi, next_i, state::Nothing,L=_ncodeunits(p))
    till, posi, next_i
    j::Int = next_i
    k::Int = 1
    1
    while k<=L
        (j > till) && return(nothing)
        @inbounds pc,k=iterate(p,k)
        @inbounds sc,j=iterate(sequence,j)
        !ismatch(sc,pc) && return(nothing)
    end
    return j, MatchState()
end

@inline function _iterate(parser::Char, sequence, till, posi, next_i, state, L=_ncodeunits(parser))
    @assert parser isa Char
    state !== nothing || next_i>till && return nothing
    if next_i<=till && ismatch(sequence[next_i],parser)
        next_i+L, MatchState()
    else
        nothing
    end
end

@inline function _iterate(p::Union{<:AbstractString,Char}, sequence, till, posi, next_i, state)
    nothing
end

