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
@auto_hash_equals struct ConstantParser{P,T} <: LeafParser{MatchState,T}
    parser::P
    function ConstantParser(x::T) where {T<:AbstractString}
        new{T,SubString{T}}(x)
    end
    function ConstantParser(x)
        new{typeof(x),typeof(x)}(x)
    end
end
@inline _ncodeunits(x::Union{Char,AbstractString}) = ncodeunits(x)
@inline _ncodeunits(x) = 1
@inline _ncodeunits(x::ConstantParser) = _ncodeunits(x.parser)

@inline _rightof(str,i,parser::ConstantParser,x) =
    i+_ncodeunits(parser)
@inline _leftof(str,i,parser::ConstantParser,x) = 
    i-_ncodeunits(parser)

children(x::ConstantParser) = ()
regex_prefix(x::ConstantParser) = ""
print_constructor(io::IO,x::ConstantParser) = print(io,"")
# caveat
regex_inner(x::ConstantParser) = regex_string(x.parser)
regex_suffix(x::ConstantParser) = ""

_lowercase(x::CombinedParser) = x

_lowercase(x::ConstantParser) = ConstantParser(lowercase(x.parser))

@inline _iterate(parser::ConstantParser, sequence, till, posi, next_i, state::Nothing) =
    _iterate_constant(parser,sequence,till,posi, next_i, state)

@inline _iterate_constant(parser::ConstantParser, sequence, till, posi, next_i, state) =
    _iterate_constant(parser.parser,sequence,till,posi, next_i, state, _ncodeunits(parser))

@inline function _iterate_constant(p::AbstractString, sequence, till, posi, next_i, state::Nothing,L)
    till, posi, next_i
    j::Int = next_i
    k::Int = 1
    while k<=L
        (j > till) && return nothing
        @inbounds pc=p[k]
        k=_nextind(p,k)
        @inbounds sc=sequence[j]
        j=_nextind(sequence,j)
        !ismatch(sc,pc) && return nothing
    end
    return j, MatchState()
end

@inline function _iterate_constant(parser, sequence, till, posi, next_i, state::Nothing, L)
    state !== nothing || next_i>till || next_i < 1 && return nothing
    if next_i<=till && ismatch(sequence[next_i],parser)
        next_i+L, MatchState()
    else
        nothing
    end
end

