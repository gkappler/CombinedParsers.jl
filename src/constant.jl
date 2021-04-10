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
@auto_hash_equals struct ConstantParser{P,N,T} <: NIndexParser{N, T}
    parser::P
    function ConstantParser(x::T) where {T<:AbstractString}
        new{T,_ncodeunits(x),SubString}(x)
    end
    function ConstantParser(x)
        new{typeof(x),_ncodeunits(x),typeof(x)}(x)
    end
end
_ncodeunits(x::Type{<:ConstantParser{<:Any,N}}) where N = N
_ncodeunits(x::ConstantParser) = _ncodeunits(typeof(x))

@inline _nextind(str,i::Int,parser::ConstantParser,x) =
    i+_ncodeunits(parser)
@inline _prevind(str,i::Int,parser::ConstantParser,x) = 
    i-_ncodeunits(parser)

children(x::ConstantParser) = ()
regex_prefix(x::ConstantParser) = ""
print_constructor(io::IO,x::ConstantParser) = print(io,"")
# caveat
regex_string(x) = "$x::$(typeof(x))"
regex_inner(x::ConstantParser) = regex_string(x.parser)
regex_suffix(x::ConstantParser) = ""


reversed(x::ConstantParser{Char}) = x

reversed(x::ConstantParser{<:AbstractString}) =
    ConstantParser(reverse(x.parser))

lowercase(x::ConstantParser) = ConstantParser(lowercase(x.parser))

deepmap_parser(f::Function,mem::AbstractDict,x::ConstantParser,a...;kw...) =
    get!(mem,x) do
        f(x,a...;kw...)
    end

@inline function _iterate(parser::ConstantParser, sequence, till, posi, next_i, state::Nothing)
    _iterate_constant(parser,sequence,till,posi, next_i, state)
end

@inline function _iterate(p::ConstantParser, sequence, till, posi, next_i, state::MatchState)
    nothing
end

@inline function _iterate_constant(parser::ConstantParser, sequence, till, posi, next_i, state)
    _iterate_constant(parser.parser,sequence,till,posi, next_i, state, _ncodeunits(parser))
end

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
