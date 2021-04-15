
export MemoizingParser
@auto_hash_equals struct MemoizingParser{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    function MemoizingParser(p)
        new{typeof(p),state_type(p),result_type(p)}(p)
    end
end

@inline function _iterate(parser::MemoizingParser, sequence::String, till, posi,after,state)
    error("for memoizing, wrap sequence in WithMemory. Todo: automize wrapping in root parser with optimize")
    _iterate(parser.parser, sequence, till,posi,after,state)
end

deepmap_parser(f::Function,mem::AbstractDict,x::MemoizingParser,a...;kw...) =
    get!(mem,x) do
        MemoizingParser(deepmap_parser(f,mem,x.parser,a...;kw...))
    end


export WithMemory
"""
    WithMemory(x) <: AbstractString

String wrapper with memoization of next match states for parsers at indices.
Memoization is sometimes recommended as a way of improving the performance of parser combinators (like state machine optimization and compilation for regular languages).


!!! note
    A snappy performance gain could not be demonstrated so far, probably because the costs of state memory allocation for caching are often greater than recomputing a match. 
    If you have a case where your performance benefits with this, let me know!
```
"""
struct WithMemory{S,M} <: AbstractString
    x::S
    mem::M
    function WithMemory(x::S,mem::M) where {S,M}
        new{S,M}(x,mem)
    end
end
function WithMemory(x)
    WithMemory(x,Dict())
end
Base.show(io::IO, x::WithMemory) =
    print(io,x.x)

@inline Base.@propagate_inbounds function _iterate(parser::MemoizingParser, sequence::WithMemory, till, posi,after,state)
    get!(sequence.mem,(parser.parser,posi,state)) do
        _iterate(parser.parser, sequence,till,posi,after,state)
    end
end

@inline Base.@propagate_inbounds Base.getindex(x::WithMemory,i::Integer) =
    getindex(x.x,i)
@inline Base.@propagate_inbounds Base.iterate(x::WithMemory{<:AbstractString}) =
    iterate(x.x)
@inline Base.@propagate_inbounds Base.iterate(x::WithMemory{<:AbstractString},i::Integer) =
    iterate(x.x,i)

@inline Base.@propagate_inbounds Base.SubString(x::WithMemory,start::Int,stop::Int) = SubString(x.x,start,stop)
@inline Base.@propagate_inbounds Base.length(x::WithMemory) = length(x.x)
@inline Base.@propagate_inbounds Base.lastindex(x::WithMemory) = lastindex(x.x)
@inline Base.@propagate_inbounds Base.firstindex(x::WithMemory) = firstindex(x.x)
@inline Base.@propagate_inbounds _prevind(x::WithMemory,i::Int,n::Int) = _prevind(x.x,i,n)
@inline Base.@propagate_inbounds _nextind(x::WithMemory,i::Int,n::Int) = _nextind(x.x,i,n)
@inline Base.@propagate_inbounds _prevind(x::WithMemory,i::Int) = _prevind(x.x,i)
@inline Base.@propagate_inbounds _nextind(x::WithMemory,i::Int) = _nextind(x.x,i)
@inline Base.@propagate_inbounds Base.ncodeunits(x::WithMemory) = ncodeunits(x.x)

