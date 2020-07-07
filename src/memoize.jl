
export MemoizingParser
@auto_hash_equals struct MemoizingParser{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    function MemoizingParser(p)
        new{typeof(p),state_type(p),result_type(p)}(p)
    end
end

@inline function _iterate(parser::M, sequence::String, till, posi,after,state) where {M<:MemoizingParser}
    # @warn "for memoizing, wrap sequence in WithMemory"
    _iterate(parser.parser, sequence,till,posi,after,state)
end

deepmap_parser(f::Function,mem::AbstractDict,x::MemoizingParser,a...;kw...) =
    get!(mem,x) do
        MemoizingParser(deepmap_parser(f,mem,x.parser,a...;kw...))
    end


export WithMemory
"""
A lazy element transformation type (e.g. AbstractString), 
`getindex` wraps elements in `with_options(flags,...)`.

With parsing options

TODO: make flags a transformation function?
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

@inline Base.@propagate_inbounds function _iterate(parser::M, sequence::WithMemory, till, posi,after,state) where {M<:MemoizingParser}
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
@inline Base.@propagate_inbounds Base.prevind(x::WithMemory,i::Int,n::Int) = prevind(x.x,i,n)
@inline Base.@propagate_inbounds Base.nextind(x::WithMemory,i::Int,n::Int) = nextind(x.x,i,n)
@inline Base.@propagate_inbounds Base.prevind(x::WithMemory,i::Int) = prevind(x.x,i)
@inline Base.@propagate_inbounds Base.nextind(x::WithMemory,i::Int) = nextind(x.x,i)
@inline Base.@propagate_inbounds Base.ncodeunits(x::WithMemory) = ncodeunits(x.x)

