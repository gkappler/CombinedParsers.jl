"""
    abstract type StringWrapper <: AbstractString end

Provides default `@inline` AbstractString API methods defering to property `:x`.
"""
abstract type StringWrapper <: AbstractString end
@inline Base.@propagate_inbounds Base.getindex(x::StringWrapper,i::Integer) =
    getindex(x.x,i)
@inline Base.@propagate_inbounds Base.iterate(x::StringWrapper) =
    iterate(x.x)
@inline Base.@propagate_inbounds Base.iterate(x::StringWrapper,i::Integer) =
    iterate(x.x,i)
@inline Base.@propagate_inbounds Base.SubString(x::StringWrapper,start::Int,stop::Int) =
    SubString(x.x,start,stop)
@inline Base.@propagate_inbounds Base.length(x::StringWrapper) =
    length(x.x)
@inline Base.@propagate_inbounds Base.lastindex(x::StringWrapper) =
    lastindex(x.x)
@inline Base.@propagate_inbounds Base.firstindex(x::StringWrapper) =
    firstindex(x.x)
@inline Base.@propagate_inbounds Base.prevind(x::StringWrapper,i::Int,n::Int) =
    prevind(x.x,i,n)
@inline Base.@propagate_inbounds Base.nextind(x::StringWrapper,i::Int,n::Int) =
    nextind(x.x,i,n)
@inline Base.@propagate_inbounds Base.prevind(x::StringWrapper,i::Int) =
    prevind(x.x,i)
@inline Base.@propagate_inbounds Base.nextind(x::StringWrapper,i::Int) =
    nextind(x.x,i)
@inline Base.@propagate_inbounds Base.ncodeunits(x::StringWrapper) =
    ncodeunits(x.x)
@inline Base.@propagate_inbounds Base.codeunit(s::StringWrapper, i::Integer) =
    codeunit(x.x, i)
@inline Base.@propagate_inbounds Base.isvalid(x::StringWrapper,i::Int) =
    isvalid(x.x, i)
@inline Base.@propagate_inbounds Base.thisind(x::StringWrapper,i::Int) =
    thisind(x.x, i)


