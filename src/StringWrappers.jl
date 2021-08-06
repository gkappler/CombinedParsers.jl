"""
    abstract type StringWrapper <: AbstractString end

Provides default `sw::StringWrapper<:AbstractString` API methods defering to `wrappedstring(sw)` call, by default `sw.x`.
"""
abstract type StringWrapper <: AbstractString end
@inline wrappedstring(sw::StringWrapper) = sw.x
@inline Base.@propagate_inbounds Base.getindex(sw::StringWrapper,i::Integer) =
    getindex(wrappedstring(sw),i)
@inline Base.@propagate_inbounds Base.iterate(sw::StringWrapper) =
    iterate(wrappedstring(sw))
@inline Base.@propagate_inbounds Base.iterate(sw::StringWrapper,i::Integer) =
    iterate(wrappedstring(sw),i)
@inline Base.@propagate_inbounds Base.SubString(sw::StringWrapper,start::Int,stop::Int) =
    SubString(wrappedstring(sw),start,stop)
@inline Base.@propagate_inbounds Base.length(sw::StringWrapper) =
    length(wrappedstring(sw))
@inline Base.@propagate_inbounds Base.lastindex(sw::StringWrapper) =
    lastindex(wrappedstring(sw))
@inline Base.@propagate_inbounds Base.firstindex(sw::StringWrapper) =
    firstindex(wrappedstring(sw))
@inline Base.@propagate_inbounds Base.prevind(sw::StringWrapper,i::Int,n::Int) =
    prevind(wrappedstring(sw),i,n)
@inline Base.@propagate_inbounds Base.nextind(sw::StringWrapper,i::Int,n::Int) =
    nextind(wrappedstring(sw),i,n)
@inline Base.@propagate_inbounds Base.prevind(sw::StringWrapper,i::Int) =
    prevind(wrappedstring(sw),i)
@inline Base.@propagate_inbounds Base.nextind(sw::StringWrapper,i::Int) =
    nextind(wrappedstring(sw),i)
@inline Base.@propagate_inbounds Base.ncodeunits(sw::StringWrapper) =
    ncodeunits(wrappedstring(sw))
@inline Base.@propagate_inbounds Base.codeunit(s::StringWrapper, i::Integer) =
    codeunit(wrappedstring(sw), i)
@inline Base.@propagate_inbounds Base.isvalid(sw::StringWrapper,i::Int) =
    isvalid(wrappedstring(sw), i)
@inline Base.@propagate_inbounds Base.thisind(sw::StringWrapper,i::Int) =
    thisind(wrappedstring(sw), i)


