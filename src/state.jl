
"""
    tuple_pos(pos_state::Tuple)

`pos_state[1]` is position after match in tuple returned by [`_iterate`](@ref).
"""
@inline tuple_pos(pos_state::Tuple) =
    pos_state[1]

"""
    tuple_state(pos_state::Tuple)

`pos_state[2]` is state of match in tuple returned by [`_iterate`](@ref).
"""
@inline tuple_state(pos_state::Tuple) =
    pos_state[2]

export MatchState
"""
State object for a match that is defined by the parser, sequence and position.
"""
struct MatchState end
Base.show(io::IO, ::MatchState) = print(io,"∘")

"""
    CombinedParsers.state_type(x::T)

Return the state type of `x`
"""
@inline state_type(x::Union{ParserTypes,AbstractToken}) = state_type(typeof(x))
@inline state_type(x::Type{<:Union{Char,AbstractString}}) = MatchState
@inline state_type(::Type{Any}) = Any
@inline function state_type(T::Type{<:AbstractToken{P}}) where P
    T <: CombinedParser && error("define state_type(::Type{$T})")
    NCodeunitsState{P}
end


"""
State object representing ncodeunits explicitely with state of match for `prevind`, `nextind` to improve performance.
    nc::Int
    state::S

See also [`MatchState`](@ref), [`prevind`](@ref), [`nextind`](@ref).
"""
struct NCodeunitsState{S}
    nc::Int
    state::S
end
NCodeunitsState(posi::Int,after::Int,state) =
    after, NCodeunitsState(after-posi,state)
@inline NCodeunitsState{S}(posi::Int,after::Int,state) where S =
    after, NCodeunitsState{S}(after-posi,state)
Base.convert(::Type{NCodeunitsState{T}}, x::NCodeunitsState{S}) where {S,T} =
    NCodeunitsState{T}(x.nc, convert(T,x.state))

@inline function _nextind(str,i::Int,parser,x::NCodeunitsState)
    i+x.nc
end

@inline function _prevind(str,i::Int,parser,x::NCodeunitsState)
    i-x.nc
end

# @inline _nextind(str,i::Int,parser::W,x::NCodeunitsState) where {W <: WrappedParser} = i+x.nc
# @inline _prevind(str,i::Int,parser::W,x::NCodeunitsState) where {W <: WrappedParser} = i-x.nc

