using Tries 
function Either(x::Vector{<:AbstractString})
    P = Trie{Char,Union{Missing,Nothing}}
    r = P()
    for e in x
        r[e...] = nothing
    end
    Either{String}(r)
end

function Either(x::Dict)
    P = Trie{Char,Union{Missing,valtype(x)}}
    r=P()
    for (e,v) in pairs(x)
        r[e...] = v
    end
    Either{valtype(x)}(r)
end
either_state_type(T::Type{<:Trie}) = NCodeunitsState{T}

"""
    _iterate(p::Trie{Char}, str, till, posi, next_i, ::Nothing)

Match char path in `p` greedily, recording shorter matches in state.
"""
@inline function _iterate(p::Union{Trie{Char},SubTrie{Char}}, str, till, posi, next_i, state::Nothing)
    ni = ni_ = posi
    st = st_ = p
    while st !== nothing && ni <= till
        @inbounds c, ni = iterate(str,ni)
        @inbounds st = get( Tries.nodes(st),c, nothing)
        st === nothing && break
        if get(st) !== missing
            ni_ = ni
            st_ = st
        end
    end
    if get(st_) !== missing
        return ni_, NCodeunitsState(ni_-posi,st_)
    else
        return nothing
    end
end

@inline function _iterate(p::AbstractTrie{Char}, str, till, posi, next_i, state)
    _iterate(p, str, _prevind(str,next_i,2), posi, posi, nothing)
end

function _iterate(p::Either{<:AbstractTrie}, str, till, posi, next_i, state)
    _iterate(p.options, str, till, posi, next_i, state)
end

@inline _nextind(str,i::Int,parser::Either{<:AbstractTrie},x::NCodeunitsState)  = i+x.nc
@inline _prevind(str,i::Int,parser::Either{<:AbstractTrie},x::NCodeunitsState)  = i-x.nc

children(x::Either{<:AbstractTrie}) =
    children(x.options)

function deepmap_parser(f::typeof(lowercase),mem::AbstractDict,x::Either{<:AbstractTrie},a...;kw...)
    get!(mem,x) do
        g = (lowercase.(Tries.path(st))=>get(st)
             for st in PreOrderDFS(x.options)
             if !isempty(Tries.path(st)))
        Either{result_type(x)}(Trie(g))
    end
end


# struct InternedParser{E, I<:Either{<:AbstractTrie}} <: WrappedParser{E,NCodeunitsState,T} where {P}
#     interned::I
#     parser::E
# end
