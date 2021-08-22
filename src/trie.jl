using Tries

"""
    Either(x::Vector{<:AbstractString})

Create a fast `Trie{Char,Union{Missing,Nothing}}` parser.

Can Trie values be CombinedParsers that need to match after path?
(missing this is Never(), nothing is Always()
"""
function Either(x::Vector{<:AbstractString})
    P = Trie{Char,Union{Missing,Nothing}}
    r = P()
    for e in x
        r[e...] = nothing
    end
    !Either{NCodeunitsState, Nothing}(r)
end

function Either(x::Dict)
    P = Trie{Char,Union{Missing,valtype(x)}}
    r=P()
    for (e,v) in pairs(x)
        r[e...] = v
    end
    Either{NCodeunitsState, valtype(x)}(r)
end
either_state_type(T::Type{<:Trie}) = NCodeunitsState{T}

@inline _iterate(p::Either{<:AbstractTrie}, str, till, posi, next_i, state) =
    _iterate(p.options, str, till, posi, next_i, state)

"""
    _iterate(p::AbstractTrie{Char}, str, till, posi, next_i, ::Nothing)

Match char path in `p` greedily, recording `SubTrie` in a [`NCodeunitsState`](@ref).
"""
@inline function _iterate(p::AbstractTrie{Char}, str, till, posi, next_i, state::Nothing)
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

@inline _iterate(p::AbstractTrie{Char}, str, till, posi, next_i, state) = 
    _iterate(p, str, _prevind(str,next_i,2), posi, posi, nothing)

# disambiguation
@inline _rightof(str,i,parser::Either{<:AbstractTrie},x::NCodeunitsState) =
    i+x.nc
@inline _leftof(str,i,parser::Either{<:AbstractTrie},x::NCodeunitsState) =
    i-x.nc

children(x::Either{<:AbstractTrie}) =
    children(x.options)

function deepmap_either(f::typeof(_lowercase),mem::AbstractDict,x::Either{<:AbstractTrie},a...;kw...)
    g = (lowercase.(Tries.path(st))=>get(st)
         for st in PreOrderDFS(x.options)
             if !isempty(Tries.path(st)))
    Either{NCodeunitsState, String}(Trie(g))
end

function deepmap_either(f,mem::AbstractDict,x::Either{<:AbstractTrie},a...;kw...)
    x ##?
end

# struct InternedParser{E, I<:Either{<:AbstractTrie}} <: WrappedParser{E,NCodeunitsState,T} where {P}
#     interned::I
#     parser::E
# end
