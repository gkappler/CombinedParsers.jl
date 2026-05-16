
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
    !Either(r)
end

function Either(x::Dict)
    P = Trie{Char,Union{Missing,valtype(x)}}
    r=P()
    for (e,v) in pairs(x)
        r[e...] = v
    end
    Either(r)
end
either_state_type(T::Type{<:Trie}) = NCodeunitsState{T}

@inline iterate_state(p::Either{<:AbstractTrie}, str, till, posi, next_i, state) =
    iterate_state(p.options, str, till, posi, next_i, state)

"""
    iterate_state(p::AbstractTrie{Char}, str, till, posi, next_i, ::Nothing)

Match char path in `p` greedily, recording `SubTrie` in a [`NCodeunitsState`](@ref).
"""
@inline function iterate_state(p::AbstractTrie{Char}, str, till, posi, next_i, state::Nothing)
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

@inline iterate_state(p::AbstractTrie{Char}, str, till, posi, next_i, state) = 
    iterate_state(p, str, _prevind(str,next_i,2), posi, posi, nothing)

# disambiguation
@inline _rightof(str,i,parser::Either{<:AbstractTrie},x::NCodeunitsState) =
    i+x.nc
@inline _leftof(str,i,parser::Either{<:AbstractTrie},x::NCodeunitsState) =
    i-x.nc

result_type(x::Either{<:AbstractTrie}, sequence; kw...) =
    SubString{typeof(sequence)}


function Base.get(x::Either{<:AbstractTrie},
                  sequence, till, after, i, state)
    li = _prevind(sequence,after)
    li<i ? "" : @inbounds SubString(sequence,i,li)
end


function deepmap_either(f::typeof(_lowercase),mem::AbstractDict,x::Either{<:AbstractTrie},a...;kw...)
    g = (lowercase.(Tries.path(st))=>get(st)
         for st in PreOrderDFS(x.options)
             if !isempty(Tries.path(st)))
    Either(Trie(g))
end

function deepmap_either(f,mem::AbstractDict,x::Either{<:AbstractTrie},a...;kw...)
    x ##?
end

# struct InternedParser{E, I<:Either{<:AbstractTrie}} <: WrappedParser{E,NCodeunitsState,T} where {P}
#     interned::I
#     parser::E
# end
can_collapse(x::AbstractTrie) = false #length(children(x)) <= 1


export flexible_spacing

"""
    flexible_spacing(code::AbstractString)
    flexible_spacing(codes::Vector{<:AbstractString})

Creates a flexible-whitespace `Either` parser utilizing a `Trie{String}`.
It matches text token-by-token (words and single punctuation chars) 
and ignores any inter-token whitespace, perfectly annihilating LLM hallucinations.
"""
function flexible_spacing(codes::Vector{<:AbstractString})
    P = Trie{String, Union{Missing,Nothing}}
    r = P()
    for code in codes
        toks = _tokenize_code(code)
        if isempty(toks)
            # Edge case: Empty string matches. Reconstruct Trie root with valid state.
            r = P(nothing, Tries.nodes(r))
        else
            r[toks...] = nothing
        end
    end
    # Return capturing Either parser wrapping this string-based Trie
    Either(r)
end

flexible_spacing(code::AbstractString) = flexible_spacing([code])

# ∇ --- Tokenizer & State Machine --- ∇

function _tokenize_code(text::AbstractString)
    tokens = String[]
    ni = firstindex(text)
    till = lastindex(text)
    while ni <= till
        c, ni_next = iterate(text, ni)
        if isspace(c)
            ni = ni_next
            continue
        end
        
        start_ni = ni
        if isletter(c) || isnumeric(c) || c == '_'
            ni = ni_next
            while ni <= till
                c, ni_next = iterate(text, ni)
                if isletter(c) || isnumeric(c) || c == '_'
                    ni = ni_next
                else
                    break
                end
            end
            push!(tokens, String(SubString(text, start_ni, prevind(text, ni))))
        else
            # Punctuation boundary
            push!(tokens, String(SubString(text, start_ni, start_ni)))
            ni = ni_next
        end
    end
    return tokens
end

@inline function _next_token(str, ni, till)
    # Skip leading whitespace
    while ni <= till
        c, ni_next = iterate(str, ni)
        if !isspace(c)
            break
        end
        ni = ni_next
    end
    ni > till && return nothing, ni
    
    c, ni_next = iterate(str, ni)
    start_ni = ni
    if isletter(c) || isnumeric(c) || c == '_'
        ni = ni_next
        while ni <= till
            c, ni_next = iterate(str, ni)
            if isletter(c) || isnumeric(c) || c == '_'
                ni = ni_next
            else
                break
            end
        end
        return SubString(str, start_ni, prevind(str, ni)), ni
    else
        return SubString(str, start_ni, start_ni), ni_next
    end
end

"""
    iterate_state(p::AbstractTrie{<:AbstractString}, str, till, posi, next_i, ::Nothing)

Token-based iteration state for Trie. Yields the next non-whitespace chunk from `str`
and checks against `Trie{String}` keys natively.
"""
@inline function iterate_state(p::AbstractTrie{<:AbstractString}, str, till, posi, next_i, state::Nothing)
    # 1. Do not match if we are starting on a space. This delegates
    # advancing the start position to MatchesIterator, preventing leading
    # whitespace from being captured in the match result bounds.
    if posi <= till
        c, _ = iterate(str, posi)
        if isspace(c)
            return nothing
        end
    end

    ni = ni_ = posi
    st = st_ = p
    
    while st !== nothing && ni <= till
        tok, ni_next = _next_token(str, ni, till)
        tok === nothing && break
        
        # O(1) allocation-free dictionary lookup matching SubString against String keys
        st = get(Tries.nodes(st), tok, nothing)
        st === nothing && break
        
        ni = ni_next
        if get(st) !== missing
            ni_ = ni
            st_ = st
        end
    end
    
    if get(st_) !== missing
        # Return state representing successful token match sequence bounds.
        # Trailing whitespace naturally isn't captured by `ni_`.
        return ni_, NCodeunitsState(ni_ - posi, st_)
    else
        return nothing
    end
end
