export revert

revert(x::Tuple) = reverse(x)
revert(x::String) = Reverse(x)

export Reverse
@auto_hash_equals struct Reverse{V}<:AbstractString
    x::V
    lastindex::Int
    Reverse(x) =
        # if lastindex(x) == 1
        #     x
        # else
        new{typeof(x)}(x, lastindex(x))
        ##end
end
revert(x::Reverse) = x.x

Base.SubString(x::Reverse,start::Int,stop::Int) =
    SubString(x.x, reverse_index(x, stop), reverse_index(x, start))

function set_capture(sequence::Reverse, index::Int, x)
    @warn "check"
    set_capture(sequence.x,index,x)
end

regex_string(x::Reverse) = regex_escape(x.x)
Base.ncodeunits(x::Reverse) = ncodeunits(x.x)
Base.firstindex(x::Reverse) = 1
Base.lastindex(x::Reverse) = x.lastindex
Base.getindex(x::Reverse,is::UnitRange{<:Integer}) =
    getindex(x.x,reverse_index(x,is.stop):reverse_index(x,is.start))
Base.getindex(x::Reverse,i::Int) =
    getindex(x.x,reverse_index(x,i))

"""
    reverse_index(x::Reverse,i)

Return corresponding index in unreversed String `x.lastindex-i+1`.
Cap at `0:lastindex+1`.
(can be optimized maybe)
"""
function reverse_index(x::Reverse,i)
    ri = x.lastindex-i+1
    if ri > x.lastindex
        x.lastindex + 1
    elseif ri<0 
        0
    else
        ri
    end
end

reverse_index(x::AbstractString,i) =
    i

function Base.nextind(x::Reverse,i::Int)
    ri = reverse_index(x, i)
    reverse_index(x, prevind(x.x, ri))
end
function Base.nextind(x::Reverse,i::Int,n::Int)
    ri = reverse_index(x, i)
    reverse_index(x, prevind(x.x, ri, n))
end
function Base.prevind(x::Reverse,i::Int)
    ri = reverse_index(x, i)
    reverse_index(x, nextind(x.x, ri))
end
function Base.prevind(x::Reverse,i::Int,n::Int)
    ri = reverse_index(x, i)
    reverse_index(x,nextind(x.x, ri, n))
end
Base.iterate(x::Reverse) =
    iterate(x,1)
Base.iterate(x::Reverse,i::Int) =
    if reverse_index(x,i) >= 1 && reverse_index(x,i) <= lastindex(x) # ? <=
        x[i], nextind(x,i)
    else
        nothing
    end

export PositiveLookbehind
"""
    PositiveLookbehind(parser)

Parser that succeeds if and only if `parser` succeeds **before cursor**. Consumes no input.
The match is returned.
Useful for checks like "must be preceded by `parser`, don't consume its match".
"""
@auto_hash_equals struct PositiveLookbehind{T,P} <: LookAround{T}
    parser::P
    function PositiveLookbehind(p_,revert_parser=true)
        p = revert_parser ? deepmap_parser(revert,IdDict(),parser(p_)) : parser(p_)
        new{result_type(p),typeof(p)}(p)
    end
end
# result_type(p::Type{PositiveLookbehind{T}}) where T = T
regex_prefix(x::PositiveLookbehind) = "(?<="

export NegativeLookbehind
"""
    NegativeLookbehind(parser)

Parser that succeeds if and only if `parser` does not succeed **before cursor**.  Consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be preceded by `parser`, don't consume its match".

```jldoctest
julia> la=NegativeLookbehind("keep")
re"(?<!keep)"

julia> parse("peek"*la,"peek")
("peek", re"(?<!keep)")
```
"""
@auto_hash_equals struct NegativeLookbehind{P} <: LookAround{NegativeLookbehind{P}}
    parser::P
    function NegativeLookbehind(p_,revert_parser=true)
        p = revert_parser ? deepmap_parser(revert,IdDict(),parser(p_)) : parser(p_)
        new{typeof(p)}(p)
    end
end
regex_prefix(x::NegativeLookbehind) = "(?<!"

export Lookbehind
"""
    Lookbehind(does_match::Bool, p)

[`PositiveLookbehind`](@ref) if `does_match==true`, 
[`NegativeLookbehind`](@ref) otherwise.
"""
function Lookbehind(does_match::Bool, p)
    if does_match
        PositiveLookbehind(p)
    else
        NegativeLookbehind(p)
    end
end
@deprecate look_behind(does_match,p) Lookbehind(does_match, p)

function _iterate(t::NegativeLookbehind, str, till, posi, next_i, state::Nothing)
    rseq=revert(str)
    next_i < 1 && return next_i, MatchState()
    r = _iterate(t.parser, rseq, till,
                 reverse_index(rseq,prevind(str,next_i)), nothing)
    if r === nothing
        next_i,MatchState()
    else
        nothing
    end
end


_iterate(t::PositiveLookbehind, str, till, posi, next_i, state::MatchState) =
    nothing

function _iterate(t::PositiveLookbehind, str, till, posi, next_i, state::Nothing)
    rseq=revert(str)
    next_i < 1 && return nothing
    r = _iterate(t.parser, rseq, till,
                 reverse_index(rseq,prevind(rseq,next_i)), nothing)
    if r === nothing
        nothing
    else
        next_i,MatchState()
    end
end


regex_inner(x::Union{PositiveLookbehind,NegativeLookbehind}) =
    regex_inner(revert(x.parser))

children(x::Union{PositiveLookbehind,NegativeLookbehind}) =
    reverse(children(x.parser))

for T in [PositiveLookahead,NegativeLookahead,PositiveLookbehind,NegativeLookbehind]
    eval(quote
         deepmap_parser(f::Function,mem::AbstractDict,x_::$T,a...; kw...) =
         let x = deepmap_parser(f,mem,x_.parser,a...; kw...)
         $T(x,false)
         end
         end)
end
revert(x) = deepmap_parser(revert,x)
revert(x::Union{AnyChar,CharIn,CharNotIn,UnicodeClass,Always,Never,ConstantParser{N,Char} where N}) = x
revert(x::AtStart) = AtEnd()
revert(x::AtEnd) = AtStart()
deepmap_parser(::typeof(revert),mem::AbstractDict,x::Sequence) =
    get!(mem,x) do
        Sequence(( deepmap_parser(revert,mem,p) for p in reverse(x.parts) )...)
    end
deepmap_parser(::typeof(revert),mem::AbstractDict,x::Atomic) =
    get!(mem,x) do
        Atomic(deepmap_parser(revert,mem,x.parser))
    end
deepmap_parser(::typeof(revert),mem::AbstractDict,x::NegativeLookbehind) =
    get!(mem,x) do
        NegativeLookahead(x.parser) ##deepmap_parser(revert,@show x.parser))
    end
deepmap_parser(::typeof(revert),mem::AbstractDict,x::NegativeLookahead) =
    get!(mem,x) do
        NegativeLookbehind(x.parser) ##deepmap_parser(revert,x.parser))
    end
deepmap_parser(::typeof(revert),mem::AbstractDict,x::PositiveLookbehind) =
    get!(mem,x) do
        PositiveLookahead(x.parser) ##deepmap_parser(revert,x.parser))
    end
deepmap_parser(::typeof(revert),mem::AbstractDict,x::PositiveLookahead) =
    get!(mem,x) do
        PositiveLookbehind(x.parser) ##deepmap_parser(revert,x.parser))
    end
