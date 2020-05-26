export revert

revert(x::String) = Reverse(x)

export Reverse
@auto_hash_equals struct Reverse{V}
    x::V
    lastindex::Int
    Reverse(x) =
        # if lastindex(x) == 1
        #     x
        # else
        new{typeof(x)}(x,lastindex(x))
        ##end
end
revert(x::Reverse) = x.x
reverse_index(x::Reverse,i) =
    x.lastindex-i+1    
reverse_index(x::AbstractString,i) =
    i

Base.SubString(x::Reverse,start,stop) =
    SubString(x.x, reverse_index(x,stop), reverse_index(x,start))

function set_capture(sequence::Reverse, index::Int, x)
    @warn "check"
    set_capture(sequence.x,index,x)
end

Base.firstindex(x::Reverse) = 1
Base.lastindex(x::Reverse) = x.lastindex
Base.getindex(x::Reverse,is::UnitRange) = getindex(x.x,reverse_index(x,is.stop):reverse_index(x,is.start))
Base.getindex(x::Reverse,i) = getindex(x.x,reverse_index(x,i))
Base.nextind(x::Reverse,i::Integer) = reverse_index(x,prevind(x.x,reverse_index(x,i)))
Base.prevind(x::Reverse,i::Integer) = reverse_index(x,nextind(x.x,reverse_index(x,i)))
Base.nextind(x::Reverse,i::Integer,n::Integer) = reverse_index(x,prevind(x.x,reverse_index(x,i),n))
Base.prevind(x::Reverse,i::Integer,n::Integer) = reverse_index(x,nextind(x.x,reverse_index(x,i),n))

export PositiveLookbehind
"""
    PositiveLookbehind(parser)

Parser that succeeds if and only if `parser` succeeds **before cursor**. Consumes no input.
The match is returned.
Useful for checks like "must be preceded by `parser`, don't consume its match".
"""
@auto_hash_equals struct PositiveLookbehind{T,P} <: LookAround{T}
    parser::P
    PositiveLookbehind(p_) =
        let p = parser(p_)
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
re"(?<!peek)"

julia> parse("peek"*la,"peek")
("peek", 'p')
```
"""
@auto_hash_equals struct NegativeLookbehind{P} <: LookAround{NegativeLookbehind{P}}
    parser::P
    NegativeLookbehind(p_) =
        let p = parser(p_)
        end
        new{typeof(p)}(p)
end
regex_prefix(x::NegativeLookbehind) = "(?<!"

export Lookbehind
function Lookbehind(does_match::Bool, p_)
    p = deepmap_parser(revert,IdDict(),parser(p_))
    if does_match
        PositiveLookbehind(p)
    else
        NegativeLookbehind(p)
    end
end
@deprecate look_behind(does_match,p) Lookbehind(does_match, p)

function _iterate(t::NegativeLookbehind, str, till, i, state)
    if state === nothing
        rseq=revert(str)
        i < 1 && return i, tuple()
        r = _iterate(t.parser, rseq, till,
                     reverse_index(rseq,prevind(str,i)), nothing)
        if r === nothing
            i,tuple()
        else
            nothing
        end
    elseif state === tuple()
        nothing
    else
        i, tuple()
    end
end

function Base.get(parser::NegativeLookbehind, sequence, till, after, i, state)
    parser
end

function _iterate(t::PositiveLookbehind, str, till, i, state)
    if state === nothing
        rseq=revert(str)
        i < 1 && return nothing
        r = _iterate(t.parser, rseq, till,
                     reverse_index(rseq,prevind(rseq,i)), nothing)
        if r === nothing
            nothing
        else
            i,tuple()
        end
    else
        nothing
    end
end

function Base.get(t::PositiveLookbehind, str, till, after, i, state)
    rseq = revert(str)
    get(t.parser, rseq, till, after, reverse_index(rseq,prevind(rseq,i)), state)
end
for T in [PositiveLookahead,NegativeLookahead,PositiveLookbehind,NegativeLookbehind]
    eval(quote
         deepmap_parser(f::Function,mem::AbstractDict,x_::$T,a...; kw...) =
         let x = deepmap_parser(f,mem,x_.parser,a...; kw...)
         $T(x)
         end
         end)
end

revert(x::Union{AnyChar,CharIn,CharNotIn,UnicodeClass,Always,Never,ConstantParser{N,Char} where N}) = x
revert(x::AtStart) = AtEnd()
revert(x::AtEnd) = AtStart()
deepmap_parser(::typeof(revert),mem::AbstractDict,x::Sequence) =
    get!(mem,x) do
        Sequence(( deepmap_parser(revert,mem,p) for p in reverse(x.parts) )...)
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
