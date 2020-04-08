export revert

revert(x::String) = Reverse(x)

export Reverse
struct Reverse{V}
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
wraps a `parser::P`, succeeds if and only if `parser` succeeds, but consumes no input.
The match is returned.
Useful for checks like "must be followed by `parser`, but don't consume its match".
"""
struct PositiveLookbehind{T,P} <: LookAround{T}
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
wraps a `parser::P`, succeeds if and only if `parser` does not succeed, but consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct NegativeLookbehind{T,P} <: LookAround{T}
    parser::P
    NegativeLookbehind(p_) =
        let p = parser(p_)
            new{result_type(p),typeof(p)}(p)
        end
end
regex_prefix(x::NegativeLookbehind) = "(?<!"

export look_behind
function look_behind(match::Bool, p_)
    p = map_parser(revert,IdDict(),parser(p_))
    if match
        PositiveLookbehind(p)
    else
        NegativeLookbehind(p)
    end
end

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

for T in [PositiveLookahead,NegativeLookahead,PositiveLookbehind,NegativeLookbehind]
    eval(quote
         map_parser(f::Function,mem::AbstractDict,x_::$T,a...) =
         let x = map_parser(f,mem,x_.parser,a...)
         $T(x)
         end
         end)
end

revert(x::Union{AnyChar,CharIn,CharNotIn,UnicodeClass,Always,Never,ConstantParser{N,Char} where N}) = x
revert(x::AtStart) = AtEnd()
revert(x::AtEnd) = AtStart()
map_parser(::typeof(revert),mem::AbstractDict,x::Sequence) =
    get!(mem,x) do
        Sequence(( map_parser(revert,mem,p) for p in reverse(x.parts) )...)
    end
map_parser(::typeof(revert),mem::AbstractDict,x::NegativeLookbehind) =
    get!(mem,x) do
        NegativeLookahead(x.parser) ##map_parser(revert,@show x.parser))
    end
map_parser(::typeof(revert),mem::AbstractDict,x::NegativeLookahead) =
    get!(mem,x) do
        NegativeLookbehind(x.parser) ##map_parser(revert,x.parser))
    end
map_parser(::typeof(revert),mem::AbstractDict,x::PositiveLookbehind) =
    get!(mem,x) do
        PositiveLookahead(x.parser) ##map_parser(revert,x.parser))
    end
map_parser(::typeof(revert),mem::AbstractDict,x::PositiveLookahead) =
    get!(mem,x) do
        PositiveLookbehind(x.parser) ##map_parser(revert,x.parser))
    end
