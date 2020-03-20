export revert
revert(x) = map_parser(revert,x)

revert(x::String) = Reverse(x)

export Reverse
struct Reverse{V}
    x::V
    lastindex::Int
    Reverse(x) =
        if lastindex(x) == 1
            x
        else
            new{typeof(x)}(x,lastindex(x))
        end
end

reverse_index(x::Reverse,i) =
    x.lastindex-i+1    
Base.firstindex(x::Reverse) = 1
Base.lastindex(x::Reverse) = x.lastindex
Base.getindex(x::Reverse,is::UnitRange) = Reverse(getindex(x.x,reverse_index(x,is.start):reverse_index(x,is.stop)))
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
struct PositiveLookbehind{P} <: LookAround
    parser::P
    PositiveLookbehind(p) =
        new{typeof(p)}(p)
end
# result_type(p::Type{PositiveLookbehind{T}}) where T = T

export NegativeLookbehind
"""
wraps a `parser::P`, succeeds if and only if `parser` does not succeed, but consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct NegativeLookbehind{P} <: LookAround
    parser::P
    NegativeLookbehind(p) =
        new{typeof(p)}(p)
end

export look_behind
function look_behind(match::Bool, p_)
    p = revert(parser(p_))
    if match
        PositiveLookbehind(p)
    else
        NegativeLookbehind(p)
    end
end

function _iterate(t::NegativeLookbehind, str, till, i, state)
    if state === nothing
        rseq=Reverse(str)
        r = _iterate(t.parser, rseq, till,
                     nextind(rseq,reverse_index(rseq,i)), nothing)
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
        rseq=Reverse(str)
        r = _iterate(t.parser, rseq, till,
                     nextind(rseq,reverse_index(rseq,i)), nothing)
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
         map_parser(f::Function,x_::$T,a...) =
         let x = map_parser(f,x_.parser,a...)
         $T(x)
         end
         end)
end

revert(x::Union{AnyChar,CharIn,CharNotIn,UnicodeClass,Always,ConstantParser{N,Char} where N}) = x

map_parser(::typeof(revert),x::Sequence) = seq(( map_parser(revert,p) for p in reverse(x.parts) )...)
