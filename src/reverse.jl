export revert
revert(x) = map_parser(revert,x)

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
set_capture(sequence::Reverse, index::Int, x) =
    set_capture(sequence.x,index,x)
reverse_index(x::Reverse,i) =
    x.lastindex-i+1    
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
        rseq=revert(str)
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
map_parser(::typeof(revert),x::NegativeLookbehind) = NegativeLookahead(x.parser) ##map_parser(revert,@show x.parser))
map_parser(::typeof(revert),x::NegativeLookahead) = NegativeLookbehind(map_parser(revert,x.parser))
map_parser(::typeof(revert),x::PositiveLookbehind) = PositiveLookahead(x.parser) ##map_parser(revert,x.parser))
map_parser(::typeof(revert),x::PositiveLookahead) = PositiveLookbehind(map_parser(revert,x.parser))
