export reversed

using ReversedStrings
import ReversedStrings: reversed

reversed(x::Tuple) = reverse(x)

function set_capture(sequence::ReversedString, index::Int, x)
    @warn "check"
    set_capture(sequence.x,index,x)
end

## caveat!
regex_string(x::ReversedString) = regex_escape(x.representation)

export PositiveLookbehind
"""
    PositiveLookbehind(parser)

Parser that succeeds if and only if `parser` succeeds **before cursor**. Consumes no input.
The match is returned.
Useful for checks like "must be preceded by `parser`, don't consume its match".
"""
@auto_hash_equals struct PositiveLookbehind{T,P} <: LookAround{T}
    parser::P
    function PositiveLookbehind(p_,reversed_parser=true)
        p = reversed_parser ? deepmap_parser(reversed,IdDict(),parser(p_)) : parser(p_)
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
    function NegativeLookbehind(p_,reversed_parser=true)
        p = reversed_parser ? deepmap_parser(reversed,IdDict(),parser(p_)) : parser(p_)
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
    rseq=reversed(str)
    next_i < 1 && return next_i, MatchState()
    r = _iterate(t.parser, rseq, till,
                 reverse_index(rseq,_prevind(str,next_i)), nothing)
    if r === nothing
        next_i,MatchState()
    else
        nothing
    end
end


_iterate(t::PositiveLookbehind, str, till, posi, next_i, state::MatchState) =
    nothing

function _iterate(t::PositiveLookbehind, str, till, posi, next_i, state::Nothing)
    rseq=reversed(str)
    ri = reverse_index(rseq,_prevind(str,next_i))
    next_i < 1 && return nothing
    r = _iterate(t.parser, rseq, till,
                 ri, nothing)
    if r === nothing
        nothing
    else
        next_i,MatchState()
    end
end


regex_inner(x::Union{PositiveLookbehind,NegativeLookbehind}) =
    regex_inner(reversed(x.parser))

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
reversed(x) = deepmap_parser(reversed,x)
reversed(x::Union{AnyChar,CharIn,CharNotIn,UnicodeClass,Always,Never,ConstantParser{N,Char} where N}) = x
reversed(x::AtStart) = AtEnd()
reversed(x::AtEnd) = AtStart()
deepmap_parser(::typeof(reversed),mem::AbstractDict,x::Sequence) =
    get!(mem,x) do
        Sequence(( deepmap_parser(reversed,mem,p) for p in reverse(x.parts) )...)
    end
deepmap_parser(::typeof(reversed),mem::AbstractDict,x::Atomic) =
    get!(mem,x) do
        Atomic(deepmap_parser(reversed,mem,x.parser))
    end
deepmap_parser(::typeof(reversed),mem::AbstractDict,x::NegativeLookbehind) =
    get!(mem,x) do
        NegativeLookahead(x.parser) ##deepmap_parser(reversed,@show x.parser))
    end
deepmap_parser(::typeof(reversed),mem::AbstractDict,x::NegativeLookahead) =
    get!(mem,x) do
        NegativeLookbehind(x.parser) ##deepmap_parser(reversed,x.parser))
    end
deepmap_parser(::typeof(reversed),mem::AbstractDict,x::PositiveLookbehind) =
    get!(mem,x) do
        PositiveLookahead(x.parser) ##deepmap_parser(reversed,x.parser))
    end
deepmap_parser(::typeof(reversed),mem::AbstractDict,x::PositiveLookahead) =
    get!(mem,x) do
        PositiveLookbehind(x.parser) ##deepmap_parser(reversed,x.parser))
    end
