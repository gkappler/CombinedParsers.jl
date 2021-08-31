export reversed

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
@auto_hash_equals struct PositiveLookbehind{S,T,P} <: WrappedAssertion{S,T}
    parser::P
    function PositiveLookbehind(p_,reversed_parser=true)
        p = reversed_parser ? reversed(parser(p_)) : parser(p_)
        new{Tuple{Int,state_type(p)},result_type(p),typeof(p)}(p)
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
@auto_hash_equals struct NegativeLookbehind{P} <: WrappedAssertion{MatchState,NegativeLookbehind{P}}
    parser::P
    function NegativeLookbehind(p_,reversed_parser=true)
        p = reversed_parser ? reversed(parser(p_)) : parser(p_)
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

children(x::PositiveLookbehind) =
    children(x.parser)

function iterate_state(t::NegativeLookbehind, str, till, posi, next_i, state::Nothing)
    rseq=reversed(str)
    next_i < 1 && return next_i, MatchState()
    p = reverse_index(rseq,_prevind(str,next_i))
    r = iterate_state(t.parser, rseq, till, p, p, nothing)
    if r === nothing
        next_i,MatchState()
    else
        nothing
    end
end

@inline iterate_state(t::NegativeLookbehind, str, till, posi, next_i, state::MatchState) =
    nothing


@inline iterate_state(t::PositiveLookbehind, str, till, posi, next_i, state::MatchState) =
    nothing

function iterate_state(t::PositiveLookbehind, str, till, posi, next_i, state)
    rseq=reversed(str)
    ri = reverse_index(rseq,_prevind(str,next_i))
    next_i < 1 && return nothing
    r = iterate_state(t.parser, rseq, till, ri, tuple_pos(state,ri), tuple_state(state))
    if r === nothing
        nothing
    else
        next_i, r
    end
end


"""
    Base.get(parser::PositiveLookbehind, sequence, till, after, i, state)

get result of PositiveLookbehind

!!! note
    The result is currently for a `reversed` sequence, and 
    you might find it difficult to [`Base.map`](@ref) a lookbehind parser match.
    If you require this functionality please open an issue for discussion.

    Assertions do not consume input, so typically these input chars are parsed/mapped outside of the assertion.

```jldoctest
julia> p = Sequence(!re"a+b", PositiveLookbehind(!re"a+b"))
🗄 Sequence
├─ 🗄 Sequence |> !
│  ├─ a+  |> Repeat
│  └─ b
└─ (?<=🗄) Sequence |> ! |> PositiveLookbehind
   ├─ b
   └─ a+  |> Repeat
::Tuple{SubString{String}, SubString{String}}

julia> p("aaab")
("aaab", "baaa")
```
"""
function Base.get(parser::PositiveLookbehind, sequence, till, after, i, state)
    rseq = reversed(sequence)
    after_ = tuple_pos(state)
    get(parser.parser, rseq, till, after_, reverse_index(rseq,prevind(sequence, i)), tuple_state(state))
end

regex_inner(x::Union{PositiveLookbehind,NegativeLookbehind}) =
    regex_inner(reversed(x.parser))

children(x::Union{PositiveLookbehind,NegativeLookbehind}) =
    reverse(children(x.parser))
reversed(x::CombinedParser) = deepmap_parser(_reversed, x)
_reversed(x::ConstantParser{<:AbstractString}) =
    ConstantParser(reversed(x.parser))
_reversed(x) = x
_reversed(x::AtEnd) = AtStart()
_reversed(x::AtStart) = AtEnd()

_deepmap_parser(::typeof(_reversed),mem::AbstractDict,x::Sequence) =
    Sequence(( deepmap_parser(_reversed,mem,p) for p in reverse(x.parts) )...)

for T in [PositiveLookahead,NegativeLookahead,PositiveLookbehind,NegativeLookbehind]
    eval(quote
             _deepmap_parser(f::Function,mem::AbstractDict,x_::$T,a...; kw...) =
                 $T(deepmap_parser(f,mem,x_.parser,a...; kw...),false)
         end)
end


_deepmap_parser(::typeof(_reversed),mem::AbstractDict,x::NegativeLookbehind) =
    NegativeLookahead(x.parser) ##deepmap_parser(reversed,@show x.parser))
_deepmap_parser(::typeof(_reversed),mem::AbstractDict,x::NegativeLookahead) =
    NegativeLookbehind(x.parser) ##deepmap_parser(reversed,x.parser))
_deepmap_parser(::typeof(_reversed),mem::AbstractDict,x::PositiveLookbehind) =
    PositiveLookahead(x.parser) ##deepmap_parser(reversed,x.parser))
_deepmap_parser(::typeof(_reversed),mem::AbstractDict,x::PositiveLookahead) =
    PositiveLookbehind(x.parser) ##deepmap_parser(reversed,x.parser))
