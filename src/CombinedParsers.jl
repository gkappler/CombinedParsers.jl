# TODO:
# - remove after from get (nextind with state and i)
# - (Feedback appreciated: Would is be more efficient change the `_iterate` internal API for the first match to arity 4?)
# - Base.get(parser, sequence, till, after, i, state) to
#   Base.get(parser, sequence, i, after, till, state) 
"""
A package for combining parsers and transforming strings into julia types.

Compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inferrence for transformations,
log conveniently for debugging, and let Julia compile your parser for good performance.
"""
module CombinedParsers
import Base: (^), (*), (~), (/), (|), (!), cat, get, prevind, nextind
using Nullables
using AutoHashEquals

using TextParse
import TextParse: AbstractToken
import Base: ==, hash

export CombinedParser
export result_type

"""
State object for a match that is defined by the parser, sequence and position.

See also [`NCodeuntitsMatchState`](@ref).
"""
struct MatchState end
Base.show(io::IO, ::MatchState) = print(io,"∘")





"Julia types that provide CombinedParser methods result_type, state_type, _iterate, get, nextind, prevind."
ParserTypes = Union{AbstractToken, AbstractString, Char, Regex}
## Pair{<:Union{AbstractToken, AbstractString, Char, Regex, Pair},<:Any} }
export parser
import Base: convert
parser(x::AbstractToken) = x
"""
    parser(x::Union{AbstractString,Char})

A [`ConstantParser`](@ref) matching `x`.
"""
parser(x::Char) =
    CharIn(x)

"""
    parser(x::StepRange{Char,<:Integer})

[`CharIn`](@ref) matching x.
"""
parser(x::StepRange{Char,<:Integer}) =
    CharIn(x)
parser(x::T) where {T<:AbstractString} =
    ConstantParser(x)
function Base.convert(::Type{AbstractToken},x)
    parser(x)
end

"""
    CombinedParsers.state_type(x::T)

Return the state type of `x`
"""
@inline state_type(x::T) where {T<:Union{ParserTypes,AbstractToken}} = state_type(T)
@inline state_type(x::Type{<:Union{Char,AbstractString}}) = MatchState
@inline state_type(::Type{Any}) = Any
@inline function state_type(T::Type{<:AbstractToken{P}}) where P
    T <: CombinedParser && error("define state_type(::Type{$T})")
    NCodeunitsState{P}
end

result_type(x::Union{ParserTypes,AbstractToken}) = result_type(typeof(x))
result_type(::Type{<:AbstractToken{T}}) where T = T



"""
    _iterate(parser, sequence, till::Int, posi::Int[, nothing])

Dispatches to `_iterate(parser, sequence,till,posi,posi,nothing)` to retrieve first match, or nothing.
"""
@inline _iterate(parser, sequence, till::Int, posi::Int) =
    _iterate(parser, sequence,till,posi,posi,nothing)
@inline _iterate(parser, sequence, till::Int, posi::Int, ::Nothing) =
    _iterate(parser, sequence,till,posi,posi,nothing)



############################################################
## CombinedParsers interface for Union{Char,AbstractString}
"""
    tuple_pos(pos_state::Tuple)

`pos_state[1]` is position after match in tuple returned by [`_iterate`](@ref).
"""
@inline tuple_pos(pos_state::Tuple) =
    pos_state[1]

"""
    tuple_state(pos_state::Tuple)

`pos_state[2]` is state of match in tuple returned by [`_iterate`](@ref).
"""
@inline tuple_state(pos_state::Tuple) =
    pos_state[2]

result_type(T::Type{<:Union{Char,AbstractString}}) = T
"""
    _iterate(parser, sequence, till, posi, next_i, states)

Note: `next_i` is the index in `sequence` after `parser` match according to `state` (and not the start of the match), 
such that `start_index(sequence,after,parser,state)` returns the start of the matching subsequence,
and sequence[start_index(sequence,after,parser,state):prevind(sequence,next_i)] is the matched subsequence.
"""
@inline function _iterate(p::AbstractString, sequence, till, posi, next_i, state::Nothing,L=ncodeunits(p))
    till, posi, next_i
    j::Int = next_i
    k::Int = 1
    1
    while k<=L
        (j > till) && return(nothing)
        @inbounds pc,k=iterate(p,k)
        @inbounds sc,j=iterate(sequence,j)
        !ismatch(sc,pc) && return(nothing)
    end
    return j, MatchState()
end

@inline function _iterate(parser::Char, sequence, till, posi, next_i, state::Nothing,L=ncodeunits(parser))
    next_i>till && return nothing
    if ismatch(sequence[next_i],parser)
        next_i+L, MatchState()
    else
        nothing
    end
end

@inline function _iterate(p::Union{<:AbstractString,Char}, sequence, till, posi, next_i, state)
    nothing
end
@inline nextind(str,i::Int,parser::Union{AbstractString,Char},x) where L =
    i+ncodeunits(parser)
@inline prevind(str,i::Int,parser::Union{AbstractString,Char},x) where L = 
    i-ncodeunits(parser)

@inline _prevind(str,i,parser,x::Nothing) = i
@inline _nextind(str,i,parser,x::Nothing) = i
@inline _prevind(str,i,parser,x) = prevind(str,i,parser,x)
@inline _nextind(str,i,parser,x) = nextind(str,i,parser,x)






############################################################
## Parsing with AbstractToken

"""
State object representing ncodeunits of match for `prevind`, `nextind`, performance.

See also [`MatchState`](@ref), [`prevind`](@ref), [`nextind`](@ref).
"""
struct NCodeunitsState{S}
    nc::Int
    state::S
end
NCodeunitsState(posi::Int,after::Int,state) =
    after, NCodeunitsState(after-posi,state)
@inline NCodeunitsState{S}(posi::Int,after::Int,state) where S =
    after, NCodeunitsState{S}(after-posi,state)

@inline function nextind(str,i::Int,parser,x::NCodeunitsState)
    i+x.nc
end

@inline function prevind(str,i::Int,parser,x::NCodeunitsState)
    i-x.nc
end

function _iterate(parser::AbstractToken, sequence, till, before_i, next_i, state,opts=TextParse.default_opts)
    if parser isa CombinedParser
        @warn "define _iterate(parser::$(typeof(parser)), sequence, till, start_i, next_i, state::$(typeof(state)))"
        return nothing
    end
    if state === nothing
        r,next_i_ = tryparsenext(parser, sequence, next_i, till,opts)
        if isnull(r)
            nothing
        else
            NCodeunitsState(next_i,next_i_,get(r))
        end
    else
        nothing
    end
end


"""
    CombinedParser{T,S} <: AbstractToken{T}

Abstract parser type for parsers returning matches transformed to `::T` and 
state::`S`.
"""
abstract type CombinedParser{S,T} <: AbstractToken{T} end
"""
    (x::CombinedParser)(str;kw...)


`parse(x,str;kw...)`

See also [`parse`](@ref).
"""
(x::CombinedParser)(str;kw...) = parse(x,str;kw...)
(x::CombinedParser)(prefix,str;kw...) = parse(Sequence(2,prefix,x),str;kw...)
@inline state_type(::Type{<:CombinedParser{S}}) where {S} = S
state_type(::Type{<:CombinedParser}) = Any

"""
    LeafParser{T} <: AbstractToken{T}

Abstract parser type for parsers that have no sub-parser.
Used for dispatch in [`deepmap_parser`](@ref)
"""
abstract type LeafParser{S,T} <: CombinedParser{S,T} end

deepmap_parser(f::Function,mem::AbstractDict,x::ParserTypes,a...;kw...) =
    f(x,a...;kw...)

"""
    deepmap_parser(f::Function,x::CombinedParser,a...;kw...)

Perform a deep transformation of a CombinedParser.
Used for [`log_names`](@ref).

Calls `deepmap_parser(f,IdDict(),x,a...)`.
"""
deepmap_parser(f::Function,x::CombinedParser,a...;kw...) =
    deepmap_parser(f,IdDict(),x,a...;kw...)

"""
    deepmap_parser(f::Function,mem::AbstractDict,x,a...;kw...)

Perform a deep transformation of a CombinedParser.

!!! note
    For a custom parser `P<:CombinedParser` with sub-parsers, provide a method
    ```julia
    deepmap_parser(f::Function,mem::AbstractDict,x::P,a...;kw...) =
        get!(mem,x) do
            ## construct replacement, e.g. if P <: WrappedParser
            P(deepmap_parser(f,mem,x.parser,a...;kw...))
        end
    ```
"""
deepmap_parser(f::Function,mem::AbstractDict,x,a...;kw...) =
    error("""
    For a custom parser `$(typeof(x))` with sub-parsers, provide a method
    ```julia
    deepmap_parser(f::$(typeof(f)),mem::AbstractDict,x::$(typeof(x)),a...;kw...) =
        get!(mem,x) do
            ## construct replacement, e.g. if P <: WrappedParser
            P(deepmap_parser(f,mem,x.parser,a...;kw...))
        end
    ```
""")

"""
    deepmap_parser(f::Function,mem::AbstractDict,x::LeafParser,a...;kw...)

return 
```julia
    get!(mem,x) do
        f(x,a...;kw...)
    end
```
"""
deepmap_parser(f::Function,mem::AbstractDict,x::LeafParser,a...;kw...) =
    get!(mem,x) do
        f(x,a...;kw...)
    end


export regex_string
"""
    regex_string(x::CombinedParser)

`regex_prefix(x)*regex_inner(x)*regex_suffix(x)`
"""
regex_string(x::CombinedParser) = regex_prefix(x)*regex_inner(x)*regex_suffix(x)

"""
    regex_prefix(x)

Prefix printed in parser tree node.
"""
regex_prefix(x::AbstractToken) = ""
"""
    regex_suffix(x)

Suffix printed in parser tree node.
"""
regex_suffix(x::AbstractToken) = ""
"""
    regex_inner(x::AbstractToken)

Regex representation of `x`.
See [`regex_string`](@ref)
"""
regex_inner(x::AbstractToken) = "$(typeof(x))"

regex_prefix(x::CombinedParser) = ""
regex_suffix(x::CombinedParser) = ""
regex_inner(x::CombinedParser) = ""
"""
    print_constructor(io::IO,x)

Print constructor pipeline in parser tree node.
"""
print_constructor(io::IO,x) = print(io, typeof(x).name)




"Abstract type for parser wrappers, providing default methods"
abstract type WrappedParser{P,S,T} <: CombinedParser{S,T} end
import AbstractTrees: children
children(x::WrappedParser) = children(x.parser)
children_char = '\U1F5C4'

function print_constructor(io::IO,x::WrappedParser)
    print_constructor(io, x.parser)
    print(io, " |> ", typeof(x).name)
end

regex_prefix(x::WrappedParser) = regex_prefix(x.parser)
regex_suffix(x::WrappedParser) = regex_suffix(x.parser)
regex_inner(x::WrappedParser) = regex_inner(x.parser)

@inline prevind(str,i::Int,parser::W,x) where {W <: WrappedParser} = prevind(str,i,parser.parser,x)
@inline nextind(str,i::Int,parser::W,x) where {W <: WrappedParser} = nextind(str,i,parser.parser,x)

@inline nextind(str,i::Int,parser::W,x::NCodeunitsState) where {W <: WrappedParser} = i+x.nc
@inline prevind(str,i::Int,parser::W,x::NCodeunitsState) where {W <: WrappedParser} = i-x.nc


@inline _iterate(parser::WrappedParser, sequence, till, posi, after, state) =
    _iterate(parser.parser, sequence, till, posi, after, state)


export FilterParser
"""
A parser the succeeds ony if wrapped parser succeeds and a predicate function on the position,state tuple
`.state_filter(sequence, till, posi, r...)`.
"""
struct FilterParser{P,S,F,T} <: WrappedParser{P,S,T}
    parser::P
    state_filter::F
    FilterParser(f::Function,parser_) =
        let p = parser(parser_)
            new{typeof(p),state_type(p),typeof(f),result_type(p)}(p,f)
        end
end
Base.filter(f::Function, x::CombinedParser) =
    FilterParser(f,x)

@inline function _iterate(parser::W, sequence, till, posi, next_i, state) where {W <: FilterParser}
    r::Union{Nothing,Tuple{Int,state_type(W)}} = nothing
    while r === nothing
        r = _iterate(parser.parser, sequence, till, posi, next_i, state)
        if r === nothing
            return nothing
        elseif !parser.state_filter(sequence, till, posi, r...)
            next_i,state=r
            r = nothing
        end
    end
    r
end

export JoinSubstring
"""
    JoinSubstring(x)
    (!)(x::AbstractToken)

Parser Transformation getting the matched SubString.
"""
@auto_hash_equals struct JoinSubstring{P,S} <: WrappedParser{P,S,SubString}
    parser::P
    JoinSubstring(x) =
        new{typeof(x),state_type(x)}(x)
end
Base.map(f::Type{<:JoinSubstring}, p::AbstractToken) = JoinSubstring(p)
revert(x::JoinSubstring) = JoinSubstring(revert(x.parser))


"""
    (!)(x::AbstractToken)

Parser Transformation getting the matched SubString.

```jldoctest
julia> parse(Repeat(CharIn(:L)),"abc123")
3-element Array{Char,1}:
 'a'
 'b'
 'c'

julia> parse(!Repeat(CharIn(:L)),"abc123")
"abc"

```

"""
(!)(x::AbstractToken) = JoinSubstring(x)
using InternedStrings
import InternedStrings: intern
(!)(x::JoinSubstring) = map(InternedStrings.intern, x)


deepmap_parser(f::Function,mem::AbstractDict,x::JoinSubstring,a...;kw...) =
    get!(mem,x) do
        JoinSubstring(
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end

export map_match
map_match(f::Function,p_) =
    map(f, JoinSubstring(parser(p_)))


"wrapper for stepping with ncodeunit length."
@auto_hash_equals struct ConstantParser{N,P,T} <: WrappedParser{T,MatchState,T}
    parser::P
    function ConstantParser{N}(x::Char) where N
        new{N,Char,Char}(x)
    end
    function ConstantParser{N}(x::T) where {N,T<:AbstractString}
        new{N,T,SubString}(x)
    end
    function ConstantParser(x::T) where {T<:AbstractString}
        new{ncodeunits(x),T,SubString}(x)
    end
end

children(x::ConstantParser) = ()
regex_prefix(x::ConstantParser) = ""
print_constructor(io::IO,x::ConstantParser) = print(io,"")
regex_inner(x::ConstantParser) = regex_string(x.parser)
regex_suffix(x::ConstantParser) = ""

revert(x::ConstantParser{N,<:AbstractString}) where N =
    ConstantParser{N}(reverse(x.parser))

deepmap_parser(f::Function,mem::AbstractDict,x::ConstantParser,a...;kw...) =
    get!(mem,x) do
        f(x,a...;kw...)
    end

@inline nextind(str,i::Int,parser::ConstantParser{L},x) where L =
    i+L
@inline prevind(str,i::Int,parser::ConstantParser{L},x) where L = 
    i-L

@inline function _iterate(parser::ConstantParser{L}, sequence, till, posi, next_i, state::Nothing) where {L}
    _iterate(parser.parser,sequence,till,posi, next_i,state,L)
end




"Abstract type for stepping with previndex/nextindex, accounting for ncodeunit length of chars at point."
abstract type NIndexParser{N,T} <: LeafParser{MatchState,T} end
@inline prevind(str,i::Int,parser::Union{NIndexParser{0},ConstantParser{0}},x) =
    i
@inline nextind(str,i::Int,parser::Union{NIndexParser{0},ConstantParser{0}},x) =
    i
@inline prevind(str,i::Int,parser::NIndexParser{L},x) where L =
    prevind(str,i,L)
@inline nextind(str,i::Int,parser::NIndexParser{L},x) where L =
    nextind(str,i,L)
_iterate(parser::Union{NIndexParser,ConstantParser}, sequence, till, posi, next_i, state::MatchState)  =
    nothing

export AnyChar, any
"""
    AnyChar()

Parser matching exactly one `Char`, returning the character.
```jldoctest
julia> AnyChar()
re"."

```

"""
struct AnyChar <: NIndexParser{1,Char} end
any() = AnyChar()
regex_inner(x::AnyChar) = "."

struct MatchingNever{T} end
""""
    ismatch(c::Char,p::Union{Function,Steprange,Set,UnicodeClass})::Bool

checks is a character matches a pattern.

"""
ismatch(c::MatchingNever,p)::Bool = false
ismatch(c::MatchingNever,p::AnyChar)::Bool = false
_ismatch(c,p::Function)::Bool = p(c)::Bool
_ismatch(c,p::AnyChar)::Bool = true
_ismatch(c::Char,p::Union{StepRange,Set})::Bool = c in p

"""
    _ismatch(x::Char, set::Union{Tuple,Vector})::Bool

`_ismatch(x,set...)` respects boolean logic:

Example:
```jldoctest
julia> p = CharNotIn(CharNotIn("ab"));

julia> parse(p,"a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

```
"""
function _ismatch(x::Char, set::Union{Tuple,Vector})::Bool
    return _ismatch(x,set...)
end

function _ismatch(x::Char)::Bool
    return false
end

function _ismatch(x::Char, f, r1, r...)::Bool
    ismatch(x,f) && return true
    return _ismatch(x::Char, r1, r...)
end

function _ismatch(c::Char,p::Char)::Bool
    c==p
end
function ismatch(c::Char,p)::Bool
    _ismatch(c, p)
end


@inline function _iterate(parser::AnyChar, sequence, till, posi, next_i, state::Nothing)
    next_i>till && return(nothing)
    @inbounds c,ni = iterate(sequence,next_i)
    !ismatch(c,parser) && return nothing
    return ni, MatchState()
end

export AtStart, AtEnd
"""
    AtStart()

Parser succeding if and only if at index 1 with `result_type` `AtStart`.

```jldoctest
julia> AtStart()
re"^"

```
"""
struct AtStart <: NIndexParser{0,AtStart} end
regex_prefix(x::AtStart) = "^"
_iterate(parser::AtStart, sequence, till, posi, next_i, state::Nothing) =
    next_i == 1 ? (next_i, MatchState()) : nothing

print_constructor(io::IO, x::AtStart) = print(io,"AtStart")

"""
    AtEnd()

Parser succeding if and only if at last index with `result_type` `AtEnd`.

```jldoctest
julia> AtEnd()
re"\$"

```
"""
struct AtEnd <: NIndexParser{0,AtEnd} end
regex_suffix(x::AtEnd) = "\$"
_iterate(parser::AtEnd, sequence, till, posi, next_i, state::Nothing) =
    next_i > till ? (next_i, MatchState()) : nothing
print_constructor(io::IO, x::AtEnd) = print(io,"AtEnd")



export Never
"""
    Never()

Assertion parser matching never.

```jldoctest
julia> Never()
re"(*FAIL)"

```
"""
struct Never <: LeafParser{MatchState,Never} end
regex_prefix(x::Never) = "(*"
regex_inner(x::Never) = "FAIL"
regex_suffix(x::Never) = ")"
_iterate(x::Never,str,posi, next_i,till,state) =
    nothing


export Always
"""
    Always()

Assertion parser matching always and not consuming any input.
Returns `Always()`.

```jldoctest
julia> Always()
re""

```
"""
struct Always <: LeafParser{MatchState,Always}
end
Base.show(io::IO,x::Always) = print(io,"re\"\"")
children(x::Union{Never,Always}) = tuple()
regex_prefix(x::Always) = ""
regex_inner(x::Always) = ""
regex_suffix(x::Always) = ""
_iterate(parser::Always, str, till, posi, next_i, s::Nothing) =
    next_i, MatchState()
_iterate(parser::Always, str, till, posi, next_i, s::MatchState) =
    nothing
prevind(str,i::Int,p::Always,x) = i
nextind(str,i::Int,p::Always,x) = i
##_iterate(parser::Never, str, till, posi, next_i, s) = nothing


Base.show(io::IO, x::Union{AtStart,AtEnd,Never,Always}) =
    print(io,"re\"",regex_string(x),"\"")


"""
Parsers that do not consume any input can inherit this type.
"""
abstract type LookAround{T} <: NIndexParser{0,T} end
children(x::LookAround) = (x.parser,)
_iterate(t::LookAround, str, till, posi, next_i, state::MatchState) =
    nothing

export PositiveLookahead
"""
    PositiveLookahead(parser)

Parser that succeeds if and only if `parser` succeeds, but consumes no input.
The match is returned.
Useful for checks like "must be followed by `parser`, but don't consume its match".

```jldoctest
julia> la=PositiveLookahead("peek")
re"(?=peek)"

julia> parse(la*AnyChar(),"peek")
("peek", 'p')

```
"""
@auto_hash_equals struct PositiveLookahead{T,P} <: LookAround{T}
    parser::P
    PositiveLookahead(p_,revert=true) =
        let p = parser(p_)
            new{result_type(p),typeof(p)}(p)
        end
end
regex_prefix(x::PositiveLookahead) = "(?="*regex_prefix(x.parser)
regex_suffix(x::LookAround) = regex_suffix(x.parser)*")"
regex_inner(x::LookAround) = regex_string(x.parser)
function _iterate(t::PositiveLookahead, str, till, posi, next_i, state::Nothing)
    r = _iterate(t.parser, str, till, posi, next_i, nothing)
    if r === nothing
        nothing
    else
        next_i,MatchState()
    end
end


export NegativeLookahead
"""
    NegativeLookahead(parser)

Parser that succeeds if and only if `parser` does not succeed, but consumes no input.
`parser` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".

```jldoctest
julia> la = NegativeLookahead("peek")
re"(?!peek)"

julia> parse(la*AnyChar(),"seek")
(re"(?!peek)", 's')

```
"""
@auto_hash_equals struct NegativeLookahead{P} <: LookAround{NegativeLookahead{P}}
    parser::P
    NegativeLookahead(p_,revert=true) =
        let p = parser(p_)
            new{typeof(p)}(p)
        end
end
regex_prefix(x::NegativeLookahead) = "(?!"*regex_prefix(x.parser)
function _iterate(t::NegativeLookahead, str, till, posi, next_i, state::Nothing)
    r = _iterate(t.parser, str, till, posi, next_i, nothing)
    if r === nothing
        next_i,MatchState()
    else
        nothing
    end
end

export Lookahead

"""
    Lookahead(does_match::Bool, p)

[`PositiveLookahead`](@ref) if `does_match==true`, 
[`NegativeLookahead`](@ref) otherwise.
"""
function Lookahead(does_match::Bool, p_)
    p = parser(p_)
    if does_match
        PositiveLookahead(p)
    else
        NegativeLookahead(p)
    end
end

@deprecate look_ahead(does_match,p) Lookahead(does_match, p)





@auto_hash_equals struct PartialMatchException{S,P} <: Exception
    index::Int
    str::S
    delta::Int
    pattern::P
    PartialMatchException(i,str::S,p::P) where {S<:AbstractString,P} =
        new{S,P}(i,str,200,p)
    PartialMatchException(i,str::S,p::P) where {S,P} =
        new{S,P}(i,str,6,p)
end
export context
context(x::PartialMatchException) =
    x.str[min(x.index,end):min(end, nextind(x.str,x.index,x.delta))]
import Base: showerror
function Base.showerror(io::IO, x::PartialMatchException)
    println(io, "parsing stopped at postion $(x.index) in:")
    println(io, "$(x.str)")
    println(io, "."^(x.index-1),"^")
    ##println(io, x.pattern)
end

@auto_hash_equals struct SideeffectParser{P,S,T,A} <: WrappedParser{P,S,T}
    parser::P
    args::A
    effect::Function
    SideeffectParser(f::Function, p::AbstractToken,a...) =
        new{typeof(p),state_type(p),result_type(p),typeof(a)}(p,a,f)
end
children(x::SideeffectParser) = children(x.parser)
function print_constructor(io::IO,x::SideeffectParser)
    print_constructor(io,x.parser)
    c = if x.effect == log_effect
        "with_log(;nomatch=true)"
    elseif x.effect == log_effect_match
        "with_log"
    else
        "with_effect($(x.effect))"
    end
    print(io," |> $c")
end
regex_string(x::SideeffectParser) = regex_string(x.parser)

export with_log, with_effect
"""
    with_effect(f::Function,p,a...)

Call `f(sequence,before_i,after_i,state,a...)` if `p` matches,
 `f(sequence,before_i,before_i,nothing,a...)` otherwise.
"""
with_effect(f::Function,p,a...) =
    SideeffectParser(f,p,a...)

@inline function _iterate(parser::SideeffectParser, sequence, till, posi, next_i, state)
    r = _iterate(parser.parser, sequence, till, posi, next_i, state)
    if r!==nothing
        parser.effect(sequence,posi,r...,parser.args...)
    else
        parser.effect(sequence,posi,posi,nothing,parser.args...)
    end
    r
end

export deepmap_parser
deepmap_parser(f::Function,mem::AbstractDict,x::SideeffectParser,a...;kw...) =
    get!(mem,x) do
        SideeffectParser(x.effect,
                         deepmap_parser(f,mem,x.parser,a...;kw...),
                         x.args...)
    end

"""
    with_log(s::AbstractString,p, delta=5;nomatch=false)

Log matching process of parser `p`, displaying `delta` characters left of and right of match.

If `nomatch==true`, also log when parser does not match.

See also: [`log_names`](@ref), [`with_effect`](@ref)
"""
with_log(s::AbstractString,p_, delta_char::Integer=5;nomatch=false) =
    let p = parser(p_), log=s*": "
        SideeffectParser(nomatch ? log_effect : log_effect_match ,p, log, delta_char)
    end

function log_effect(s,start,after,state,log,delta)
    if state === nothing
        printstyled("no match ",
                    bold=true,color=:underline)
    else
        printstyled("   match ";
                    bold=true,color=:green)
    end
    print(log)
    firsti = prevind(s,start,delta)
    lasti = (prevind(s,start))
    before, matched = if prevind(s,start)<start
        escape_string(s[max(1,firsti):lasti]), escape_string(s[start:prevind(s,after)])
    else
        "",""
    end
    printstyled(before)
    printstyled(matched; bold=true,color=:green)
    if state === nothing 
        printstyled(escape_string(s[after:min(end,after+delta)]),
                    bold=true,color=:underline)
    else
        printstyled(escape_string(s[after:min(end,after+delta)]),
                    color=:yellow)
    end
    println()
    if !get(stdout,:color,false)
        print(" "^(9+length(log)+length(before)),"^")
        if length(matched)>1
            print("_"^(length(matched)-2),"^")
        end
        println()
    end
end

function log_effect_match(s,start,after,state,log,delta)
    if state!==nothing
        log_effect(s,start,after,state,log,delta)
    end
end




export NamedParser, with_name
@auto_hash_equals struct NamedParser{P,S,T} <: WrappedParser{P,S,T}
    name::Symbol
    parser::P
    doc::String
    NamedParser(name::Symbol,p_,doc="") =
        let p=parser(p_)
            new{typeof(p),state_type(p),result_type(p)}(name,p,doc)
        end
end
function print_constructor(io::IO,x::NamedParser)
    print_constructor(io,x.parser)
    print(io, " |> with_name(:")
    printstyled(io, x.name, bold=true,color=:red)
    print(io, ")")
end

"""
    parser(x::Pair{Symbol, P}) where P

A parser labelled with name `x.first`.
Labels are useful in printing and logging.

See also: [`@with_names`](@ref), [`with_name`](@ref), [`log_names`](@ref)
"""
parser(x::Pair{Symbol, P}) where P =
    NamedParser(x.first, parser(x.second))

"""
    with_name(name::Symbol,x; doc="")

A parser labelled with `name`.
Labels are useful in printing and logging.

See also: [`@with_names`](@ref), [`with_name`](@ref), [`log_names`](@ref)
"""
with_name(name::Symbol,x; doc="") = 
    NamedParser(name,x,doc)

with_name(name::AbstractString,x; doc="") =
    name=="" && doc=="" ? x : NamedParser(Symbol(name),x,doc)

log_names_(x::CombinedParser,a...;kw...) = x
function deepmap_parser(f::typeof(log_names_),mem::AbstractDict,x::NamedParser,message::Function;kw...)
    get!(mem,x) do
        r = NamedParser(x.name,deepmap_parser(f,mem,x.parser,message;kw...))
        log=message(x)
        if log!==nothing
            with_log("$(log)",r)
        else
            r
        end
    end
end

deepmap_parser(f::Function,mem::AbstractDict,x::NamedParser,a...;kw...) =
    get!(mem,x) do
        NamedParser(x.name,deepmap_parser(f,mem,x.parser,a...;kw...))
    end



export log_names
"""
    log_names(x,names=true; exclude=nothing)

Rebuild parser replacing `NamedParser` instances with `with_log` parsers.
Log all `NamedParser` instanses if `names==true` or `name in names` and not `name in exclude`.

See also: [`with_log`](@ref), [`deepmap_parser`](@ref)
"""
function log_names(x,names=true; exclude=nothing, kw...)
    message = if names === true
        if exclude === nothing
            x -> x isa NamedParser ? x.name : nothing
        else
            x -> ( x isa NamedParser && !in(x.name,exclude) ) ? x.name : nothing
        end
    else
        x -> ( x isa NamedParser && in(x.name,names) ) ? x.name : nothing
    end
    deepmap_parser(log_names_,Dict(),x, message;kw...)
end

export @with_names
with_names(x) = x
function with_names(node::Expr)
    if node.head == :(=) && length(node.args) == 2 && isa(node.args[1], Symbol)
        node.args[2] = Expr(:call, :with_name, QuoteNode(node.args[1]), node.args[2])
    end
    if node.head != :call 
        node.args = map(with_names, node.args)
    end
    node
end


"""
Sets names of parsers within begin/end block to match the variables they are asigned to.

so, for example
```jldoctest
julia> @with_names foo = AnyChar()
. AnyChar |> with_name(:foo)
::Char

julia> parse(log_names(foo),"ab")
   match foo: ab
              ^
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

See also [`log_names(parser)`](@ref)
"""
macro with_names(block)
    esc(with_names(block))
end

export @syntax
"""
Convenience macro `@syntax name = expr` for defining a CombinedParser `name=expr` and custom parsing macro `@name_str`.

With `@syntax for name in either; expr; end` the defined parser is [`pushfirst!`](@ref) to `either`.
If `either` is undefined, it will be created.
If `either == :text || either == Symbol(:)` the parser will be added to `CombinedParser_globals` variable in your module.

```@meta
DocTestFilters = r"map\\(#[^)]+\\)"
```

```jldoctest
julia> @syntax for german_street_address in street_address
            Sequence(:street => !Repeat(AnyChar()),
                 " ",
                 :no =>Numeric(Int))
       end
🗄 Sequence |> map(#38) |> with_name(:german_street_address)
├─ .* AnyChar |> Repeat |> JoinSubstring |> with_name(:street)
├─ \\
└─ TextParse.Numeric{Int64} TextParse.Numeric |> with_name(:no)
::NamedTuple{(:street, :no),Tuple{SubString,Int64}}

julia> german_street_address"Some Avenue 42"
NamedTuple{(:street, :no),Tuple{SubString,Int64}}(("Some Avenue", 42))


julia> @syntax for us_street_address in street_address
            Sequence(:no =>Numeric(Int),
                     " ",
                     :street => !Repeat(AnyChar()))
       end
🗄 Sequence |> map(#38) |> with_name(:us_street_address)
├─ TextParse.Numeric{Int64} TextParse.Numeric |> with_name(:no)
├─ \\  
└─ .* AnyChar |> Repeat |> JoinSubstring |> with_name(:street)
::NamedTuple{(:no, :street),Tuple{Int64,SubString}}

julia> street_address"50 Oakland Ave"
NamedTuple{(:no, :street),Tuple{Int64,SubString}}((50, "Oakland Ave"))
```
"""
macro syntax(block)
    R = if block.head == :for
        name, within = block.args[1].args
        within_expr = if within === :texts || within === Symbol(":") ## parser is global
            quote
                if isdefined($__module__,:CombinedParser_globals)
                    CombinedParser_globals
                else
                    global CombinedParser_globals
                    CombinedParser_globals = Repeat(Either{Any}())
                end
            end
        elseif __module__.eval( :(isdefined($__module__,$(QuoteNode(within))) && $within isa CombinedParser ))
            :($within)
        else ## new Either
            quote
                @syntax $within = Either{Any}()
            end
        end
        body = block.args[2]
        if body.head==:block
            expr = Any[]
            for e in body.args
                if e isa LineNumberNode
                    push!(expr,e)
                elseif e isa Symbol
                    push!(expr,e)
                elseif e.head==Symbol("=") && e.args[1]==:examples
                    @warn "examples currently ignored" 
                else
                    push!(expr,with_names(Expr(Symbol("="), name, e)))
                end
            end
            quote
                macro $(Symbol(string(name)*"_str"))(x)
                    $name(x)
                end
                global $name
                $(expr...)
                pushfirst!($within_expr, $name)
                $name
            end
        else
            dump(block)
            error()
        end
    elseif block.head==Symbol("=")
        name = block.args[1]
        quote
            macro $(Symbol(string(name)*"_str"))(x)
                $name(x)
            end
            $(with_names(block))
        end
    else
        dump(block)
        error()
    end
    esc(R)
end


import Base: in
include("unicode.jl")

export CharIn
"""
    CharIn(x)

Parser matching exactly one character in `x`.

```jldoctest
julia> a_z = CharIn('a':'z')
re"[a-z]"

julia> parse(a_z, "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> ac = CharIn("ac")
re"[ac]"


julia> parse(ac, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

```
"""
@auto_hash_equals struct CharIn{S} <: NIndexParser{1,Char}
    pcre::String
    sets::S
    CharIn(pcre::String,x::T) where {T<:Union{Char,Set{Char},<:Function,<:UnicodeClass,<:Tuple}}=
        new{T}(pcre,x)
end
CharIn(pcre::String,x_...) =
    CharIn(pcre, optimize(CharIn,x_...))
CharIn(chars::String) =
    CharIn("",chars)
CharIn(x_...) =
    CharIn("",x_...)
CharIn(chars::StepRange) =
    CharIn("$(chars.start)-$(chars.stop)",chars)

"""
    CharIn(unicode_class::Symbol...)

succeeds if char at cursor is in one of the unicode classes.
"""
CharIn(unicode_classes::Symbol...) =
    CharIn(UnicodeClass(unicode_classes...))

_ismatch(c,p::CharIn)::Bool = _ismatch(c,p.sets)

export regex_escape
## https://github.com/JuliaLang/julia/pull/29643/commits/dfb865385edf19b681bc0936028af23b1f282b1d
"""
        regex_escape(s::AbstractString)

regular expression metacharacters are escaped along with whitespace.
"""
function regex_escape(s)
    res = replace(escape_string(string(s)), r"([()[\]{}?*+\-|^\$.&~#\s=!<>|:])" => s"\\\1")
    replace(res, "\0" => "\\0")
end
export regex_string
regex_string(x::AbstractString) = regex_escape(x)
regex_string_(x::AbstractString) = regex_escape(x)
regex_string(::TextParse.Numeric{<:Integer}) = "-?[[:digit:]]+"

result_type(::Type{<:CharIn}) = Char
regex_string_(x::Union{Vector,Set}) = join(regex_string_.(x))
regex_string(x::Char) = regex_escape("$x") ##x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string_(x::Char) = regex_escape("$x") ##x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string_(x::StepRange) =
    if x.start == x.stop
        x.start
    else
        x.start*"-"*x.stop
    end
regex_string_(x::Tuple) = join([regex_string_(s) for s in x])
regex_string_(x::CharIn) = ( x.pcre =="" ? regex_string_(x.sets) : x.pcre )
regex_inner(x::CharIn) =
    "["*regex_string_(x)*"]"

print_constructor(io::IO,x::CharIn{Char}) = nothing
regex_inner(x::CharIn{Char}) =
    regex_string_(x.sets)



export CharNotIn
"""
    CharNotIn(x)

Parser matching exactly one character in `x`.

```jldoctest
julia> a_z = CharNotIn('a':'z')
re"[^a-z]"

julia> ac = CharNotIn("ac")
re"[^ac]"

```
"""
@auto_hash_equals struct CharNotIn{S} <: NIndexParser{1,Char}
    pcre::String
    sets::S
    CharNotIn(pcre::String,x::T) where {T<:Union{Char,Set{Char},Function,UnicodeClass,Tuple}}=
        new{T}(pcre,x)
end
CharNotIn(pcre::String,x_...) =
    CharNotIn(pcre,optimize(CharNotIn,x_...))
CharNotIn(chars::String) =
    CharNotIn("",chars)
CharNotIn(chars::StepRange) =
    CharNotIn("$(chars.start)-$(chars.stop)",chars)
CharNotIn(x_...) =
    CharNotIn("",x_...)
result_type(::Type{<:CharNotIn}) = Char
regex_string_(x::CharNotIn) = ( x.pcre =="" ? regex_string_(x.sets) : x.pcre )
regex_inner(x::CharNotIn) =
    "[^"*regex_string_(x)*"]"
_ismatch(c,p::CharNotIn)::Bool = !_ismatch(c,p.sets)

CharIn(x::Tuple{<:CharNotIn}) = x[1]

"""
    CharIn(unicode_classes::Symbol...)

succeeds if char at cursor is not in any of the `unicode_classes`.
"""
CharNotIn(unicode_classes::Symbol...) =
    CharNotIn(UnicodeClass(unicode_classes...))

@inline function _iterate(parser::CharNotIn, sequence, till, posi, next_i, state::Nothing)
    next_i>till && return(nothing)
    @inbounds c,ni = iterate(sequence,next_i)
    ismatch(c,parser.sets) && return nothing 
    return ni, MatchState()
end

@inline function _iterate(parser::CharIn, sequence, till, posi, next_i, state::Nothing)
    next_i>till && return nothing
    @inbounds c,ni = iterate(sequence,next_i)
    !ismatch(c,parser.sets) && return nothing
    return ni, MatchState()
end


export CharMatcher
BaseCharMatcher = Union{Char, AnyChar, UnicodeClass,StepRange{Char,Int}}
CharMatcher = Union{CharIn, CharNotIn, BaseCharMatcher}

optimize!(charset,otherstuff) =
    charset,otherstuff
optimize!(charset::Nothing,otherstuff,x::Char) =
    x,otherstuff
optimize!(charset::Char,otherstuff,x::Char) =
    optimize!(Set{Char}(charset),otherstuff,x)
function optimize!(charset::Set{Char},otherstuff,x::Char)
    push!(charset,x)
    charset,otherstuff
end
optimize!(charset,otherstuff::Nothing,x::Union{<:Function,<:UnicodeClass,<:CharNotIn}) =
    optimize!(charset,Any[],x)
function optimize!(charset,otherstuff::Vector{Any},x::Union{<:Function,<:UnicodeClass,<:CharNotIn})
    push!(otherstuff,x)
    charset,otherstuff
end
function optimize!(charset,otherstuff,c::CharIn)
    optimize!(charset,otherstuff,c.sets)
end
function optimize!(charset,otherstuff,x::Union{Vector,Tuple,StepRange{Char,Int},Set{Char},<:AbstractString})
    optimize!(charset,otherstuff,x...)
end
function optimize!(charset,otherstuff,x1,x2,x...)
    optimize!(optimize!(charset,otherstuff,x1)...,x2,x...)
end

function optimize(::Type{<:Union{CharIn,CharNotIn}},x...)
    charset,otherstuff = optimize!(nothing,nothing,x...)
    if otherstuff===nothing
        charset === nothing ? tuple() : charset
    elseif charset===nothing
        tuple(otherstuff...)
    else
        tuple(charset,otherstuff...)
    end
end


export Repeat_stop, Repeat_until
"""
    Repeat_stop(p,stop)

Repeat `p` until `stop` (`NegativeLookahead`), not matching `stop`.
Sets cursor **before** `stop`.
Returns results of `p`.

```jldoctest
julia> p = Repeat_stop(AnyChar(),'b') * AnyChar()
🗄 Sequence
├─ 🗄* Sequence |> map(#52) |> Repeat
│  ├─ (?!🗄) NegativeLookahead
│  │  └─ b
│  └─ . AnyChar
└─ . AnyChar
::Tuple{Array{Char,1},Char}

julia> parse(p,"acbX")
(['a', 'c'], 'b')
```

See also [`NegativeLookahead`](@ref)
"""
Repeat_stop(p,stop) =
    Repeat(Sequence(2,NegativeLookahead(parser(stop)),parser(p)))

@deprecate rep_stop(a...;kw...) Repeat_stop(a...;kw...)

"""
    Repeat\\_until(p,until, with_until=false;wrap=identity)

Repeat `p` until `stop` (with [`Repeat_stop`](@ref)).
and set point **after** `stop`.

Return a `Vector{result_type(p)}` if `wrap_until==false`, otherwise a `Tuple{Vector{result_type(p)},result_type(until)}`.

To transform the `Repeat_stop(p)` parser head, provide a function(::Vector{result_type(p)}) in `wrap` keyword argument, e.g.
```jldoctest
julia> p = Repeat_until(AnyChar(),'b') * AnyChar()
🗄 Sequence
├─ 🗄 Sequence |> map(#52)
│  ├─ (?>🗄*) Sequence |> map(#52) |> Repeat |> Atomic
│  │  ├─ (?!🗄) NegativeLookahead
│  │  │  └─ b
│  │  └─ . AnyChar
│  └─ b
└─ . AnyChar
::Tuple{Array{Char,1},Char}

julia> parse(p,"acbX")
(['a', 'c'], 'X')

julia> parse(Repeat_until(AnyChar(),'b';wrap=JoinSubstring),"acbX")
"ac"
```

See also [`NegativeLookahead`](@ref)
"""
Repeat_until(p,until, with_until=false;wrap=identity) =
    if with_until
        Sequence(map(wrap,Atomic(Repeat_stop(p,until))), until)
    else
        Sequence(1, map(wrap,Atomic(Repeat_stop(p,until))), until)
    end

@deprecate rep_until(p,until) Repeat_until(p,until)



export FlatMap,after
@auto_hash_equals struct FlatMap{P,S,Q<:Function,T} <: CombinedParser{S,T}
    left::P
    right::Q
    function FlatMap{T}(left::P, right::Q) where {T, P, Q<:Function}
        new{P,Tuple{<:Any,<:Any,<:Any},Q,T}(left, right)
    end
end
flatmap_state(old,ls,rp,rs) = tuple_pos(rs), (ls,rp,tuple_state(rs))
left_state(state::Tuple) = state[1]
right_parser(state::Tuple) = state[2]
right_state(state::Tuple) = state[3]


children(x::FlatMap) = ( x.left, x.right )
function print_constructor(io::IO,x::FlatMap)
    print_constructor(io,x.left)
    print(io, "FlatMap(",x.right,")" )
end
after(right::Function,left::AbstractToken,T::Type) =
    FlatMap{T}(left,right)

function after(right::Function,left::AbstractToken)
    result_type(left)
    RT = infer_result_type(right,Any,left,"")
    T=result_type(RT)
    FlatMap{T}(left,right)
end

deepmap_parser(f::Function,mem::AbstractDict,x::FlatMap,a...;kw...) =
    get!(mem,x) do
        FlatMap{result_type(x)}(deepmap_parser(f,mem,x.left,a...;kw...),
                                v -> deepmap_parser(f,mem,x.right(v),a...;kw...))
    end
regex_inner(x::FlatMap)  = error("regex determined at runtime!")


@inline nextind(str,i::Int,parser::FlatMap,x::Tuple) =
    let li = nextind(str,i,parser.left,tuple_pos(x))
        nextind(str,li,x[2],x[3])
    end

@inline prevind(str,i::Int,parser::FlatMap,x::Tuple) =
    let li = prevind(str,i,x[2],x[3])
        prevind(str,li,parser.left,tuple_pos(x))
    end

    
function _iterate(tokf::FlatMap, str, till, posi, next_i, state::Nothing)
    posi = next_i
    lr = _iterate(tokf.left, str, till, posi, next_i, nothing)
    lr === nothing && return nothing
    next_i_ = tuple_pos(lr)
    rightp = tokf.right(get(tokf.left, str, till, next_i_,next_i,tuple_state(lr)))
    rr = nothing
    while rr === nothing
        rr = _iterate(rightp, str, till, next_i_, next_i_, nothing)
        if rr === nothing
            lr = _iterate(tokf.left, str, till, posi, next_i_, tuple_state(lr))
            lr === nothing && return nothing
            next_i_ = tuple_pos(lr)
            rightp = tokf.right(get(tokf.left, str, till, next_i_,posi,tuple_state(lr)))
        else
            return flatmap_state(nothing,tuple_state(lr), rightp, rr)
        end
    end
    nothing
end

function _iterate(tokf::FlatMap, str, till, posi, next_i, state)
    lstate,rightp,rstate = left_state(state), right_parser(state), right_state(state)

    next_i_=next_i
    posi_ = start_index(str,next_i_,rightp,rstate)
    rr = nothing
    while rr === nothing
        rr = _iterate(rightp, str, till, posi_, next_i_, rstate)
        if rr === nothing
            lr = _iterate(tokf.left, str, till, posi, next_i_, lstate)
            lr === nothing && return nothing
            next_i_,lstate = lr
            rightp = tokf.right(get(tokf.left, str, till, next_i_,posi,lstate))
            rstate = nothing
        else
            return flatmap_state(state,lstate, rightp, rr)
        end
    end
end

"""
    result_type(::Type{T}) where {T<:Tuple}

Internally used for `Sequence` result_type.
"""
@generated result_type(::Type{T}) where {T<:Tuple} =
    Tuple{ (result_type(t) for t in fieldtypes(T))... }

export Sequence
@auto_hash_equals struct Sequence{P,S,T} <: CombinedParser{S,T}
    parts::P
    Sequence(p::ParserTypes...) =
        new{typeof(p),state_type_tuple(p),result_type(typeof(p))}(p)
end


state_type_tuple(x) = state_type_tuple(typeof(x))
function state_type_tuple(pts::Type)
    if isempty(fieldtypes(pts)) || all(t->state_type(t)<:MatchState, fieldtypes(pts))
        MatchState
    else
        ## Tuple{(state_type(p) for p in fieldtypes(pts))...}
        Vector{Any}
    end
end

function Sequence(p...)
    s = Sequence(( parser(x) for x = p )...)
    T = fieldtypes(result_type(s))
    names = ( t.first=>i
              for (i,t) in enumerate(p)
              if t isa Pair{Symbol,<:ParserTypes} )
    isempty(names) && return s
    NT= NamedTuple{ tuple( (n.first for n in names)...),
                    Tuple{ (T[n.second] for n in names)... }}
    NTn = NamedTuple{ tuple( (n.first for n in names)...) }
    function transform(v,i)
        NT( tuple( (v[k.second] for k in names )... ))
    end
    map_at(transform, NT, s)
end

Base.lastindex(x::Sequence) = lastindex(x.parts)

"""
    Sequence(;kw...)

Sequence keyword argument constructors transform the parsing into a named tuple.
"""
Sequence(;kw...) =
    isempty(kw) ? Always() : Sequence(kw...)


Sequence(transform::Function, T::Type, a...) =
    map(transform, T, Sequence(a...))

Sequence(transform::Function, a...) =
    map(transform, Sequence(a...))


Sequence(transform::Integer,tokens...) =
    Sequence(Val{transform}(),tokens...)

function Sequence(::Val{transform},tokens...) where {transform}
    s = Sequence(tokens...)
    map(v -> v[transform], fieldtype(result_type(s),transform), s)
end





print_constructor(io::IO,x::Sequence) = print(io,"Sequence")
children(x::Sequence) = x.parts

Base.getindex(x::CombinedParser, i) = map(IndexAt(i),x)

deepmap_parser(f::Function,mem::AbstractDict,x::Sequence,a...;kw...) =
    get!(mem,x) do
        Sequence( ( deepmap_parser(f,mem,p,a...;kw...)
                    for p in x.parts)... )
    end

export sSequence
sSequence_(x::Sequence) = sSequence_(x.parts...)
sSequence_(x::Always) = tuple()
sSequence_() = tuple()
sSequence_(x1) = tuple(parser(x1))
sSequence_(x1,x...) = Iterators.flatten( ( sSequence_(x1), Iterators.flatten( ( sSequence_(e) for e in x ) ) ) )

"""
    sSequence(x...)

Simplifying `Sequence`, flatten `Sequence`s, remove `Always` lookarounds.

```jldoctest
julia> Sequence('a',CharIn("AB")*'b')
🗄 Sequence
├─ a
└─ 🗄 Sequence
   ├─ [AB] CharIn
   └─ b
::Tuple{Char,Tuple{Char,Char}}


julia> sSequence('a',CharIn("AB")*'b')
🗄 Sequence
├─ a
├─ [AB] CharIn
└─ b
::Tuple{Char,Char,Char}
```
See also [`Sequence`](@ref)
"""
function sSequence(x...)
    sSequence(sSequence_(x...)...)
end

sSequence(x::AbstractToken) = x
function sSequence(x::AbstractToken...)
    Sequence(sSequence_(x...)...)
end



@inline function prevind(str,i::Int,parser::Sequence,x::MatchState)
    for p in length(parser.parts):-1:1
        i=prevind(str,i,parser.parts[p],x)
    end
    i
end

@inline function prevind(str,i::Int,parser::Sequence,x)
    for j in lastindex(x):-1:1
        (p,e) = parser.parts[j],x[j]
        i=prevind(str,i,p,e)
    end
    i
end

@inline function nextind(str,i::Int,parser::Sequence,x::MatchState)
    for p in parser.parts
        i=nextind(str,i,p,MatchState())
    end
    i
end

@inline function nextind(str,i::Int,parser::Sequence,x)
    for (p,e) in zip(parser.parts,x)
        i=nextind(str,i,p,e)
    end
    i
end

@inline function start_index(sequence,after,parser,state::Nothing)
    after
end
@inline function start_index(sequence,after,parser,state)
    _prevind(sequence, after, parser, state)
end

function prune_captures(sequence,after_i)
end


Base.getindex(A::MatchState, i::Int) = MatchState()
Base.setindex!(A::MatchState, ::MatchState, i::Int) = nothing
Base.setindex!(A::MatchState, v, i::Int) = error("MatchState elements can only be ::MatchState")

Base.convert(::Type{MatchState},::Tuple{}) = MatchState()

function _iterate_(parser::Sequence, sequence, till, posi, next_i, states)
    next_i_ = next_i
    parts=parser.parts
    nexti,states = if states === nothing
        sss = Vector{Any}(undef,length(parts))
        length(parts) == 0 && return next_i,sss
        sss[1] = nothing
        1,sss
    else
        length(states),states
    end
    length(parts) == 0 && return nothing
    while nexti<=length(states)
        ## compute before next iteration, because states[nexti] might change.
        ## only used if ns===nothing, but states[nexti] might still be modified.
        posi = start_index(sequence, next_i_, parts[nexti], states[nexti])
        ns = _iterate(parts[nexti], sequence, till, posi, next_i_, states[nexti])
        if ns === nothing
            nexti -= 1
            next_i_ = posi
            prune_captures(sequence,next_i_)
            nexti == 0 && return nothing
            if posi==0
                println(parts[nexti])
                error()
            end
        else
            states[nexti] = tuple_state(ns)
            next_i_ = tuple_pos(ns)
            nexti += 1
            if nexti > length(states)
                return next_i_, states
            else
                states[nexti] = nothing
            end
        end
    end
    error("?")
end

@generated function _iterate(parser::Sequence{pts,sts}, sequence, till, posi, next_i, states)::Union{Nothing,Tuple{Int,sts}} where {pts<:Tuple,sts}
    fpts = fieldtypes(pts)
    spts = Type[ Union{Nothing,state_type(t)} for t in fpts ]
    n = length(fpts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    pposi = Symbol[ gensym(:pos) for p in 1:(n+1) ]
    substate,init = if states<:Nothing
        substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            $(substate[i])::$t = nothing
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    elseif states<:MatchState
        substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            $(substate[i])::Union{Nothing,MatchState} = MatchState()
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    elseif states<:Vector
        substate = Expr[ Expr(Symbol("::"), Expr(:ref,:states,i), t) for (i,(p,t)) in enumerate(zip(fpts,spts)) ]
        ## substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            ## @inbounds $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    else ## Tuple
        substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            @inbounds $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    end

    ret_state = if state_type(parser) <: MatchState
        :(MatchState())
    elseif state_type(parser) <: Tuple
        :(tuple( $([ :(($(s))) for s in substate ]...) ) )
    elseif states <: Nothing
        :(Any[ $([ :(($(s))) for s in substate ]...) ] )
    elseif states <: Vector
        quote
            ## $( [ :(@inbounds states[$i]=$(substate[i])) for i in 1:n ]...)
            states
        end
    else
        error("invalid state_type")
    end
    parseparts = [
        quote
        @label $(subsearch[p])
        if iszero($(pposi[p]))
            $(pposi[p]) = start_index(sequence, $(pposi[p+1]), $(part[p]), @inbounds $(substate[p]))
        end
        if (@inbounds $(substate[p])) === nothing
            ## if sss[$p] === nothing
            $(pposi[p+1]) = $(pposi[p])
        end
        ## TODO: gc happening in next line?
        $(subresult[p]) = _iterate($(part[p]), sequence, till, $(pposi[p]), $(pposi[p+1]), @inbounds $(substate[p]))
        if $(subresult[p]) === nothing
        prune_captures(sequence,$(pposi[p]))
        @inbounds $(substate[p]) = nothing
        $(pposi[p+1]) = $(pposi[p])
           @goto $(p == 1 ? :theend : subsearch[p-1])
        else
            $(pposi[p+1]) = tuple_pos($(subresult[p]))
            @inbounds $(substate[p]) = tuple_state($(subresult[p]))
        ##$(pposi[p+1]), $(substate[p]) = $(subresult[p])
            $(if p < length(fpts); (:(@inbounds $((substate[p+1]))=nothing)); end )
        end
        end
        for (p,t) in enumerate(fpts)
    ]
    R = quote
        $(init...)
        $(pposi[1]) = posi
        $(pposi[end]) = next_i
        states !== nothing && @goto $(subsearch[end])
        $(parseparts...)
        return $(pposi[end]), $ret_state
        @label theend
        return nothing
    end
    R
end

(*)(x::Any, y::AbstractToken) = sSequence(parser(x),y)
(*)(x::AbstractToken, y::Any) = sSequence(x,parser(y))
(*)(x::AbstractToken, y::AbstractToken) = sSequence(x,y)



regex_inner(x::Sequence)  = join([ regex_string(p) for p in x.parts])

export Lazy
"""
    Lazy(x::Repeat)
    Lazy(x::Optional)

Lazy `x` repetition matching from greedy to lazy.

```jldoctest
julia> re"a+?"
a+?  |> Repeat |> Lazy
::Array{Char,1}

julia> re"a??"
a??  |> Optional(default=missing) |> Lazy
::Union{Missing, Char}
```
"""
@auto_hash_equals struct Lazy{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    Lazy(p_) =
        let p = parser(p_)
            new{typeof(p),state_type(p),result_type(p)}(p)
        end
end

deepmap_parser(f::Function,mem::AbstractDict,x::Lazy,a...;kw...) =
    get!(mem,x) do
        Lazy(deepmap_parser(f,mem,x.parser,a...;kw...))
    end

regex_inner(x::Lazy) = regex_inner(x.parser)
regex_suffix(x::Lazy) = regex_suffix(x.parser)*"?"

function print_constructor(io::IO, x::Lazy)
    print_constructor(io,x.parser)
    print(io, " |> Lazy" )
end

const Repeat_max = 10^6
export Repeat1, Repeat
"""
    Repeat(x)
    Repeat(x; min=0,max=Repeat_max)
    Repeat(min::Integer, x)
    Repeat(min::Integer,max::Integer, x)

Parser repeating pattern `x` `min:max` times.
"""
@auto_hash_equals struct Repeat{P,S,T} <: WrappedParser{P,S,T}
    range::UnitRange{Int}
    parser::P
    Repeat(range::UnitRange{Int},p::P) where {P<:AbstractToken} =
        new{P,repeat_state_type(state_type(p)),Vector{result_type(P)}}(range,p)
    Repeat(p::P) where {P<:AbstractToken} =
        new{P,repeat_state_type(state_type(p)),Vector{result_type(P)}}(0:Repeat_max,p)
end
Repeat(range::UnitRange{Int},p::ParserTypes...) =
    Repeat(range,sSequence(p...))
Repeat(min::Integer,max::Integer,p::ParserTypes...) =
    Repeat((min:max),p...)
Repeat(p::ParserTypes...;min::Integer=0,max::Integer=Repeat_max) =
    Repeat((min:max),p...)
Repeat(min::Integer,p::ParserTypes...) =
    Repeat((min:Repeat_max),p...)

@inline repeat_state_type(::Type{MatchState}) = Int
@inline repeat_state_type(T::Type) =
    Vector{T}

"""
    Repeat(f::Function,a...)

Abbreviation for `map(f,Repeat(a...))`.
"""
Repeat(f::Function,a...;kw...) =
    map(f,Repeat(a...;kw...))

"""
    Repeat1(x)

Parser repeating pattern `x` one time or more.
"""
Repeat1(x) =
    Repeat(1,x)
"""
    Repeat1(f::Function,a...)

Abbreviation for `map(f,Repeat1(a...))`.
"""
Repeat1(f::Function,a...) =
    map(f,Repeat1(a...))

@deprecate Repeat(minmax::Tuple{<:Integer,<:Integer},x::ParserTypes,y::Vararg{ParserTypes}) Repeat(minmax...,Sequence(x,y...))

@deprecate Repeat(transform::Function, T::Type, a...) map(transform, T, Repeat(a...))

@deprecate Repeat(transform::Function, minmax::Tuple{<:Integer,<:Integer}, a...) map(transform, Repeat(minmax..., a...))

@deprecate Repeat(T::Type, x, minmax::Tuple{<:Integer,<:Integer}=(0,Repeat_max); transform) map(transform,T, Repeat(minmax...,parser(x)))

@deprecate rep(a...;kw...) Repeat(a...;kw...)

import Base.join

"""
    Base.join(x::Repeat,delim)

Parser matching repeated `x.parser` separated by `delim`.
```jldoctest
julia> parse(join(Repeat(AnyChar()),','),"a,b,c")
3-element Array{Char,1}:
 'a'
 'b'
 'c'
```
"""
function Base.join(x::Repeat,delim_)
    delim = parser(delim_)
    ## todo: the get function could be optimized
    ##@show x.range
    map(x.parser * Repeat(
        max(0,x.range.start-1),
        x.range.stop == Repeat_max ? Repeat_max : x.range.stop-1,
        Sequence(2, delim_,x.parser ))) do (f,r)
            pushfirst!(r,f)
            r::result_type(x)
        end
end

"""
    Base.join(x::Repeat,delim)

Shorthand for `join(Repeat(x),delim)`.
"""
Base.join(x::CombinedParser,delim) = join(Repeat(x),delim)

"""
    Base.join(f::Function, x::CombinedParser, delim)

Shorthand for `map(f,join(x,delim))`.
"""
Base.join(f::Function,p::CombinedParser,delim_) =
    map(f,join(p,delim_))

function print_constructor(io::IO,x::Repeat)
    print_constructor(io,x.parser)
    print(io, " |> Repeat" )
end

regex_inner(x::Repeat) = regex_inner(x.parser)
regex_suffix(x::Repeat) = 
    regex_suffix(x.parser)*if x.range.start == 0
        if x.range.stop == Repeat_max
            "*"
        else            
            "{,$(x.range.stop)}"
        end
    else
        if x.range.stop == Repeat_max
            if x.range.start == 1
                "+"
            else
                "{$(x.range.start),}"
            end
        elseif x.range.start==x.range.stop
            "{$(x.range.start)}"
        else
            "{$(x.range.start),$(x.range.stop)}"
        end
    end


revert(x::Repeat) = x
deepmap_parser(f::Function,mem::AbstractDict,x::Repeat,a...;kw...) =
    get!(mem,x) do
        f(Repeat(x.range,
                 deepmap_parser(f,mem,x.parser,a...)),a...;kw...)
    end




@inline function prevind(str,i::Int,parser::Repeat,x::Int)
    for e in 1:x
        i=prevind(str,i,parser.parser,MatchState())
    end
    i
end

@inline function nextind(str,i::Int,parser::Repeat,x::Int)
    for e in 1:x
        i=nextind(str,i,parser.parser,MatchState())
    end
    i
end

@inline function nextind(str,i::Int,parser::Repeat,x::Vector)
    for e in x
        i=nextind(str,i,parser.parser,e)
    end
    i
end

@inline function prevind(str,i::Int,parser::Repeat,x::Vector)
    for j in lastindex(x):-1:1
        @inbounds i=prevind(str,i,parser.parser,x[j])
    end
    i
end


@inline emptystate(::Type{Int}) = 0
@inline state_length(parser,state::Int) = state
@inline pushstate!(state::Int,parser,substate::MatchState) =
    state + 1
@inline poplast!(state::Int,parser) =
    if iszero(state)
        nothing, 0
    else
        MatchState(), state - 1
    end

@inline state_length(parser::Repeat,x::Vector) =
    length(x)
@inline emptystate(::Type{Vector{T}}) where T =
    T[]

@inline function pushstate!(state::Vector,parser,substate)
    push!(state,substate)
end
@inline function poplast!(state::Vector,parser)
    l=pop!(state)
    l,state
end

## kernel function (function barrier)
@inline function fill_rep_j_state(x::Tuple{Int,S},state_,tparser) where S
    pos, state = x
    pos, pushstate!(state_,tparser, state)
end

@inline function fill_rep(t::Repeat, sequence, till::Int, i::Int,state) 
    j_::Int = -1
    j::Int = i
    state_ = state
    tp = t.parser
    while state_length(t,state_) < t.range.stop && ( x = _iterate(t.parser,sequence, till, j, j,nothing) )!==nothing
        ## @info "rep fill..." x state_
        ## e.g. match(re"(?:a|(?=b)|.)*\z","abc")
        j_=j
        ##j, state_ = fill_rep_j_state(x,state_,tp)
        j, state_ = fill_rep_j_state(x,state_,tp)
        state_length(t,state_)>t.range.start && j_==j && break
    end
    j,state_,state_length(t,state_) < t.range.start
end

## used by Repeat
function push_rep(t,sequence, till, posi, x::Nothing, state_)
    posi, state_, !iszero(state_length(t,state_))
end

## used by Repeat
function push_rep(t,sequence, till, posi, x::Tuple{Int,S}, state_) where S
    p,s = x
    if posi==p
        posi, state_, true
    else
        fill_rep(t,sequence,till,p,pushstate!(state_,t.parser,s))
    end
end


## used by Lazy{Repeat}
@inline function pushstate!_fill_rep(t_, sequence, till, state_, x)
    t = t_.parser
    state_=pushstate!(state_,t.parser,tuple_state(x))
    fill_rep(t_,sequence,till,tuple_pos(x),state_)
end

function _iterate(t::Repeat, sequence, till, posi, next_i, state)
    next_i_::Int,state_::state_type(typeof(t)),goback::Bool = if state === nothing
        es = emptystate(state_type(typeof(t)))
        fill_rep(t,sequence,till,next_i, es)
    else
        if state_length(t,state)==0
            return nothing
            # https://www.pcre.org/original/doc/html/pcrepattern.html:
            # It is possible to construct infinite loops by following
            # a subpattern that can match no characters with a
            # quantifier that has no upper limit, for example:
            
            #   (a?)*
            
            # Earlier versions of Perl and PCRE used to give an error
            # at compile time for such patterns. However, because
            # there are cases where this can be useful, such patterns
            # are now accepted, but if any repetition of the
            # subpattern does in fact match no characters, the loop is
            # forcibly broken.
        #     return nothing
        end
        next_i, state, true
    end
    while goback
        if state_length(t,state_)==0
            return nothing
        end
        lstate, state_=poplast!(state_,t.parser)
        posi = _prevind(sequence,next_i_,t.parser,lstate) ##state[end][1]
        prune_captures(sequence,posi)
        x = _iterate(t.parser,sequence, till, posi, next_i_, lstate)
        x === nothing && state_length(t,state_) in t.range && return posi, state_
        next_i_,state_,goback = push_rep(t,sequence, till, posi, x, state_)
    end
    if state_length(t,state_) in t.range
        next_i_, state_
    else
        nothing
    end
end

@inline function fill_rep(t_::Lazy{<:Repeat}, sequence, till::Int, j::Int, state_)
    t = t_.parser
    tp = t.parser
    while state_length(t,state_) < t.range.start && (x = _iterate(t.parser,sequence, till,j, j,nothing))!==nothing 
        j_=j
        j, state_ = fill_rep_j_state(x,state_,tp)
        j_==j && break
    end
    j,state_,false
end
function _iterate(t_::Lazy{<:Repeat}, sequence, till, posi, next_i, state)
    t = t_.parser
    next_i_::Int,state_::state_type(typeof(t)),goback::Bool = if state === nothing
        es = emptystate(state_type(typeof(t)))
        fill_rep(t_,sequence,till,next_i, es)
    else
        if state_length(t,state)<t.range.stop
            x = _iterate(t.parser,sequence, till, next_i, next_i, nothing)
            if x!==nothing && ( tuple_pos(x)>next_i || state_length(t,state)==0)
                return fill_rep_j_state(x,state,t.parser) #tuple_pos(x),pushstate!(state,t.parser,tuple_state(x))
            end
        end
        next_i, state, true
    end

    while goback
        if state_length(t,state)==0
            return nothing
        end
        lstate, state_=poplast!(state,t.parser)
        posi = prevind(sequence,next_i_,t.parser,lstate) ##state[end][1]
        x = _iterate(t.parser,sequence, till, posi, next_i_, lstate)
        if x === nothing
            next_i_ = posi
            prune_captures(sequence,next_i_)
            if state_length(t,state_)==0
                return nothing
            end
            state = state_
        else
            next_i_,state_ = pushstate!_fill_rep(t_, sequence, till, state_, x)
            if state_length(t,state_) in t.range
                goback = false
            end
        end
    end
    if state_length(t,state_) in t.range ## && state_length(t,state_)>0
        return next_i_, state_
    else
        nothing
    end
end











export Optional
"""
State type for skipped optional. (Missing was breaking julia).
"""
struct None end
Base.show(io::IO, ::None) = print(io,"n/a")
"""
    Optional(parser;default=defaultvalue(result_type(parser)))
    
Parser that always succeeds. 
If parser succeeds, return result of `parser` with curser behind match.
If parser does not succeed, return `default` with curser unchanged.

```jldoctest
julia> match(r"a?","b")
RegexMatch("")

julia> parse(Optional("a", default=42),"b")
42
```
"""
@auto_hash_equals struct Optional{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    default::T
    function Optional(p_;default=defaultvalue(result_type(p_)))
        p = parser(p_)
        T = result_type(p)
        D = typeof(default)
        T_ = promote_type(T,D)
        T_ === Any && ( T_ = Union{T,D} )
        new{typeof(p),Union{None,state_type(p)},T_}(p, default)
    end
end



Optional(p_::NamedParser;kw...) =
    with_name(p_.name,Optional(p_.parser;kw...))


Optional(x1,x...;kw...) = Optional(Sequence(x1,x...);kw...)


Optional(T::Type, x_; transform, kw...) =
    Optional(transform, T, x; kw...)

function Optional(transform::Function, T::Type, x;
             default=defaultvalue(T))
    map(transform,T,Optional(x; default=default))
end



children(x::Optional) = children(x.parser)
regex_inner(x::Optional) = regex_inner(x.parser)
regex_suffix(x::Optional) = regex_suffix(x.parser)*"?"

function print_constructor(io::IO, x::Optional)
    print_constructor(io,x.parser)
    print(io, " |> Optional(default=$(x.default))")
end
deepmap_parser(f::Function,mem::AbstractDict,x::Optional,a...;kw...) =
    get!(mem,x) do
        Optional(deepmap_parser(f,mem,x.parser,a...;kw...);
                 default=x.default)
    end

@inline prevind(str,i::Int,parser::Optional,x::None) = i
@inline nextind(str,i::Int,parser::Optional,x::None) = i



_iterate(t::Optional, str, till, posi, next_i, state::None) =
    nothing

function _iterate(t::Optional, str, till, posi, next_i, state)
    posi = state === nothing ? next_i : prevind(str,next_i,t.parser,state) ##state[end][1]
    r = _iterate(t.parser, str, till, posi, next_i, state)
    if r === nothing
        prune_captures(str,posi)
        return tuple(posi, None())
    else
        r
    end
end

function _iterate(t_::Lazy{<:Optional}, str, till, posi, next_i, state)
    t=t_.parser
    if state === nothing
        next_i,None()
    else 
        r = _iterate(t.parser, str, till, posi, next_i,
                     state === None() ? nothing : state)
        if r === nothing
            nothing
        else
            r
        end            
    end
end


defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type) = missing
defaultvalue(V::Type{<:CombinedParser}) = Always()





using Tries 
export alt, Either
"""
    Either(parsers...)
    
Parser that tries matching the provided parsers in order, accepting the first match, and fails if all parsers fail.

This parser has no custom `==` and `hash` methods because it can recurse.

```jldoctest
julia> match(r"a|bc","bc")
RegexMatch("bc")

julia> parse(Either("a","bc"),"bc")
"bc"

julia> parse("a" | "bc","bc")
"bc"

```
"""
struct Either{Ps,S,T} <: CombinedParser{S,T}
    options::Ps
    Either{T}(p::P) where {T,P<:Union{Vector,Tuple,Trie}} =
        new{P,either_state_type(P),T}(p)
    Either{T}(p::CombinedParser...) where {T} =
        new{Vector{Any},either_state_type(CombinedParser),T}(Any[p...])
    function Either(x::Vector{<:AbstractString})
        P = Trie{Char,Nothing}
        r = P()
        for e in x
            r[e...] = nothing
        end
        new{P,either_state_type(P),String}(r)
    end

    function Either(x::Dict)
        P = Trie{Char,valtype(x)}
        r=P()
        for (e,v) in pairs(x)
            r[e...] = v
        end
        new{P,either_state_type(P),valtype(x)}(r)
    end
end
either_state_type(ts::Type{Vector{Any}}) = Tuple{Int,Any}
either_state_type(ts::Type{CombinedParser}) = Tuple{Int,Any}
either_state_type(ts::Type{<:Vector}) = Tuple{Int,state_type(eltype(ts))}
either_state_type(ts::Type{<:Tuple}) = Tuple{Int,promote_type(state_type.(fieldtypes(ts))...)}
either_state_type(ts...) = Tuple{Int,promote_type(state_type.(ts))}
either_state_type(T::Type{<:Trie}) = NCodeunitsState{T}
@inline with_state!(x::Nothing,k::Int,s) = (k,s)
function promote_type_union(Ts...)
    T = promote_type(Ts...)
    Any <: T ? Union{Ts...} : T
end

"return tuple(state_type,result_type)"
function either_types(ts)
    promote_type_union(result_type.(ts)...)
end

function Either(x::Vector)
    parts = [ parser(y) for y in x ]
    T = either_types(typeof.(parts))
    Either{T}(parts)
end
function Either(x...)
    parts = [ parser(y) for y in x  ]
    T = either_types(typeof.(parts))
    Either{T}(parts)
end

children(x::Either) = x.options
regex_string(x::Either) = join(regex_string.(x.options),"|")
regex_prefix(x::Either) = "|"
regex_inner(x::Either) = join([ regex_string(p) for p in x.options],"|")
regex_suffix(x::Either) = "..."
print_constructor(io::IO,x::Either) = print(io,"Either")




"""
    Either(transform::Function, x::Vararg)

abbreviation for `map(transform, Either(x...))`.
"""
function Either(transform::Function, x::Vararg)
    map(transform, Either(x...))
end

function deepmap_parser(f::Function,mem::AbstractDict,x::Either{<:Vector},a...;kw...)
    if haskey(mem,x)
        mem[x]
    else
        mem[x] = r = Either{result_type(x)}(Any[])
        ## f(x,a...)
        for p in x.options
            push!(r,deepmap_parser(f,mem,p,a...;kw...))
        end
        r
    end
end

function deepmap_parser(f::Function,mem::AbstractDict,x::Either{<:Tuple},a...;kw...)
    get!(mem,x) do
        Either{result_type(x)}(tuple( (deepmap_parser(f,mem,p,a...;kw...) for p in x.options)... ))
    end
end


export sEither
sEither_(x::Either) = sEither_(x.options...)
sEither_(x::Never) = tuple()
sEither_() = tuple()
sEither_(x1) = tuple(x1)
sEither_(x1,x...) = Iterators.flatten( Any[ sEither_(x1), ( sEither_(e) for e in x )... ] )
"""
    sEither(x...)

Simplifying `Either`, flattens nested `Either`s, remove `Never` parsers.

```jldoctest
julia> Either('a',CharIn("AB")|"bc")
|🗄... Either
├─ a
└─ |🗄... Either
   ├─ [AB] CharIn
   └─ bc
::Union{Char, SubString}


julia> sEither('a',CharIn("AB")|"bc")
|🗄... Either
├─ a
├─ [AB] CharIn
└─ bc
::Union{Char, SubString}

```

See also [`Either`](@ref)
"""
function sEither(x...)
    opts = collect(sEither_(x...))
    length(opts)==1 ? opts[1] : Either(opts...)
end



(|)(x, y::ParserTypes) = sEither(parser(x),y)
(|)(x::ParserTypes, y) = sEither(x,parser(y))
"""
    (|)(x::AbstractToken, y)
    (|)(x, y::AbstractToken)
    (|)(x::AbstractToken, y::AbstractToken)

Operator syntax for `sEither(x, y)`.

```jldoctest
julia> 'a' | CharIn("AB") | "bc"
|🗄... Either
├─ a
├─ [AB] CharIn
└─ bc
::Union{Char, SubString}

```

"""
(|)(x::ParserTypes, y::ParserTypes) = sEither(x,y)

"""
    (|)(x::AbstractToken{T}, default::Union{T,Missing})

Operator syntax for `Optional(x, default=default)`.

```jldoctest
julia> parser("abc") | "nothing"
|🗄... Either
├─ abc
└─ nothing
::SubString

```

"""
function (|)(x::AbstractToken{T}, default::Union{T,Missing}) where { T }
    Optional(x,default=default)
end
function (|)(x::Char, y::Char)
    CharIn(tuple(x,y))
end
function (|)(x::CharIn, y::Char)
    CharIn(tuple(x.sets...,y))
end


"""
    `(|)(x::Either, T::Type)`

Return new Either with `T` added to result_type(x).
todo: Note that the options array is kept. As a consequence `push!`on result will also push to `x`.
"""
(|)(x::Either, T::Type) =
    Either{Union{result_type(x),T}}(x.options)

"""
    Base.push!(x::Either, option)

Push `option` to `x.options` as parser tried next if `x` fails.
Recursive parsers can be built with `push!` to `Either`.

See also [`pushfirst!`](@ref).
"""
function Base.push!(x::Either{<:Vector,<:Any}, y)
    result_type(y) <: result_type(x) || error("$(result_type(y)) <: $(result_type(x)). Fix with `push!(x|$(typeof(y)),y)`.")
    push!(x.options,y)
    x
end

"""
    Base.pushfirst!(x::Either, option)

Push `option` to `x.options` as parser tried first, and trying `x` if option fails.
Recursive parsers can be built with `pushfirst!` to `Either`.

See also [`push!`](@ref).
"""
function Base.pushfirst!(x::Either{<:Vector,<:Any}, y)
    result_type(y) <: result_type(x) || error("$(result_type(y)) <: $(result_type(x)). Fix with `push!(x|$(typeof(y)),y)`.")
    pushfirst!(x.options,y)
    x
end

Base.push!(x::Repeat{<:Either}, y) = push!(x.parser,y)
Base.pushfirst!(x::Repeat{<:Either}, y) = pushfirst!(x.parser,y)

function Base.push!(x::NamedParser{<:Either}, y)
    push!(x.parser,y)
    x
end
function Base.pushfirst!(x::NamedParser{<:Either}, y)
    pushfirst!(x.parser,y)
    x
end



mutable struct MutablePair{K,V}
    first::K
    second::V
    MutablePair{K,V}(f,s) where {K,V} =
        new{K,V}(f,s)
    MutablePair(f,s) =
        new{typeof(f),typeof(s)}(f,s)
end
Base.show(io::IO, x::MutablePair) =
    print(io, x.first, "=>", x.second)
@inline function with_state!(x::MutablePair,s)
    ##s isa Tuple{Int,Nothing} && error()
    x.second=s
    x
end

@inline function with_state!(x::MutablePair,k,s)
    ##s isa Tuple{Int,Nothing} && error()
    x.first=k
    x.second=s
    x
end
@inline function with_state!(x::Pair,s)
    Pair(x.first,s)
end
@inline function with_state!(x::Pair,k,s)
    Pair(k, s)
end

@inline function with_state!(x::Tuple,k,s)
    (k, s)
end

either_state_option(::Nothing) = 1
either_state_state(x::Nothing) = nothing
either_state_option(x::Tuple) = x[1]
either_state_state(x::Tuple) = x[2]
either_state_option(x::Union{Pair,MutablePair}) = x.first
either_state_state(x::Union{Pair,MutablePair}) = x.second

@inline function prevind(str,i::Int,parser::Either,x)
    ## @show i
    prevind(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end

@inline function nextind(str,i::Int,parser::Either,x)
    ## @show i
    nextind(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end
@inline function nextind(str,i::Int,parser::Either{P,T},x::Tuple{Int,T}) where {P,T}
    nextind(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end
 

@generated function prevind(str,i::Int,parser::Either{pts},x::Union{Pair,MutablePair}) where {pts<:Tuple}
    fpts = fieldtypes(pts)
    parseoptions = [
        quote
        if j === $p
        return prevind(str,i,parser.options[$p],s) # $(part[p]),s)
        end
        end
        for (p,t) in enumerate(fpts)
    ]
    R = quote
        j = x.first
        s = x.second
        $(parseoptions...)
        error("?")
    end
    R
end


@inline function __iterate_paired(first,state,sstate::Nothing)
    nothing
end

@inline function __iterate_paired(first, state, sstate::Tuple)
    __iterate_paired(first, state, sstate...)
end

@inline function __iterate_paired(first, state, next_i_::Int, nstate_)
    next_i_, with_state!(state,first,nstate_)
end

function _iterate_paired(first, t, str, till, posi, next_i, state)
    __iterate_paired(first, state, _iterate(t, str, till, posi, next_i, either_state_state(state)))
end

function _iterate(t::Either{<:Vector}, str, till, posi, next_i, state::Nothing)
    r = nothing
    for (j,o) in enumerate(t.options)
        r = _iterate_paired(j,o,str,till,posi, next_i,nothing)
        r!== nothing && return r
    end
    nothing
end

function _iterate(t::Either{<:Vector}, str, till, posi, next_i, state)
    @inbounds opt = t.options[either_state_option(state)]
    fromindex = either_state_option(state)+1
    posi = _prevind(str,next_i,opt,either_state_state(state)) ##state[end][1]
    r = _iterate_paired(either_state_option(state),opt,str,till,posi, next_i,state)
    r !== nothing && return r
    prune_captures(str,posi)
    ##sstate = nothing
    for j in fromindex:length(t.options)
        @inbounds r2 = _iterate_paired(j,t.options[j],str,till,posi,posi,nothing)
        r2 !== nothing && return r2
    end
    nothing
end


function _iterate(parser::Either{<:Tuple}, sequence, till, posi, next_i, state)
    either_first(parser,posi,next_i,state) do index, option, ni, sstate
        _iterate_paired(index, option, sequence, till, posi, ni, sstate)
    end
end



@generated function either_first(f::Function, parser::Either{pts}, posi, next_i, state) where {pts<:Tuple}
    fpts = fieldtypes(pts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    push!(subsearch, gensym(:subsearch))
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    init = Expr(:(=), Expr(:tuple, part...),:(parser.options))
    parseoptions = [
        quote
        @label $(subsearch[p])
        j > $p && @goto $(subsearch[p+1])
        $(subresult[p]) = f($p,$(part[p]), next_i_, sstate)
        $(subresult[p]) !== nothing && return $(subresult[p])# __iterate_paired($p,state, $(subresult[p]))
        next_i_ = posi
        sstate = nothing
        end
        for (p,t) in enumerate(fpts)
    ]
    init_before = 
        quote
            j = either_state_option(state)
            sstate = state
        end
    R = quote
        next_i_::Int = next_i
        $(init)
        $(init_before)
        $(parseoptions...)
        @label $(subsearch[end])
        return nothing
    end
    R
end

import Tries: nodes, AbstractTrie
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
    _iterate(p, str, prevind(str,next_i,2), posi, posi, nothing)
end

function _iterate(p::Either{<:Trie}, str, till, posi, next_i, state)
    _iterate(p.options, str, till, posi, next_i, state)
end

@inline nextind(str,i::Int,parser::Either{<:Trie},x::NCodeunitsState)  = i+x.nc
@inline prevind(str,i::Int,parser::Either{<:Trie},x::NCodeunitsState)  = i-x.nc

children(x::Either{<:Trie}) =
    children(x.options)

export Atomic
"""
    Atomic(x)

A parser matching `p`, and failing when required to backtrack
(behaving like an atomic group in regular expressions).
"""
@auto_hash_equals struct Atomic{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    Atomic(x) =
        let p=parser(x)
            new{typeof(p),state_type(p),result_type(p)}(p)
        end
end

deepmap_parser(f::Function,mem::AbstractDict,x::Atomic,a...;kw...) =
    get!(mem,x) do
        Atomic(
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end

@inline function _iterate(parser::Atomic, sequence, till, posi, next_i, state::Nothing)
    _iterate(parser.parser, sequence, till, posi, next_i, state)
end
@inline function _iterate(parser::Atomic, sequence, till, posi, next_i, state)
    nothing
end

function print_constructor(io::IO,x::Atomic)
    print_constructor(io,x.parser)
    print(io, " |> Atomic" )
end

regex_prefix(x::Atomic) = "(?>"*regex_prefix(x.parser)
regex_suffix(x::Atomic) = regex_suffix(x.parser)*")"
regex_inner(x::Atomic) = regex_inner(x.parser)



include("match.jl")




export _iterate

export Numeric
Numeric = TextParse.Numeric

deepmap_parser(f::Function,mem::AbstractDict,x::Numeric,a...; kw...) = x

include("reverse.jl")
include("textparse.jl")
include("get.jl")
include("re.jl")


include("show.jl")

include("operators.jl")


export optimize
optimize(x) = deepmap_parser(_optimize,x)

_optimize(x,a...) = x
deepmap_parser(::typeof(_optimize),dict::AbstractDict,x::SideeffectParser) = x.parser
end # module



