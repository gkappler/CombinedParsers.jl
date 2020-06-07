# TODO:
# - remove after from get (nextind with state and i)
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

@inline _prevind(str,i,parser,x::Nothing) = i
@inline _nextind(str,i,parser,x::Nothing) = i
@inline _prevind(str,i,parser,x) = prevind(str,i,parser,x)
@inline _nextind(str,i,parser,x) = nextind(str,i,parser,x)

"""
    CombinedParser{T} <: AbstractToken{T}

Abstract parser type for parsers returning matches transformed to `::T`.
"""
abstract type CombinedParser{T} <: AbstractToken{T} end


"""
    LeafParser{T} <: AbstractToken{T}

Abstract parser type for parsers that have no sub-parser.
Used for dispatch in [`deepmap_parser`](@ref)
"""
abstract type LeafParser{T} <: CombinedParser{T} end


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
"Julia types that can convert(AbstractToken,x). TODO: remove"
ParserTypes = Union{AbstractToken, AbstractString, Char, Regex,
                    Pair{<:Union{AbstractToken, AbstractString, Char, Regex, Pair},<:Any}}

export parser
import Base: convert
"""
calls `convert(AbstractToken,x)`.
"""
parser(x) = Base.convert(AbstractToken, x)
parser(x::Regex) = x

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


############################################################
## Parsing with AbstractToken

state_type(p::Type{<:AbstractToken}) = Tuple{Int,result_type(p)}

function _iterate(parser::AbstractToken, sequence, till, i, state)
    parser isa CombinedParser && @warn "define _iterate(parser::$(typeof(parser)), sequence, till, i, state)"
    ##@show parser typeof(parser)
    if state === nothing
        r,i_ = tryparsenext(parser, sequence, i, till)
        if isnull(r)
            nothing
        else
            i_,(i_-i,get(r))
        end
    else
        nothing
    end
end

function Base.get(parser::AbstractToken, sequence, till, after, i, state)
    state[2]
end

@inline function nextind(str,i::Int,parser::AbstractToken,x)
    parser isa CombinedParser && @warn "define nextind(str,i::Int,parser::$(typeof(parser)),x)"
    i+x[1]
end
@inline function prevind(str,i::Int,parser::AbstractToken,x)
    parser isa CombinedParser && @warn "define prevind(str,i::Int,parser::$(typeof(parser)),x)"
    i-x[1]
end

function Base.get(parser::CombinedParser, sequence, till, after, i, state)
    error("define Base.get(parser::$(typeof(parser)), sequence, till, after, i, state)")
end
function nextind(str,i::Int,parser::CombinedParser,x)
    error("define nextind(str,i::Int,parser::$(typeof(parser)),x)")
end
function prevind(str,i::Int,parser::CombinedParser,x)
    error("define prevind(str,i::Int,parser::$(typeof(parser)),x)")
end

result_type(x::ParserTypes) = result_type(typeof(x))
result_type(T::Type{<:Union{Char,AbstractString}}) = T
result_type(::Type{<:AbstractToken{T}}) where T = T

struct MatchState end
"""
    Base.get(parser::CombinedParser{Nothing}, sequence, till, after, i, state)

Default method for parser types returning nothing
"""
Base.get(parser::CombinedParser{Nothing}, sequence, till, after, i, state) =
    nothing
state_type(p::Type{<:CombinedParser}) =  error("implement state_type(::Type{$(p)})")
_iterate(x::CombinedParser,str,i,till,state) =
    error("implement _iterate(x::$(typeof(x)),str::$(typeof(str)),i,till,state::$(typeof(state)))")

"Abstract type for parser wrappers, providing default methods"
abstract type WrappedParser{P,T} <: CombinedParser{T} end

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
state_type(::Type{<:WrappedParser{P,T}}) where {P,T} = state_type(P)

@inline prevind(str,i::Int,parser::W,x) where {W <: WrappedParser} = prevind(str,i,parser.parser,x)
@inline nextind(str,i::Int,parser::W,x) where {W <: WrappedParser} = nextind(str,i,parser.parser,x)

Base.get(parser::W, sequence, till, after, i, state) where {W <: WrappedParser} = 
    get(parser.parser, sequence, till, after, i, state)
@inline _iterate(parser::W, sequence, till, i, state) where {W <: WrappedParser} =
    _iterate(parser.parser, sequence, till, i, state)


export JoinSubstring
"""
    JoinSubstring(x)
    (!)(x::AbstractToken)

Parser Transformation getting the matched SubString.
"""
@auto_hash_equals struct JoinSubstring{P} <: WrappedParser{P,SubString}
    parser::P
end
Base.map(f::Type{<:JoinSubstring}, p::AbstractToken) = JoinSubstring(p)

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


deepmap_parser(f::Function,mem::AbstractDict,x::JoinSubstring,a...;kw...) =
    get!(mem,x) do
        JoinSubstring(
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end

export map_match
map_match(f::Function,p_) =
    map(f, JoinSubstring(parser(p_)))
Base.get(x::JoinSubstring, sequence, till, after, i, state) =
    SubString(sequence, i, prevind(sequence,after))


"wrapper for stepping with ncodeunit length."
@auto_hash_equals struct ConstantParser{N,P,T} <: WrappedParser{T,T}
    parser::P
    function ConstantParser{N}(x::Char) where N
        new{N,Char,Char}(x)
    end
    function ConstantParser{N}(x::AbstractString) where N
        new{N,typeof(x),SubString}(x)
    end
end
state_type(p::Type{<:ConstantParser}) = MatchState
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


"""
    convert(::Type{AbstractToken},x::Union{AbstractString,Char})

A [`ConstantParser`](@ref) matching `x`.
"""
Base.convert(::Type{AbstractToken},x::Char) =
    ConstantParser{Base.ncodeunits(x)}(x)

"""
    convert(::Type{AbstractToken},x::StepRange{Char,<:Integer})

[`CharIn`](@ref) matching x.
"""
Base.convert(::Type{AbstractToken},x::StepRange{Char,<:Integer}) =
    CharIn(x)
Base.convert(::Type{AbstractToken},x::AbstractString) =
    ConstantParser{Base.ncodeunits(x)}(x)

@inline nextind(str,i::Int,parser::ConstantParser{L},x) where L =
    i+L
@inline prevind(str,i::Int,parser::ConstantParser{L},x) where L = 
    i-L

# Base.get(parser::ConstantParser, sequence, till, after, i, state) = parser.parser

Base.get(parser::ConstantParser{1,Char}, sequence, till, after, i, state) where L =
    sequence[i]

Base.get(parser::ConstantParser{L,<:AbstractString}, sequence, till, after, i, state) where L =
    parser=="" ? "" : SubString(sequence,i,prevind(sequence,i+L))


@inline function _iterate(parser::ConstantParser{L,Char}, sequence, till, i, state::Nothing) where L
    i>till && return nothing
    if ismatch(sequence[i],parser.parser)
        i+L, MatchState()
    else
        nothing
    end
end


@inline function _iterate(parser::ConstantParser{L,<:AbstractString}, sequence, till, i, state::Nothing) where {L}
    j::Int = i
    k::Int = 1
    p = parser.parser
    while k<=L
        (j > till) && return(nothing)
        @inbounds pc,k=iterate(p,k)
        @inbounds sc,j=iterate(sequence,j)
        !ismatch(sc,pc) && return(nothing)
    end
    return j, MatchState()
end

function _iterate(parser::ConstantParser, sequence, till, i, state::Nothing)
    _iterate(parser.parser, sequence, till, i, state)
end


"Abstract type for stepping with previndex/nextindex, accounting for ncodeunit length of chars at point."
abstract type NIndexParser{N,T} <: LeafParser{T} end
state_type(p::Type{<:NIndexParser}) = MatchState
@inline prevind(str,i::Int,parser::Union{NIndexParser{0},ConstantParser{0}},x) =
    i
@inline nextind(str,i::Int,parser::Union{NIndexParser{0},ConstantParser{0}},x) =
    i
@inline prevind(str,i::Int,parser::NIndexParser{L},x) where L =
    prevind(str,i,L)
@inline nextind(str,i::Int,parser::NIndexParser{L},x) where L =
    nextind(str,i,L)
_iterate(parser::Union{NIndexParser,ConstantParser}, sequence, till, i, state::MatchState)  =
    nothing


Base.get(x::NIndexParser{1,Char}, sequence, till, after, i, state) =
    sequence[i]

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


@inline function _iterate(parser::AnyChar, sequence, till, i, state::Nothing)
    i>till && return(nothing)
    @inbounds c,ni = iterate(sequence,i)
    !ismatch(c,parser) && return nothing
    return ni, MatchState()
end


"""
Parsers that do not consume any input can inherit this type.
"""
abstract type LookAround{T} <: NIndexParser{0,T} end
children(x::LookAround) = (x.parser,)

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
_iterate(parser::AtStart, sequence, till, i, state::Nothing) =
    i == 1 ? (i, MatchState()) : nothing

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
_iterate(parser::AtEnd, sequence, till, i, state::Nothing) =
    i > till ? (i, MatchState()) : nothing
print_constructor(io::IO, x::AtEnd) = print(io,"AtEnd")

Base.get(parser::Union{AtStart,AtEnd}, sequence, till, after, i, state) =
    parser



 

export Never
"""
    Never()

Assertion parser matching never.

```jldoctest
julia> Never()
re"(*FAIL)"

```
"""
struct Never <: LeafParser{Never} end
regex_prefix(x::Never) = "(*"
regex_inner(x::Never) = "FAIL"
regex_suffix(x::Never) = ")"
_iterate(x::Never,str,i,till,state) =
    nothing

state_type(::Type{Never}) = MatchState

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
struct Always <: LeafParser{Always}
end
Base.show(io::IO,x::Always) = print(io,"re\"\"")
children(x::Union{Never,Always}) = tuple()
regex_prefix(x::Always) = ""
regex_inner(x::Always) = ""
regex_suffix(x::Always) = ""
state_type(::Type{Always}) = MatchState
Base.get(parser::LeafParser{Always}, sequence, till, after, i, state) =
    Always()
_iterate(parser::Always, str, till, i, s::Nothing) =
    i, MatchState()
_iterate(parser::Always, str, till, i, s::MatchState) =
    nothing
prevind(str,i::Int,p::Always,x) = i
nextind(str,i::Int,p::Always,x) = i
##_iterate(parser::Never, str, till, i, s) = nothing


Base.show(io::IO, x::Union{AtStart,AtEnd,Never,Always}) =
    print(io,"re\"",regex_string(x),"\"")

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
function _iterate(t::PositiveLookahead, str, till, i, state::Nothing)
    r = _iterate(t.parser, str, till, i, nothing)
    if r === nothing
        nothing
    else
        i,MatchState()
    end
end

function Base.get(t::PositiveLookahead, str, till, after, i, state)
    get(t.parser, str, till, after, i, state)
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
function _iterate(t::NegativeLookahead, str, till, i, state::Nothing)
    r = _iterate(t.parser, str, till, i, nothing)
    if r === nothing
        i,MatchState()
    else
        nothing
    end
end
function Base.get(parser::NegativeLookahead, sequence, till, after, i, state)
    parser
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

@auto_hash_equals struct SideeffectParser{P,T,A} <: WrappedParser{P,T}
    parser::P
    args::A
    effect::Function
    SideeffectParser(f::Function, p::AbstractToken,a...) =
        new{typeof(p),result_type(p),typeof(a)}(p,a,f)
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

@inline function _iterate(parser::SideeffectParser, sequence, till, i, state)
    before_i = start_index(sequence,i,parser,state)
    r = _iterate(parser.parser, sequence, till, i, state)
    if r!==nothing
        parser.effect(sequence,before_i,r...,parser.args...)
    else
        parser.effect(sequence,before_i,before_i,nothing,parser.args...)
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
@auto_hash_equals struct NamedParser{P,T} <: WrappedParser{P,T}
    name::Symbol
    parser::P
    doc::String
    NamedParser(name::Symbol,p_,doc="") =
        let p=parser(p_)
            new{typeof(p),result_type(p)}(name,p,doc)
        end
end
children(x::NamedParser) = children(x.parser)
function print_constructor(io::IO,x::NamedParser)
    print_constructor(io,x.parser)
    print(io, " |> with_name(:")
    printstyled(io, x.name, bold=true,color=:red)
    print(io, ")")
end

"""
    convert(::Type{AbstractToken},x::Pair{Symbol, P}) where P

A parser labelled with name `x.first`.
Labels are useful in printing and logging.

See also: [`@with_names`](@ref), [`with_name`](@ref), [`log_names`](@ref)
"""
Base.convert(::Type{AbstractToken},x::Pair{Symbol, P}) where P =
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

export Transformation
"""
    Transformation{T}(transform::Function, p_) where {T}
    map_at(f::Function, p, a...)
    map_at(f::Function, Tc::Type, p, a...)
    instance(Tc::Type, p::ParserTypes, a...)
    instance(Tc::Type, p::ParserTypes)
    Base.map(f::Function, Tc::Type, p::ParserTypes, a...)
    Base.map(f::Function, p::ParserTypes, a...)

Parser transforming result of a wrapped parser. 
`a...` is passed as additional arguments to `f` (at front .
"""
@auto_hash_equals struct Transformation{F,P,T} <: WrappedParser{P,T}
    transform::F
    parser::P
    Transformation{T}(transform, p_) where {T} =
        let p = parser(p_)
            new{typeof(transform),typeof(p),T}(transform, p)
        end
    Transformation{T}(transform, p_::NamedParser) where {T} =
        let p = p_.parser
            tp = new{typeof(transform),typeof(p),T}(transform, p)
            with_name(p_.name,tp)
        end
end
deepmap_parser(f::Function,mem::AbstractDict,x::Transformation,a...;kw...) =
    get!(mem,x) do
        Transformation{result_type(x)}(
            x.transform,
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end


struct Constant{T}
    value::T
end

"""
    Base.get(parser::Transformation, a...)

Constant value `parser.transform` fallback method.
"""
function Base.get(parser::Transformation{<:Constant}, sequence, till, after, i, state)
    parser.transform.value
end
function map_constant(transform, p::AbstractToken)
    T=typeof(transform)
    Transformation{T}(Constant(transform), p)
end
"""
    convert(::Type{AbstractToken},constant::Pair{<:ParserTypes})

A parser mapping matches of `x.first` to constant `x.second`.

See also: [`map`](@ref), [`map_at`](@ref)
"""
Base.convert(::Type{AbstractToken},constant::Pair{<:ParserTypes}) =
    map_constant(constant.second, parser(constant.first))

function _string(io::IO,x::Constant)
    print(io,"Constant(")
    show(io,x.value)
    print(io,")")
end
_string(io::IO,x::Function) = print(io,x)
children(x::Transformation) = children(x.parser)
function print_constructor(io::IO,x::Transformation)
    print_constructor(io,x.parser)
    print(io," |> map(")
    _string(io,x.transform)
    print(io,")")
end

"""
    Base.get(parser::Transformation{<:Function}, a...)

Function call `parser.transform(get(parser.parser,a...),i)`.
"""
function Base.get(parser::Transformation{<:Function}, sequence, till, after, i, state)
    v = get(parser.parser, sequence, till, after, i, state)
    parser.transform(v,i)
end

export IndexAt
struct IndexAt{I}
    i::I
end
# @inline Base.getindex(x,i::IndexAt) = getindex(x,i.i...)
_string(io::IO,x::IndexAt) = print(io,"IndexAt(",x.i,")")

"""
    Base.get(parser::Transformation{<:IndexAt}, a...)

`getindex(get(parser.parser,a...).parser.transform)`
"""
function Base.get(parser::Transformation{IndexAt{I}}, sequence, till, after, i, state) where {I <: Integer}
    v = get(parser.parser,sequence, till, after, i, state)
    v[parser.transform.i]
end
function Base.get(parser::Transformation{IndexAt{Is}}, sequence, till, after, i, state) where {Is <: Union{Tuple, Vector, UnitRange}}
    tuple(get(parser.parser,sequence, till, after, i, state)[parser.transform.i]...)
end

function _iterate(parser::Transformation, sequence, till, i, state)
    r = _iterate(parser.parser, sequence, till, i, state )
end

function infer_result_type(f::Function,Tc::Type,p::ParserTypes,onerror::AbstractString,ts::Type...)
    Ts = Base.return_types(f, tuple(result_type(p),ts...))
    isempty(Ts) && error("transformation type signature mismatch $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    ( length(Ts) > 1 || Any <: first(Ts) ) && return Tc ##error(onerror*"  $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    T = first(Ts)
    if T <: Tc
        T
    else
        @warn "type mismatch $f$(tuple(result_type(p),ts...))::$T<:$Tc"
        Tc
    end
end

export map,map_at 
"""
    map_at(f::Function, p, a...)
    map_at(f::Function, Tc::Type, p, a...)

Parser transforming result of a wrapped parser. 
`a...` is passed as additional arguments to `f`.

See also: [`map`](@ref), [`Transformation`](@ref)
"""
function map_at(f::Function, Tc::Type, p, a...)
    T = infer_result_type(f,Tc,p,"call seq(function,type,parts...)",Int,typeof.(a)...)
    Transformation{Tc}((v,i) -> (f((v), i, a...)), p)
end
function map_at(f::Function, p, a...)
    T = infer_result_type(f,Any,p,"call seq(function,type,parts...)",Int,typeof.(a)...)
    Transformation{T}((v,i) -> (f(v, i, a...)), p)
end
@deprecate instance_at(a...) map_at(a...)

export Index
Index = map_at((v,i) -> i, Always())

import Base: map
"""
    map(f::Function, p::AbstractToken, a...)

Parser matching `p`, transforming parsing results (`x`) with function `f(x,a...)`.

See also: [`map_at`](@ref), [`Transformation`](@ref)
"""
function Base.map(f::Function, p::AbstractToken, a...)
    T = infer_result_type(f,Any,p,"call seq(function,type,parts...)",typeof.(a)...)
    Transformation{T}((v,i) -> (f(v, a...)), p)
end

"""
    map(T::Type, p::AbstractToken, a...)

Parser matching `p`, transforming `p`s parsing result with constructor `T(x,a...)`.

See also: [`map_at`](@ref) [`get`](@ref), [`Transformation`](@ref)
"""
function Base.map(Tc::Type, p::AbstractToken, a...)
    Transformation{Tc}((v,i) -> Tc(a..., v), p)
end

"""
    map(index::IndexAt, p::AbstractToken, a...)
    map(constant, p::AbstractToken, a...)

Parser matching `p`, transforming `p`s parsing results to `getindex(x,index)` or `constant`.

See also: [`map_at`](@ref) [`get`](@ref), [`Transformation`](@ref)

"""
function Base.map(index::IndexAt{<:Integer}, p::AbstractToken)
    T=result_type(p)    
    Transformation{fieldtype(T,index.i)}(index, p)
end
function Base.map(index::IndexAt{<:UnitRange}, p::AbstractToken)
    T=Tuple{fieldtypes(result_type(p))[index.i]...}
    Transformation{T}(index, p)
end
function instance(Tc::Type, p::ParserTypes, a...)
    Transformation{Tc}((v,i) -> Tc(a..., v), p)
end
function instance(Tc::Type, p::ParserTypes)
    Transformation{Tc}((v,i) -> convert(Tc,v), p)
end
function Base.map(f::Function, Tc::Type, p::AbstractToken, a...)
    T = infer_result_type(f,Tc,p,"call seq(function,type,parts...)",typeof.(a)...)
    Transformation{Tc}((v,i) -> (f(v, a...)), p)
end
Base.map(f::typeof(identity), p::AbstractToken) = p


@deprecate map(T::Type, f::Function, p::AbstractToken, a...) map(f,T,p,a...)
@deprecate instance(f::Function,p,a...) map(f,parser(p),a...)



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
    sets::S
    CharIn(x_) =
        let x = Set(optimize_(CharIn,x_))
            if length(x) == 1
                new{typeof(first(x))}(first(x))
            else
                new{typeof(x)}(x)
            end
        end
end




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
    # Examples
    ```jldoctest
    julia> regex_escape("Bang!")
    "Bang\\!"
    julia> regex_escape("  ( [ { . ? *")
    "\\ \\ \\(\\ \\[\\ \\{\\ \\.\\ \\?\\ \\*"
    julia> regex_escape("/^[a-z0-9_-]{3,16}\$/")
    "/\\^\\[a\\-z0\\-9_\\-\\]\\{3,16\\}\\\$/"
    ```
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
regex_string_(x::CharIn) = regex_string_(x.sets)
regex_inner(x::CharIn) =
    "["*regex_string_(x)*"]"

print_constructor(io::IO,x::CharIn{Char}) = nothing
regex_inner(x::CharIn{Char}) =
    regex_string_(x.sets[1])



export CharNotIn
"""
    CharNotIn(x)

Parser matching exactly one character in `x`.

```jldoctest
julia> a_z = CharNotIn('a':'z')
re"[^a-z]"

julia> parse(a_z, "a")
ERROR: ArgumentError: no successfull parsing.
Stacktrace:
 [1] parse(::CharNotIn{Tuple{StepRange{Char,Int64}}}, ::String) at /home/gregor/dev/julia/CombinedParsers/src/CombinedParsers.jl:2649
 [2] top-level scope at REPL[19]:1

julia> ac = CharNotIn("ac")
re"[^ac]"


julia> parse(ac, "c")
ERROR: ArgumentError: no successfull parsing.
Stacktrace:
 [1] parse(::CharNotIn{Array{Char,1}}, ::String) at /home/gregor/dev/julia/CombinedParsers/src/CombinedParsers.jl:2649
 [2] top-level scope at REPL[19]:1

```
"""
@auto_hash_equals struct CharNotIn{S} <: NIndexParser{1,Char}
    sets::S
    CharNotIn(x_) =
        let x = Set(optimize_(CharIn,x_))
            if length(x) == 1
                new{typeof(first(x))}(first(x))
            else
                new{typeof(x)}(x)
            end
        end
end
result_type(::Type{<:CharNotIn}) = Char
regex_inner(x::CharNotIn) =
    "[^"*join([regex_string_(s) for s in x.sets])*"]"
_ismatch(c,p::CharNotIn)::Bool = !_ismatch(c,p.sets)

CharIn(x::Tuple{<:CharNotIn}) = x[1]

"""
    CharIn(unicode_classes::Symbol...)

succeeds if char at cursor is not in any of the `unicode_classes`.
"""
CharNotIn(unicode_classes::Symbol...) =
    CharNotIn(UnicodeClass(unicode_classes...))

@inline function _iterate(parser::CharNotIn, sequence, till, i, state::Nothing)
    i>till && return(nothing)
    @inbounds c,ni = iterate(sequence,i)
    ismatch(c,parser.sets) && return nothing 
    return ni, MatchState()
end

@inline function _iterate(parser::CharIn, sequence, till, i, state::Nothing)
    i>till && return(nothing)
    @inbounds c,ni = iterate(sequence,i)
    !ismatch(c,parser.sets) && return nothing
    return ni, MatchState()
end


export CharMatcher
CharMatcher = Union{Char, AnyChar, CharIn, CharNotIn, UnicodeClass,StepRange{Char,Int},AbstractString}


for T in [:CharIn,:CharNotIn]
    eval(quote
         $T(x::Union{CharMatcher}...) = $T(tuple( x...))
         $T(x::$T{Tuple{<:$T}}) = $T(x.sets[1])
         $T(x::$T) = x #$T(x.sets)
         $T(x::AbstractString...) = $T( Char[ c for x_ in x for c in x_] )
         $T(x1::Char,x::Char...) = $T( Char[ x1, ( c for c in x )... ] )
         $T(x::Union{Missing,CharMatcher}...) = $T((e for e in x if e!==missing)...)
         end)
end
         
optimize_(::Type{<:Union{CharIn,CharNotIn}}) =
    tuple()
optimize_(::Type{<:Union{CharIn,CharNotIn}}, x1::CharIn) =
    optimize_(CharIn,x1.sets)
optimize_(::Type{<:Union{CharIn,CharNotIn}}, x1) =
    error("implement optimize_(::Type{<:Union{CharIn,CharNotIn}}, ::$(typeof(x1)))")
optimize_(::Type{<:Union{CharIn,CharNotIn}}, x1::Union{Function,Char,UnicodeClass,CharNotIn}) =
    tuple(x1)
optimize_(::Type{<:Union{CharIn,CharNotIn}}, x1::StepRange) =
    collect(x1)
optimize_(::Type{<:Union{CharIn,CharNotIn}}, x1::Set{Char}) =
    x1
optimize_(T::Type{<:Union{CharIn,CharNotIn}}, x1::Union{<:Vector,<:Tuple}) =
    optimize_(T,x1...)
optimize_(T::Type{<:Union{CharIn,CharNotIn}},x1,x...) =
    Iterators.flatten( Any[
        optimize_(T,x1), ( optimize_(T,e) for e in x )... ] )
export Repeat_stop, Repeat_until
"""
    Repeat_stop(p,stop)

Repeat `p` until `stop` (`NegativeLookahead`), not matching `stop`.
Sets cursor **before** `stop`.
Returns results of `p`.

```jldoctest
julia> p = Repeat_stop(AnyChar(),'b') * AnyChar()
🗄 Sequence
├─ 🗄* Sequence |> map(IndexAt(2)) |> Repeat
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
├─ 🗄 Sequence |> map(IndexAt(1))
│  ├─ 🗄* Sequence |> map(IndexAt(2)) |> Repeat
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
        Sequence(map(wrap,Repeat_stop(p,until)), until)
    else
        Sequence(1, map(wrap,Repeat_stop(p,until)), until)
    end

@deprecate rep_until(p,until) Repeat_until(p,until)



export FlatMap,after
@auto_hash_equals struct FlatMap{T,P,Q<:Function} <: CombinedParser{T}
    left::P
    right::Q
    function FlatMap{T}(left::P, right::Q) where {T, P, Q<:Function}
        new{T,P,Q}(left, right)
    end
end

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
    let li = nextind(str,i,parser.left,x[1])
        nextind(str,li,x[2],x[3])
    end

@inline prevind(str,i::Int,parser::FlatMap,x::Tuple) =
    let li = prevind(str,i,x[2],x[3])
        prevind(str,li,parser.left,x[1])
    end

    


function Base.get(parser::FlatMap, sequence, till, after, i, state)
    li = nextind(sequence,i,parser.left,state[1])
    get(state[2],sequence, till, after,
              li,
              state[3])
end


state_type(p::Type{<:FlatMap{T,P}}) where {T,P} = Tuple{state_type(P),<:Any,<:Any}
function _iterate(tokf::FlatMap, str, till, i, state)
    T = result_type(tokf)
    if state === nothing
        before_i = i
        lr = _iterate(tokf.left, str, till, i, nothing)
        lr === nothing && return nothing
        i_ = lr[1]
        rightp = tokf.right(get(tokf.left, str, till, lr[1],i,lr[2]))
        rr = nothing
        while rr === nothing
            rr = _iterate(rightp, str, till, i_, nothing)
            if rr === nothing
                lr = _iterate(tokf.left, str, till, i_, lr[2])
                lr === nothing && return nothing
                rightp = tokf.right(get(tokf.left, str, till, lr[1],before_i,lr[2]))
                i_ = lr[1]
            else
                return rr[1], (lr[2], rightp, rr[2])
            end
        end
    else
        lstate,rightp,rstate = state
        i_=i
        before_i = start_index(str,i_,tokf.left,lstate)
        rr = nothing
        while rr === nothing
            rr = _iterate(rightp, str, till, i_, rstate)
            if rr === nothing
                lr = _iterate(tokf.left, str, till, i_, lstate)
                lr === nothing && return nothing
                i_,lstate = lr
                rightp = tokf.right(get(tokf.left, str, till, i_,before_i,lstate))
                rstate = nothing
            else
                return rr[1], (lstate, rightp, rr[2])
            end
        end
    end
end



export Sequence
@auto_hash_equals struct Sequence{T,P<:Tuple} <: CombinedParser{T}
    parts::P
    function Sequence(p...)
        parts = tuple( ( parser(x) for x = p )... )
        T = ( result_type(typeof(x)) for x in parts )
        s = new{Tuple{T...},typeof(parts)}(parts)
        names = [ t.first=>i
                  for (i,t) in enumerate(p)
                  if t isa Pair{Symbol,<:ParserTypes} ]
        isempty(names) && return s
        T = result_type(s)
        NT= NamedTuple{ tuple( (n.first for n in names)...),
                        Tuple{ (fieldtype(T,n.second) for n in names)... }}
        NTn = NamedTuple{ tuple( (n.first for n in names)...) }
        function transform(v,i)
            ##@show v
            NT( tuple( (v[k.second] for k in names )... ))
        end
        map_at(transform, NT, s)
    end
end

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

function seq(tokens::Vararg{ParserTypes};
             transform=nothing, kw...)
    if transform isa Integer
        Sequence(transform,tokens...)
    elseif transform===nothing
        Sequence(tokens...)
    elseif transform isa Function
        map(transform, s)
    end
end

Sequence(transform::Integer,tokens::Vararg{ParserTypes}) =
    Sequence(Val{transform}(),tokens...)

function Sequence(::Val{transform},tokens::Vararg{ParserTypes}) where {transform}
    s = Sequence(tokens...)
    map(v -> v[transform], fieldtype(result_type(s),transform), s)
end


function Sequence(T::Type, parts::Vararg;
             ## log=false,
             transform=:instance)
    parts = tuple( ( parser(x) for x = parts )... )
    if T==NamedTuple
        fnames = tuple( [ x.name for x in parts if x isa NamedParser ]... )
        ftypes = [ result_type(typeof(x.parser)) for x in parts if x isa NamedParser ]
        RT = isempty(fnames) ? T : NamedTuple{fnames, Tuple{ftypes...}}
    else
        RT = T
    end
    if transform == :instance
        transform = (v,i) -> instance(RT,Any[ remove_null(x) for x in v ])
    end
    map(transform,RT,Sequence(parts...))
end

@deprecate seq(a...; kw...) Sequence(a...; kw...)

parser_types(::Type{Sequence{T, P}}) where {T, P} =
    P



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
sSequence_(x1) = tuple(x1)
sSequence_(x1,x...) = Iterators.flatten( Any[ sSequence_(x1), ( sSequence_(e) for e in x )... ] )

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
    opts = collect(sSequence_(x...))
    length(opts)==1 ? opts[1] : Sequence(opts...)
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

Base.get(parser::Sequence, sequence, till, after, i, state::MatchState) =
    get(parser, sequence, till, after, i, ( MatchState() for i in 1:length(parser.parts)) )

function Base.get(parser::Sequence, sequence, till, after, i, state)
    r = Vector{Any}(undef,length(parser.parts))
    i_=i
    for (p,s) in enumerate(state)
        after_=nextind(sequence,i_,parser.parts[p],s)
        r[p] = get(parser.parts[p],sequence, till, after_, i_, s)
        i_=after_
    end
    tuple(r...)
end


@inline function start_index(sequence,after,parser,state)
    r=state === nothing ? after : _prevind(sequence, after, parser, state)
end

function prune_captures(sequence,after_i)
end


function state_type(parser::Type{<:Sequence})
    pts = parser_types(parser)
    all(t->state_type(t)<:MatchState, fieldtypes(pts)) ? MatchState : Vector{Any}
end

"""
    _iterate(parser, sequence, till, i, states)

Note: `i` is the index in `sequence` after `parser` match according to `state` (and not the start of the match), 
such that `start_index(sequence,after,parser,state)` returns the start of the matching subsequence,
and sequence[start_index(sequence,after,parser,state):prevind(sequence,i)] is the matched subsequence.
"""
function _iterate_(parser::Sequence, sequence, till, i, states)
    i_ = i
    parts=parser.parts
    nexti,states = if states === nothing
        sss = Vector{Any}(undef,length(parts))
        length(parts) == 0 && return i,sss
        sss[1] = nothing
        1,sss
    else
        length(states),states
    end
    length(parts) == 0 && return nothing
    while nexti<=length(states)
        ## compute before next iteration, because states[nexti] might change.
        ## only used if ns===nothing, but states[nexti] might still be modified.
        before_i = start_index(sequence, i_, parts[nexti], states[nexti])
        ns = _iterate(parts[nexti], sequence, till, i_, states[nexti])
        if ns === nothing
            nexti -= 1
            i_ = before_i
            prune_captures(sequence,i_)
            nexti == 0 && return nothing
            if before_i==0
                println(parts[nexti])
                error()
            end
        else
            states[nexti] = ns[2]
            i_ = ns[1]
            nexti += 1
            if nexti > length(states)
                return i_, states
            else
                states[nexti] = nothing
            end
        end
    end
    error("?")
end

@generated function _iterate(parser::Sequence, sequence, till, i, states)
    pts = parser_types(parser)
    fpts = fieldtypes(pts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    afteri = Symbol[ gensym(:after) for p in fpts ]
    substate = Symbol[ gensym(:s) for p in fpts ]
    init = if states===Nothing
        [
            quote
            $(substate[p]) = nothing
            @inbounds $(part[p]) = parser.parts[$p]
            end
            for (p,t) in enumerate(fpts)
        ]
    elseif states===MatchState
        [
            quote
            $(substate[p]) = MatchState()
            @inbounds $(part[p]) = parser.parts[$p]
            end
            for (p,t) in enumerate(fpts)
        ]
    else
        [
            quote
            $(substate[p]) = states[$p]
            @inbounds $(part[p]) = parser.parts[$p]
            end
            for (p,t) in enumerate(fpts)
        ]
    end

    ret_state = if state_type(parser) <: MatchState
        :(R::MatchState = MatchState())
    else
        :(R::$(state_type(parser)) = Any[ $([ :(($(s))) for s in substate ]...) ] )
    end
    parseparts = [
        quote
        @label $(subsearch[p])
        before_i = start_index(sequence, i_, $(part[p]), $(substate[p]))
        $(subresult[p]) = _iterate($(part[p]), sequence, till, i_, $(substate[p]))
        if $(subresult[p]) === nothing
        i_ = before_i
        prune_captures(sequence,i_)
        @goto $(p == 1 ? :theend : subsearch[p-1])
        else
        i_, $(substate[p]) = $(subresult[p])
        $(if p < length(fpts); (:($((substate[p+1]))=nothing)); end )
        end
        end
        for (p,t) in enumerate(fpts)
    ]
    R = quote
        i_::Int = i
        $(init...)
        states !== nothing && @goto $(subsearch[end])
        $(parseparts...)
        $ret_state
        return i_, R
        @label theend
        return nothing
    end
    ##Core.println( R )
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
a+?  |> Repeat |> Lazy |> regular expression combinator
::Array{Char,1}

julia> re"a??"
a??  |> Optional(default=missing) |> Lazy |> regular expression combinator
::Union{Missing, Char}
```
"""
@auto_hash_equals struct Lazy{P,T} <: WrappedParser{P,T}
    parser::P
    Lazy(p_) =
        let p = parser(p_)
            new{typeof(p),result_type(p)}(p)
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

Repeat_max = 10^6
export Repeat1, Repeat
"""
    Repeat(x)
    Repeat(x; min=0,max=Repeat_max)
    Repeat(min::Integer, x)
    Repeat(min::Integer,max::Integer, x)

Parser repeating pattern `x` `min:max` times.
"""
@auto_hash_equals struct Repeat{P,T} <: WrappedParser{P,T}
    range::UnitRange{Int}
    parser::P
    Repeat(range::UnitRange{Int},p) =
        let p_=parser(p)
            new{typeof(p_),Vector{result_type(p_)}}(range,p_)
        end
end
Repeat(min::Integer,max::Integer,parser) =
    Repeat((min:max),parser)
Repeat(parser;min::Integer=0,max::Integer=Repeat_max) =
    Repeat((min:max),parser)
Repeat(min::Integer,parser) =
    Repeat((min:Repeat_max),parser)
Repeat(x::ParserTypes,y::Vararg{ParserTypes}) =
    Repeat(Sequence(x,y...) )
"""
    Repeat(f::Function,a...)

Abbreviation for `map(f,Repeat(a...))`.
"""
Repeat(f::Function,a...) =
    map(f,Repeat(a...))

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

Repeat(x::ParserTypes, minmax::Tuple{<:Integer,<:Integer}=(0,Repeat_max)) = Repeat(minmax...,x)

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
        ( delim_ * x.parser )[2])) do (f,r)
            pushfirst!(r,f)
            r
        end
end

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
        i=prevind(str,i,parser.parser,x[j])
    end
    i
end

function Base.get(parser::Repeat, sequence, till, after, i, state::Vector)
    r = Vector{result_type(parser.parser)}(undef,length(state))
    i_=i
    for (p,s) in enumerate(state)
        after_=nextind(sequence,i_,parser.parser,s)
        r[p] = get(parser.parser,sequence, till, after_, i_, s)
        i_=after_
    end
    r
end

function Base.get(parser::Repeat, sequence, till, after, i, state::Int)
    r = Vector{result_type(parser.parser)}(undef,state)
    i_=i
    s=MatchState()
    for p in 1:state
        after_=nextind(sequence,i_,parser.parser,s)
        r[p] = get(parser.parser,sequence, till, after_, i_, s)
        i_=after_
    end
    r
end

@inline state_type(::Type{<:Repeat{P}}) where P =
    emptystate_type(Vector{state_type(P)})
@inline emptystate_type(::Type{Vector{MatchState}}) = Int
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

@inline emptystate_type(::Type{Vector{T}}) where T =
    Vector{emptystate_type(T)}
@inline emptystate_type(T::Type) = T
@inline state_length(parser::Repeat,x::Vector) =
    length(x)
@inline emptystate(::Type{Vector{T}}) where T =
    emptystate_type(T)[]

@inline function pushstate!(state::Vector,parser,substate)
    push!(state,substate)
end
@inline function poplast!(state::Vector,parser)
    l=pop!(state)
    l,state
end


@inline function fill_rep(t::Repeat, sequence, till::Int, j::Int,state_::S) where S
    j_::Int = -1
    while state_length(t,state_) < t.range.stop && ( x = _iterate(t.parser,sequence, till,j,nothing) )!==nothing
        ##@info "rep fill..." x state_
        ## e.g. match(re"(?:a|(?=b)|.)*\z","abc")
        state_length(t,state_)>t.range.start && j_==x[1] && break
        state_=pushstate!(state_,t.parser, x[2])
        j_=j
        j = x[1]
    end
    j,state_,state_length(t,state_) < t.range.start
end


function _iterate(t::Repeat, sequence, till, i, state)
    i_::Int,state_::state_type(typeof(t)),goback::Bool = if state === nothing
        es = emptystate(state_type(typeof(t)))
        fill_rep(t,sequence,till,i, es)
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
        i, state, true
    end
    while goback
        if state_length(t,state_)==0
            return nothing
        end
        lstate, state_=poplast!(state_,t.parser)
        before_i = _prevind(sequence,i_,t.parser,lstate) ##state[end][1]
        prune_captures(sequence,before_i)
        x = _iterate(t.parser,sequence, till, i_, lstate)
        if x === nothing
            state_length(t,state_) in t.range && return before_i, state_
            i_ = before_i
            if state_length(t,state_)==0
                goback = false
            end
        elseif before_i==x[1]
            i_ = before_i
        else
            state_=pushstate!(state_,t.parser,x[2])
            i_,state_,goback = fill_rep(t,sequence,till,x[1],state_)
        end
    end
    if state_length(t,state_) in t.range
        i_, state_
    else
        nothing
    end
end




function fill_rep(t_::Lazy{<:Repeat}, sequence, till, j,state_)
    t = t_.parser
    while state_length(t,state_) < t.range.start && (x = _iterate(t.parser,sequence, till,j,nothing))!==nothing 
        j==x[1] && state_length(t,state_)>0 && break
        state_=pushstate!(state_,t.parser,x[2])
        j==x[1] && break
        j = x[1]
    end
    j,state_
end
function _iterate(t_::Lazy{<:Repeat}, sequence, till, i, state)
    t = t_.parser
    i_ = i
    state_ = state
    if state === nothing
        es = emptystate(state_type(typeof(t)))
        i_,state_ = fill_rep(t_,sequence,till,i,es)
    else
        if state_length(t,state)<t.range.stop
            x = _iterate(t.parser,sequence, till, i_, nothing)
            if x!==nothing && ( x[1]>i_ || state_length(t,state)==0)
                state_=pushstate!(state,t.parser,x[2])
                return x[1],state_
            end
        end
        goback = true
        while goback
            if state_length(t,state)==0
                return nothing
            end
            lstate, state_=poplast!(state,t.parser)
            before_i = _prevind(sequence,i_,t.parser,lstate) ##state[end][1]
            x = _iterate(t.parser,sequence, till, i_, lstate)
            if x === nothing
                i_ = before_i
                prune_captures(sequence,i_)
                if state_length(t,state_)==0
                    return nothing
                end
                state = state_
            else
                state_=pushstate!(state_,t.parser,x[2])
                i_,state_ = fill_rep(t_,sequence,till,x[1],state_)
                if state_length(t,state_) in t.range
                    goback = false
                end
            end
        end
    end
    if state_length(t,state_) in t.range ## && state_length(t,state_)>0
        return i_, state_
    else
        nothing
    end
end











export Optional
struct None end
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
@auto_hash_equals struct Optional{P,T} <: WrappedParser{P,T}
    parser::P
    default::T
    function Optional(p_;default=defaultvalue(result_type(p_)))
        p = parser(p_)
        T = result_type(p)
        D = typeof(default)
        T_ = promote_type(T,D)
        T_ === Any && ( T_ = Union{T,D} )
        new{typeof(p),T_}(p, default)
    end
end
state_type(::Type{<:Optional{P}}) where P = Union{None,state_type(P)}##Tuple{Int, promote_type( state_type.(t.options)...) }



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


function Base.get(parser::Optional, sequence, till, after, i, state)
    state === None() ? parser.default : get(parser.parser,sequence, till, after, i, state)
end

_iterate(t::Optional, str, till, i, state::None) =
    nothing

function _iterate(t::Optional, str, till, i, state)
    before_i = state === nothing ? i : prevind(str,i,t.parser,state) ##state[end][1]
    r = _iterate(t.parser, str, till, i, state)
    if r === nothing
        prune_captures(str,before_i)
        return tuple(before_i, None())
    else
        r[1], r[2]
    end
end

function _iterate(t_::Lazy{<:Optional}, str, till, i, state)
    t=t_.parser
    if state === nothing
        i,None()
    else 
        r = _iterate(t.parser, str, till, i,
                     state === None() ? nothing : state)
        if r === nothing
            nothing
        else
            r[1], r[2]
        end            
    end
end


defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type) = missing
defaultvalue(V::Type{<:CombinedParser}) = Always()






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
struct Either{T,Ps} <: CombinedParser{T}
    options::Ps
    Either{T}(p::P) where {T,P<:Union{Vector,Tuple}} =
        new{T,P}(p::P)
    Either{T}(p::CombinedParser...) where {T} =
        new{T,Vector{Any}}(Any[p...])
end
state_type(::Type{<:Either}) = Pair{Int,Any}
parser_types(::Type{Either{T, P}}) where {T, P} = P
children(x::Either) = x.options
regex_string(x::Either) = join(regex_string.(x.options),"|")
regex_prefix(x::Either) = "|"
regex_inner(x::Either) = join([ regex_string(p) for p in x.options],"|")
regex_suffix(x::Either) = "..."
print_constructor(io::IO,x::Either) = print(io,"Either")

function Either(x::ParserTypes...)
    parts = Any[ parser(y) for y in x ]
    Ts = ( result_type(typeof(x)) for x in parts )
    T = promote_type(Ts...)
    Any <: T && ( T = Union{Ts...} )
    Either{T}(parts)
end

"""
    Either(transform::Function, x::Vararg)

abbreviation for `map(transform, Either(x...))`.
"""
function Either(transform::Function, x::Vararg)
    map(transform, Either(x...))
end


@deprecate alt(a...) Either(a...)


function deepmap_parser(f::Function,mem::AbstractDict,x::Either,a...;kw...)
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
   ├─ [AB]
   └─ bc
::Union{Char, SubString}


julia> sEither('a',CharIn("AB")|"bc")
|🗄... Either
├─ a
├─ [AB]
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
function Base.push!(x::Either, y)
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
function Base.pushfirst!(x::Either, y)
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
end
Base.show(io::IO, x::MutablePair) =
    print(io, x.first, "=>", x.second)
@inline function with_state!(x::MutablePair,s)
    ##s isa Tuple{Int,Nothing} && error()
    x.second=s
    x
end
@inline with_state!(x::Nothing,k,s) = Pair(k,s)
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

@inline function prevind(str,i::Int,parser::Either,x)
    ## @show i
    prevind(str,i,(@inbounds parser.options[x.first]),x.second)
end

@inline function nextind(str,i::Int,parser::Either,x)
    ## @show i
    nextind(str,i,(@inbounds parser.options[x.first]),x.second)
end

 

@generated function prevind(str,i::Int,parser::Either{<:Any,<:Tuple},x)
    pts = parser_types(parser)
    fpts = fieldtypes(pts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    push!(subsearch, gensym(:subsearch))
    part = Symbol[ gensym(:part) for p in fpts ]
    init = [
        quote
        @inbounds $(part[p]) = parser.options[$p]
        end
        for (p,t) in enumerate(fpts)
    ]
    parseoptions = [
        quote
        @label $(subsearch[p])
        j > $p && @goto $(subsearch[p+1])
        return prevind(str,i,$(part[p]),s)
        end
        for (p,t) in enumerate(fpts)
    ]
    init_before = 
        quote
            j = x.first
            s = x.second
        end
    R = quote
        $(init...)
        $(init_before)
        $(parseoptions...)
        @label $(subsearch[end])
        return i
    end
    R
end


function Base.get(parser::Either, sequence, till, after, i, state)
    j = state.first
    lstate = state.second
    get(parser.options[j],sequence, till, after, i, lstate)
end



@inline function _iterate_paired(first, t, str, till, i, state)
    sstate = _iterate(t, str, till, i, state)
    if sstate !== nothing
        i_::Int, nstate_ = sstate
        return i_, Pair{Int,Any}(first,nstate_)
    end
    nothing
end

function _iterate(t::Either{<:Any,<:Vector}, str, till, i, state::Nothing)
    for (j,o) in enumerate(t.options)
        ( r = _iterate_paired(j,( @inbounds t.options[j] ),str,till,i,nothing) )!== nothing && return r
    end
    nothing
end



function _iterate(t::Either{<:Any,<:Vector}, str, till, i, state::Pair)
    @inbounds opt = t.options[state.first]
    fromindex = state.first+1
    before_i = _prevind(str,i,opt,state.second) ##state[end][1]
    r = _iterate_paired(state.first,opt,str,till,i,state.second)
    r !== nothing && return r
    prune_captures(str,before_i)
    ##sstate = nothing
    for j in fromindex:length(t.options)
        ## @info "alt" j str[i:till] typeof(t.options[j]) #t.options[j] 
            ## i_::Int, nstate_ = sstate
            ## with_key_state!(s_,j,sstate[2])
        @inbounds r = _iterate_paired(j,t.options[j],str,till,before_i,nothing)
        r !== nothing && return r
    end
    nothing
end


@generated function _iterate(parser::Either{<:Any,<:Tuple}, sequence, till, i, state::Union{Nothing,Pair{Int,<:Any},MutablePair{Int,<:Any}})
    pts = parser_types(parser)
    fpts = fieldtypes(pts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    push!(subsearch, gensym(:subsearch))
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    afteri = Symbol[ gensym(:after) for p in fpts ]
    substate = Symbol[ gensym(:s) for p in fpts ]
    init = [
        quote
        $(substate[p]) = nothing
        @inbounds $(part[p]) = parser.options[$p]
        end
        for (p,t) in enumerate(fpts)
    ]
    parseoptions = [
        quote
        @label $(subsearch[p])
        j > $p && @goto $(subsearch[p+1])
        before_i = before_i == 0 ? start_index(sequence, i_, $(part[p]), sstate) : before_i
        $(subresult[p]) = _iterate_paired($p, $(part[p]), sequence, till, i_, sstate)
        if $(subresult[p]) !== nothing
        return $(subresult[p])
        end
        i_ = before_i
        sstate = nothing
        end
        for (p,t) in enumerate(fpts)
    ]
    init_before = if state <: Nothing
        quote
            j = 1
            sstate = nothing
            before_i = i
        end
    else
        quote
            j = state.first
            sstate = state.second
            before_i = 0
        end
    end
    R = quote
        i_::Int = i
        $(init...)
        $(init_before)
        $(parseoptions...)
        @label $(subsearch[end])
        return nothing
    end
    R
end






############################################################


##import Base: findnext



# Base.convert(::Type{Nullable{Pair{Symbol, T}}}, x::Pair{Symbol, Nullable{S}}) where {T,S} =
#     isnull(x.second) ? Nullable{Pair{Symbol, T}}() :
#     Nullable(x.first => convert(T, x.second.value))

@auto_hash_equals struct Greedy{Ps,A,F<:Function} <: CombinedParser{Any}
    pairs::Ps
    alt::A
    transform::F
end
    
export greedy
function greedy(tokens...;
                alt = [],
                transform=(v,i) -> v)
    Greedy([tokens...], alt, transform)
end

function TextParse.tryparsenext(tokf::Greedy, str, i, till, opts=TextParse.default_opts)
    T = result_type(tokf)
    sections=tokf.pairs
    RT(key, value) = if value[2] isa ParserTypes
        if Missing <: result_type(typeof(key))
            result_type(typeof(value[2]))
        else
            promote_type(result_type(typeof(key)), result_type(typeof(value[2])))
        end
    else
        result_type(typeof(key))
    end
    R = Dict([ value[1] => Vector{RT(key,value)}() for (key,value) in sections]...,
             [ key => Vector{result_type(typeof(value))}() for (key,value) in tokf.alt]...
             )
    hist = Any[]
    last_section = nothing
    last_content = nothing
    aggregator = :head
    function first_match(j)
        local repval, i__
        for (key, content) in sections
            repval, i__ = tryparsenext(key, str, j, till)
            !isnull(repval) && return key, content, repval, i__
        end
        return (first(sections)..., repval, i__)
    end
    head = nothing
    i_ = i ##isnull(1)
    while true
        key, content, r, i__ = first_match(i_)
        save = if isnull(r)
            cr, ci = if last_content === nothing || last_content === missing
                Nullable{T}(), i
            else
                tryparsenext(last_content, str, i_, till)
            end
            ai = 0
            while ai < lastindex(tokf.alt) && isnull(cr)
                ai = ai+1
                cr, ci = tryparsenext(tokf.alt[ai].second, str, i_, till)
            end
            if isnull(cr)
                return Nullable{T}(convert(T, tokf.transform(R,i))), i_
            elseif ai == 0
                push!(hist, get(cr))
                i__ = ci
                false
            else
                aggregator != :head && append!(R[aggregator],hist)
                hist = [get(cr)]
                (aggregator, last_content) = tokf.alt[ai]
                last_section = ai
            end
        else
            if last_section !== nothing
                append!(R[aggregator],hist)
            end
            hist = get(r) !== missing ? [get(r)] : Vector{RT(key,content)}()
            aggregator, last_content = content
            last_section = key
        end
        i_ = i__
    end
    error("unreachable")
end






export alternate, alternate_stop
alternate_stop(x,delim,stop;kw...) =
    alternate(seq(NegativeLookahead(stop), x; transform=2),
              seq(NegativeLookahead(stop), delim; transform=2);
              kw...)

alternate(x::Vector, delim; kw...) = alternate(alt(x...), delim; kw...)
"""
optimized repeated alternations of `x``delim`, optionally starting/ending with `delim`. `delim` `is agg`ed as right borders. 
`delim` can be discarded in agg(result,missing,delim).

if `agg` is nothing, default is to aggregate delim after match is `result_type(delim) <: result_type(x)`, if not missing.
"""
function alternate(x::ParserTypes, delim::ParserTypes;
                   agg = nothing,
                   kw...)
    T, S = result_type(typeof(x)), result_type(typeof(delim))
    af = if agg === nothing
        if S <: T
            ( r, xmatch, delimmatch ) -> begin
                xmatch !== missing && push!(r,xmatch)
                delimmatch !== missing && push!(r,delimmatch)
                r::Vector{T}
            end
        else
            ( r, xmatch, delimmatch ) -> begin
                xmatch !== missing && push!(r,xmatch)
                r::Vector{T}
            end 
        end
    else
        agg
    end
    function tf(v)
        ## @show v,i
        r = T[]
        if isempty(v[2])
            af(r,v[1],v[3])
        else
            ms = v[2]
            af(r,v[1],ms[1][1])
            for i in 2:lastindex(ms)
                af(r, ms[i-1][2],ms[i][1])
            end
            af(r, ms[end][2],v[3])
        end
        r::Vector{T}
    end

    ## todo: factor out this transform condition!!
    Sequence(tf,
             Optional(x; default=missing),
             Repeat(seq(delim, x)),
             Optional(delim;default=missing))
end


export Repeat_delim
Repeat_delim(x::AbstractToken{T}, delim::AbstractToken{S}; kw...) where {T,S} =
    Repeat_delim(promote_type(S,T), x, delim; kw...)
function Repeat_delim(
    T::Type, x, delim;
    log=false,repf=Repeat,
    transform=(v,i) -> v,
    transform_each=(v,i) -> v, kw...)
    x = parser(x)
    delim = parser(delim)
    function t(v,i)
        L = vcat(v...)
        transform(map(p -> transform_each(p,i),  L  ),i)
    end
    seq(Vector{T},
        Optional(delim; default=T[], log=log),
        repf(Vector{T},
             seq(x, delim; log=log); log=log,
             transform = (v,i) -> vcat([ [x...] for x in v ]...)),
        Optional(x; default=T[], log=log)
        ; log=log,
        ## todo: factor out this transform condition!!
        transform = (t)
        , kw...)
end


export Repeat_delim_par
function Repeat_delim_par(x, delim; repf=Repeat, transform=(v,i) -> v, transform_each=v -> v, kw...)
    x = parser(x)
    delim = parser(delim)
    T = result_type(typeof(x))
    D = result_type(typeof(delim))
    seq(Vector{T},
        Optional(Vector{D}, delim; transform = (v,i) -> D[v]),
        repf(seq(T, x, delim; 
                 transform = (v, i) -> v[1]);
             transform=(v,i) -> v),
        Optional(Vector{T}, x; transform = (v,i) -> T[v])
        ; 
        ## todo: factor out this transform condition!!
        transform = (v,i)  -> transform(
            map(p -> transform_each(p),
                vcat(v[2:end]...)),i)
        , kw...)
end


export Atomic
"""
    Atomic(x)

A parser matching `p`, and failing when required to backtrack
(behaving like an atomic group in regular expressions).
"""
@auto_hash_equals struct Atomic{P,T} <: WrappedParser{P,T}
    parser::P
    Atomic(x) =
        let p=parser(x)
            new{typeof(p),result_type(p)}(p)
        end
end

deepmap_parser(f::Function,mem::AbstractDict,x::Atomic,a...;kw...) =
    get!(mem,x) do
        Atomic(
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end

@inline function _iterate(parser::Atomic, sequence, till, i, state)
    if state !== nothing
        nothing
    else
        _iterate(parser.parser, sequence, till, i, state)
    end
end

Base.get(x::Atomic, a...) =
    get(x.parser,a...)

function print_constructor(io::IO,x::Atomic)
    print_constructor(io,x.parser)
    print(io, " |> Atomic" )
end

regex_prefix(x::Atomic) = "(?>"*regex_prefix(x.parser)
regex_suffix(x::Atomic) = regex_suffix(x.parser)*")"
regex_inner(x::Atomic) = regex_inner(x.parser)




export Parsings
@auto_hash_equals struct Parsings{P,S}
    parser::P
    sequence::S
    till::Int
    Parsings(parser,sequence) =
        new{typeof(parser),typeof(sequence)}(parser,sequence,lastindex(sequence))
end
result_type(::Type{<:Parsings{P}}) where P =
    result_type(P)
Base.eltype(T::Type{<:Parsings}) =
    result_type(T)
Base.IteratorSize(::Type{<:Parsings}) = Base.SizeUnknown()

import Base: iterate
function Base.iterate(x::Parsings, s=(1,nothing))
    s_ = _iterate(x.parser,x.sequence,x.till,s...)
    s_ === nothing && return s_
    get(x.parser,x.sequence,x.till,s_[1],1,s_[2]), s_
end

export parse_all
function parse_all(parser::ParserTypes, sequence::AbstractString)
    p=Parsings(parser,sequence)
end



import Base: tryparse, parse
"""
    parse(parser::ParserTypes, str::AbstractString; log=nothing)

Parse a string with a CombinedParser as an instance of `result_type(parser)`.

If `log` is a `Vector{Symbol}`, parser is transformed with `log_names(p, log)`.
See also [`log_names`](@ref).

```jldoctest
julia> using TextParse

julia> p = ("Number: "*TextParse.Numeric(Int))[2]
🗄 Sequence |> map(#31)
├─ Number\\:\\
└─ <Int64>
::Int64


julia> parse(p,"Number: 42")
42

```

"""
function Base.parse(p::AbstractToken, s; log=nothing)
    i = tryparse(log !== nothing ? log_names(p,log) : p,s)
    i === nothing && throw(ArgumentError("no successfull parsing."))
    i
end

"""
    tryparse(parser::ParserTypes, str::AbstractString)

Like `parse`, but returns either a value of `result_type(parser)` or `nothing` if string does not start with with a match.
"""
function Base.tryparse(p::AbstractToken, s)
    i = _iterate(p,s)
    i === nothing && return nothing
    get(p,s,lastindex(s),i[1],1,i[2])
end

export tryparse_pos
"""
    tryparse_pos(parser::ParserTypes, str::AbstractString)

Like `parse`, but returns either a tuple of `result_type(parser)` and the position after the match, or `nothing` if string does not start with with a match.
"""
function tryparse_pos(p,s)
    i = _iterate(p,s)
    i === nothing && return nothing
    get(p,s,lastindex(s),i[1],1,i[2]),i[1]
end

_iterate(parser,sequence) =
    _iterate(parser, sequence, lastindex(sequence),1,nothing)




export _iterate

export Numeric
Numeric = TextParse.Numeric

deepmap_parser(f::Function,mem::AbstractDict,x::Numeric,a...; kw...) = x

include("reverse.jl")
include("textparse.jl")
include("re.jl")


include("show.jl")

include("operators.jl")


export optimize
optimize(x) = deepmap_parser(_optimize,x)

_optimize(x,a...) = x
deepmap_parser(::typeof(_optimize),dict::AbstractDict,x::SideeffectParser) = x.parser
end # module



