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
using Nullables
using AutoHashEquals
import Base: ==, hash
import Base: lowercase
import Base: cat, get

using ReversedStrings
import ReversedStrings: reversed, reverse_index

using TextParse
import TextParse: AbstractToken

include("ind.jl")

using AbstractTrees
import AbstractTrees: children
import AbstractTrees: print_tree, printnode

export CombinedParser
export result_type

"Julia types that provide CombinedParser methods result_type, state_type, _iterate, get, nextind, prevind."
## Pair{<:Union{AbstractToken, AbstractString, Char, Regex, Pair},<:Any} }
export parser
import Base: convert

"""
    parser(x)

A [`ConstantParser`](@ref) matching `x`.
"""
parser(x) =
    ConstantParser(x)


"""
    parser(x::StepRange{Char,<:Integer})

[`CharIn`](@ref) matching x.
"""
parser(x::StepRange{Char,<:Integer}) =
    CharIn(x)


export _iterate
"""
    _iterate(parser, sequence, till::Int, posi::Int[, nothing])

Dispatches to `_iterate(parser, sequence,till,posi,posi,nothing)` to retrieve first match, or nothing.
"""
@inline _iterate(parser, sequence, till::Int, posi::Int) =
    _iterate(parser, sequence,till,posi,posi,nothing)
@inline _iterate(parser, sequence, till::Int, posi::Int, ::Nothing) =
    _iterate(parser, sequence,till,posi,posi,nothing)

"""
    CombinedParser{T,S} <: AbstractToken{T}

Abstract parser type for parsers returning matches transformed to `::T` and 
state::`S`.
"""
abstract type CombinedParser{S,T} <: AbstractToken{T} end
result_type(x::CombinedParser) = result_type(typeof(x))
result_type(::Type{<:CombinedParser{<:Any,T}}) where T = T
parser(x::CombinedParser) = x

"""
    Base.convert(::Type{CombinedParser},x)

[`parser`](@ref)`(x)`.
"""
Base.convert(::Type{CombinedParser},x) =
    parser(x)

"""
    (x::CombinedParser)(str;kw...)


`parse(x,str;kw...)`

See also [`parse`](@ref).
"""
(x::CombinedParser)(str;kw...) = parse(x,str;kw...)
(x::CombinedParser)(prefix,str;kw...) = parse(Sequence(2,prefix,x),str;kw...)
(x::CombinedParser)(f::Function,a...;kw...) = map(f,x,a...;kw...)

@inline state_type(::Type{<:CombinedParser{S}}) where {S} = S

include("state.jl")

"""
    LeafParser{T} <: CombinedParser{T}

Abstract parser type for parsers that have no sub-parser (e.g. [`ConstantParser`](@ref)).
Used for dispatch in [`deepmap_parser`](@ref).
"""
abstract type LeafParser{S,T} <: CombinedParser{S,T} end

include("textparse.jl")

export regex_string
"""
    regex_string(x::CombinedParser)

`regex_prefix(x)*regex_inner(x)*regex_suffix(x)`
"""
regex_string(x::CombinedParser) = regex_prefix(x)*regex_inner(x)*regex_suffix(x)

regex_prefix(x::AbstractString) = ""
regex_suffix(x::AbstractString) = ""
regex_inner(x::AbstractString) = ""

regex_prefix(x::CombinedParser) = ""
regex_suffix(x::CombinedParser) = ""
regex_inner(x::CombinedParser) = ""


constructor_name(x) = typeof(x).name

"""
    print_constructor(io::IO,x)

Print constructor pipeline in parser tree node.
"""
print_constructor(io::IO,x) =
    if x isa CombinedParser
        print(io, constructor_name(x))
    else
    end




"Abstract type for parser wrappers, providing default methods"
abstract type WrappedParser{P,S,T} <: CombinedParser{S,T} end

children(x::WrappedParser) = children(x.parser)
children_char = '\U1F5C4'

function print_constructor(io::IO,x::WrappedParser)
    print_constructor(io, x.parser)
    print(io, " |> ", constructor_name(x))
end

regex_prefix(x::WrappedParser) = regex_prefix(x.parser)
regex_suffix(x::WrappedParser) = regex_suffix(x.parser)
regex_inner(x::WrappedParser) = regex_inner(x.parser)

@inline _prevind(str,i::Int,parser::WrappedParser,x::NCodeunitsState) =
    i-x.nc
@inline _nextind(str,i::Int,parser::WrappedParser,x::NCodeunitsState) =
    i+x.nc

@inline _prevind(str,i::Int,parser::WrappedParser,x) = _prevind(str,i,parser.parser,x)
@inline _nextind(str,i::Int,parser::WrappedParser,x) = _nextind(str,i,parser.parser,x)

"""
    _iterate(parser, sequence, till, posi, next_i, states)

Note: `next_i` is the index in `sequence` after `parser` match according to `state` (and not the start of the match), 
such that `start_index(sequence,after,parser,state)` returns the start of the matching subsequence,
and sequence[start_index(sequence,after,parser,state):_prevind(sequence,next_i)] is the matched subsequence.
"""
@inline _iterate(parser::WrappedParser, sequence, till, posi, after, state) =
    _iterate(parser.parser, sequence, till, posi, after, state)

export FilterParser
"""
A parser succeeds ony if 
1. the wrapped `parser` succeeds 
2. and a predicate function `state_filter(sequence, till, posi, r...)` returns `true` the `after,state = r` tuple.
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


"Abstract type for stepping with previndex/nextindex, accounting for ncodeunit length of chars at point."
abstract type NIndexParser{N,T} <: LeafParser{MatchState,T} end

include("constant.jl")
export Bytes

"""
    Bytes{N,T} <: NIndexParser{N,T}

Fast parsing of a fixed number `N` of indices, 
`reinterpret(T,match)[1]` the parsed vector as `T`, if `isbitstype`, or `T(match)` constructor otherwise.

Provide `Base.get(parser::Bytes{N,T}, sequence, till, after, i, state) where {N,T}` for custom conversion.

!!! note

    Endianness can be achieved by just mapping `bswap`
    ```jldoctest
    julia> map(bswap, Bytes(2,UInt16))([0x16,0x11])
    0x1611

    julia> Bytes(2,UInt16)([0x16,0x11])
    0x1116
    ```

"""
struct Bytes{N,T} <: NIndexParser{N,T}
end

"""
    Bytes(N::Integer, T::Type=Vector{UInt8})

If available before end of sequence, parse `N` bytes successfully with `result_type` `T`, fail otherwise.
"""
Bytes(N::Integer, T::Type=Vector{UInt8}) = Bytes{N,T}()
_iterate(parser::Bytes{N}, sequence, till, posi, next_i, state::Nothing) where N = 
    posi+N <= till+1 ? (_nextind(sequence,posi,N), MatchState()) : nothing
_iterate(parser::Bytes, sequence, till, posi, next_i, state::MatchState) =
    nothing
regex_string_(x::Bytes{N}) where N = ".{$(N)}"
Base.show(io::IO, x::Bytes{N}) where N =
    print(io, "$(N) Bytes::$(result_type(x))")
@inline _prevind(str,i::Int,parser::Bytes{N},x) where N =
    _prevind(str,i,N)
@inline _nextind(str,i::Int,parser::Bytes{N},x) where N =
    _nextind(str,i,N)

@inline _prevind(str,i::Int,parser::Union{NIndexParser{0},ConstantParser{0}},x) =
    i
@inline _nextind(str,i::Int,parser::Union{NIndexParser{0},ConstantParser{0}},x) =
    i
@inline _prevind(str,i::Int,parser::NIndexParser{L},x) where L =
    _prevind(str,i,L)
@inline _nextind(str,i::Int,parser::NIndexParser{L},x) where L =
    _nextind(str,i,L)
_iterate(parser::NIndexParser, sequence, till, posi, next_i, state::MatchState)  =
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
_ismatch(c,p::Union{StepRange,Set})::Bool = c in p

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
function _ismatch(x, set::Union{Tuple,Vector})::Bool
    return _ismatch(x,set...)
end

function _ismatch(x)::Bool
    return false
end

function _ismatch(x, f, r1, r...)::Bool
    ismatch(x,f) && return true
    return _ismatch(x::Char, r1, r...)
end

function _ismatch(c,p)::Bool
    c==p
end
function ismatch(c,p)::Bool
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
_prevind(str,i::Int,p::Always,x) = i
_nextind(str,i::Int,p::Always,x) = i
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
    PositiveLookahead(p_,reversed=true) =
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
    NegativeLookahead(p_,reversed=true) =
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
    x.str[min(x.index,end):min(end, _nextind(x.str,x.index,x.delta))]
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
    SideeffectParser(f::Function, p::CombinedParser,a...) =
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


"""
    Base.escape_string(x::AbstractVector)

for printing a non-string sequence when parsing.
!!! note
    type piracy? module local `_escape_string`?
"""
Base.escape_string(x::AbstractVector) = "$x"

"""
    with_log(s::AbstractString,p, delta=5;nomatch=false)

Log matching process of parser `p`, displaying `delta` characters left of and right of match.

If `nomatch==true`, also log when parser does not match.

See also: [`log_names`](@ref), [`with_effect`](@ref)
"""
with_log(s::AbstractString,p_, delta_char::Integer=5;nomatch=false) =
    let p = parser(p_), log=s
        SideeffectParser(nomatch ? log_effect : log_effect_match ,p, log, delta_char)
    end

function log_effect(s,start,after,state,log,delta)
    at = "@$(start)-$(after)"
    if state === nothing
        printstyled("no match ", color=:underline)
    else
        print("   ")
        printstyled("match";
                    bold=false,color=:underline)
        print(" ")
    end
    printstyled(log,color=:green, bold=false)
    print(at,": ")
    firsti = _prevind(s,start,delta)
    lasti = (_prevind(s,start))
    before, matched = if _prevind(s,start)<start
        escape_string(s[max(1,firsti):lasti]), escape_string(s[start:_prevind(s,after)])
    else
        "",""
    end
    if lastindex(matched)>100
        matched=matched[1:_nextind(matched,1,20)]*"[...]"*matched[_prevind(matched,end,20):end]
    end
    printstyled(before; bold=true)
    printstyled(matched; bold=true,color=:underline)
    li = after>lastindex(s) ? lastindex(s) : _nextind(s,after,delta)
    if state === nothing 
        printstyled(escape_string(s[after:min(end,li)]),
                    bold=true,color=:underline)
    elseif after<=lastindex(s)
        printstyled(escape_string(s[after:min(end,li)]),
                    color=:darkgray)
    end
    println()
    if !get(stdout,:color,false)
        print(" "^(11+length(at)+length(log)+length(before)),"^")
        if length(matched)>1
            print("_"^(length(matched)-2),"^")
        end
        println()
    end
end

function log_effect_match(s,start,after,state,log,delta)
    if state!==nothing && start!=after
        log_effect(s,start,after,state,log,delta)
    end
end




export NamedParser, with_name
"""
    NamedParser{P,S,T} <: WrappedParser{P,S,T}

Struct with
```julia
    name::Symbol
    parser::P
    doc::String
```
"""
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
    NamedParser(name,parser(x),doc)

with_name(name::AbstractString,x; doc="") =
    name=="" && doc=="" ? x : NamedParser(Symbol(name),parser(x),doc)

log_names_(x::CombinedParser,a...;kw...) = x


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
    @with_names

Sets names of parsers within begin/end block to match the variables they are asigned to.

so, for example
```jldoctest
julia> @with_names foo = AnyChar()
. AnyChar |> with_name(:foo)
::Char

julia> parse(log_names(foo),"ab")
   match foo@1-2: ab
                  ^
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

See also [`log_names(parser)`](@ref), [`@syntax`](@ref).
"""
macro with_names(block)
    esc(with_names(block))
end

export @seq
"""
    @seq(x...)

Create a sequence interleaved with whitespace (horizontal or vertical).
The result_type is omitting whitespace.
"""
macro seq(x...)
    r = if length(x)==1
        x
    else
        quote
            x_ = [$(x...)]
            sSequence( (i < lastindex(x_) ? (e*CombinedParsers.Regexp.whitespace_newline)[1] : e for (i,e) in enumerate(x_))...)
        end
    end
    esc(r)
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
🗄 Sequence |> map(ntuple) |> with_name(:german_street_address)
├─ .* AnyChar |> Repeat |> ! |> with_name(:street)
├─ \\  
└─ Int64  |> with_name(:no)
::NamedTuple{(:street, :no),Tuple{SubString,Int64}}

julia> german_street_address"Some Avenue 42"
NamedTuple{(:street, :no),Tuple{SubString,Int64}}(("Some Avenue", 42))


julia> @syntax for us_street_address in street_address
            Sequence(:no =>Numeric(Int),
                     " ",
                     :street => !Repeat(AnyChar()))
       end
🗄 Sequence |> map(ntuple) |> with_name(:us_street_address)
├─ Int64  |> with_name(:no)
├─ \\  
└─ .* AnyChar |> Repeat |> ! |> with_name(:street)
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
                global $name = begin
                    $(expr...)
                end
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
    elseif block.head == :block
        with_names(block)
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
    CharIn(pcre::String,x::T) where { T } = # <:Union{Char,Set{Char},<:Function,<:UnicodeClass,<:Tuple}}=
        new{T}(pcre,x)
end
CharIn(pcre::String,x::CharIn) =
    CharIn(pcre,x.sets)
CharIn(pcre::String,x::AbstractString) =
    CharIn(pcre,x...)
CharIn(pcre::String,x1,x_...) =
    CharIn(pcre, optimize(CharIn,x1,x_...))
CharIn(chars::String) =
    isempty(chars) ? Never() : CharIn("",chars...)
CharIn(x_...) =
    CharIn("",x_...)
CharIn(chars::StepRange) =
    CharIn("$(chars.start)-$(chars.stop)",chars)

regex_string_(x) = "$x"

"""
    CharIn(unicode_class::Symbol...)

succeeds if char at cursor is in one of the unicode classes.
"""
CharIn(unicode_classes::Symbol...) =
    CharIn(UnicodeClass(unicode_classes...))

@inline _ismatch(c,p::CharIn)::Bool = _ismatch(c,p.sets)

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
@inline _ismatch(c,p::CharNotIn)::Bool = !_ismatch(c,p.sets)

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
optimize!(charset::Nothing,otherstuff,x::T) where { T<:Union{Char,Integer} } =
    x,otherstuff
optimize!(charset::T,otherstuff,x::T) where { T<:Union{Char,Integer} } =
    optimize!(Set{T}(charset),otherstuff,x)
function optimize!(charset::Set{T},otherstuff,x::T) where { T<:Union{Char,Integer} }
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
optimize!(charset,otherstuff,x::Union{Vector,Tuple,StepRange{T,Int},Set{T},<:AbstractString}) where { T<:Union{Char,Integer} } =
    optimize!(charset,otherstuff,x...)
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
    Repeat_stop(p,stop; min=0, max=Repeat_max)

Repeat `p` until `stop` (`NegativeLookahead`), not matching `stop`.
Sets cursor **before** `stop`. Tries `min:max` times
Returns results of `p`.

```jldoctest
julia> p = Repeat_stop(AnyChar(),'b') * AnyChar()
🗄 Sequence
├─ 🗄* Sequence[2] |> Repeat
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
Repeat_stop(p,stop; min=0, max=Repeat_max) =
    Repeat(map(IndexAt(2),Sequence(NegativeLookahead(parser(stop)),parser(p)));min=min,max=max)

@deprecate rep_stop(a...;kw...) Repeat_stop(a...;kw...)

"""
    Repeat_until(p,until, with_until=false; wrap=identity, min=0, max=Repeat_max)

Repeat `p` until `stop` (with [`Repeat_stop`](@ref)).
and set point **after** `stop`.

Return a `Vector{result_type(p)}` if `wrap_until==false`, otherwise a `Tuple{Vector{result_type(p)},result_type(until)}`.

To transform the `Repeat_stop(p)` parser head, provide a function(::Vector{result_type(p)}) in `wrap` keyword argument, e.g.
```jldoctest
julia> p = Repeat_until(AnyChar(),'b') * AnyChar()
🗄 Sequence
├─ 🗄 Sequence[1]
│  ├─ (?>🗄*) Sequence[2] |> Repeat |> Atomic
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
Repeat_until(p,until, with_until=false;wrap=identity,min=0,max=Repeat_max) =
    if with_until
        Sequence(map(wrap,Atomic(Repeat_stop(p,until;min=min,max=max))), until)
    else
        map(IndexAt(1),Sequence(map(wrap,Atomic(Repeat_stop(p,until;min=min,max=max))), until))
    end

@deprecate rep_until(p,until) Repeat_until(p,until)



export FlatMap,after
"""
    FlatMap{P,S,Q<:Function,T} <: CombinedParser{S,T}

Like Scala's [fastparse FlatMap](https://www.lihaoyi.com/fastparse/#FlatMap).
See [`after`](@ref)
"""
@auto_hash_equals struct FlatMap{P,S,Q<:Function,T} <: CombinedParser{S,T}
    left::P
    right::Q
    function FlatMap{T}(right::Q, left::P) where {T, P<:CombinedParser, Q<:Function}
        new{P,Tuple{<:Any,<:Any,<:Any},Q,T}(left, right)
    end
end
flatmap_state(old,ls,rp,rs) = tuple_pos(rs), (ls,rp,tuple_state(rs))
left_state(state::Tuple) = state[1]
right_parser(state::Tuple) = state[2]
right_state(state::Tuple) = state[3]


children(x::FlatMap) = ( x.left, x.right )
function print_constructor(io::IO,x::FlatMap)
    print(io, "FlatMap" )
end
FlatMap(right::Function, left, T::Type=Any) = FlatMap{T}(right,parser(left))
FlatMap(right::Function, T::Type, left) = FlatMap{T}(right,parser(left))

"""
    after(right::Function,left::AbstractToken)
    after(right::Function,left::AbstractToken,T::Type)

Like Scala's fastparse [`FlatMap`](@ref)

```jldoctest
julia> saying(v) = v == "same" ? v : "different";

julia> p = after(saying, String, "same"|"but")
🗄 FlatMap
├─ |🗄... Either
│  ├─ same 
│  └─ but 
└─ saying
::String

julia> p("samesame")
"same"

julia> p("butdifferent")
"different"

```
"""
after(a...) = FlatMap(a...)

regex_inner(x::FlatMap)  = error("regex determined at runtime!")


@inline _nextind(str,i::Int,parser::FlatMap,x::Tuple) =
    let li = _nextind(str,i,parser.left,tuple_pos(x))
        _nextind(str,li,x[2],x[3])
    end

@inline _prevind(str,i::Int,parser::FlatMap,x::Tuple) =
    let li = _prevind(str,i,x[2],x[3])
        _prevind(str,li,parser.left,tuple_pos(x))
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
    Sequence(p::CombinedParser...) =
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

Sequence(p::Vector{<:ParserTypes}) =
    Sequence(p...)
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
    function ntuple(v)
        NT( tuple( (v[k.second] for k in names )... ))
    end
    map(ntuple, NT, s)
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
    # map(IndexAt(transform), s)
end





print_constructor(io::IO,x::Sequence) = print(io,"Sequence")
children(x::Sequence) = x.parts

Base.getindex(x::CombinedParser, i) = map(IndexAt(i),x)


export sSequence
sSequence_(x::Sequence) = sSequence_(x.parts...)
sSequence_(x::Always) = tuple()
sSequence_() = tuple()
sSequence_(x1) = tuple(parser(x1))
sSequence_(x1,x...) =
    Iterators.flatten(tuple( sSequence_(x1), collect(Iterators.flatten( ( sSequence_(e) for e in x ) ))))

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



@inline function _prevind(str,i::Int,parser::Sequence,x::MatchState)
    for p in length(parser.parts):-1:1
        i=_prevind(str,i,parser.parts[p],x)
    end
    i
end

@inline function _prevind(str,i::Int,parser::Sequence,x)
    for j in lastindex(x):-1:1
        (p,e) = parser.parts[j],x[j]
        i=_prevind(str,i,p,e)
    end
    i
end

@inline function _nextind(str,i::Int,parser::Sequence,x::MatchState)
    for p in parser.parts
        i=_nextind(str,i,p,MatchState())
    end
    i
end

@inline function _nextind(str,i::Int,parser::Sequence,x)
    for (p,e) in zip(parser.parts,x)
        i=_nextind(str,i,p,e)
    end
    i
end

function prune_captures(sequence,after_i)
end


Base.getindex(A::MatchState, i::Int) = MatchState()
Base.setindex!(A::MatchState, ::MatchState, i::Int) = nothing
Base.setindex!(A::MatchState, v, i::Int) = error("MatchState elements can only be ::MatchState")


sequence_state(statettype::Type{MatchState}, states) = MatchState()
sequence_state(statettype::Type{<:Tuple}, states) = tuple( (s for s in states...) )
sequence_state(statettype::Type, states) = states

sequence_state(statettype::Type{MatchState}) = MatchState()
sequence_state(statettype::Type{<:Tuple}) = tuple( )
sequence_state(statettype::Type) = Any[]

_iterate_(parser::Sequence, sequence, till, posi, next_i, states::MatchState) =
    nothing

function _iterate_(parser::Sequence, sequence, till, posi, next_i, states::Nothing)
    length(parser.parts) == 0 && return next_i, sequence_state(state_type(parser))
    sss = Vector{Any}(undef,length(parser.parts))
    sss[1] = nothing
    _iterate(parser, sequence, till, posi, next_i, sss, 1)
end

function _iterate_(parser::Sequence, sequence, till, posi, next_i, substate::Vector{Any}, p=length(substate))
    next_i_ = next_i
    part=parser.parts
    length(part) == 0 && return nothing
    pposi = [ 0 for _ in 1:(length(substate)+1)]
    pposi[1]=posi
    if p==length(substate)
        pposi[end]=next_i
    end
    while p<=length(substate)
        if iszero(pposi[p])
            pposi[p] = start_index(sequence, pposi[p+1], part[p], @inbounds substate[p])
        end
        if (@inbounds substate[p]) === nothing
            pposi[p+1] = pposi[p]
        end
        r = _iterate(part[p], sequence, till, pposi[p], pposi[p+1], substate[p])

        if r === nothing
            prune_captures(sequence, pposi[p])
            @inbounds substate[p] = nothing
            pposi[p+1] = pposi[p]
            p == 1 && return nothing
            p -= 1
        else
            pposi[p+1] = tuple_pos(r)
            @inbounds substate[p] = tuple_state(r)
            if p < length(substate)
                @inbounds substate[p+1]=nothing
            end
            p += 1
        end
    end
    pposi[end], sequence_state(state_type(parser), substate)
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
a?? |missing |> Lazy
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

```jldoctest
julia> Repeat(2,2,'a')
a{2}  |> Repeat
::Array{Char,1}


julia> Repeat(3,'a')
a{3,}  |> Repeat
::Array{Char,1}

```
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
Repeat(f::Union{Function,Type},a...;kw...) =
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


@deprecate rep(a...;kw...) Repeat(a...;kw...)

import Base.join

"""
    Base.join(x::Repeat,delim, infix=:skip)

Parser matching repeated `x.parser` separated by `delim`.
```jldoctest
julia> parse(join(Repeat(AnyChar()),','),"a,b,c")
3-element Array{Char,1}:
 'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
 'b': ASCII/Unicode U+0062 (category Ll: Letter, lowercase)
 'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)
```
```jldoctest
julia> parse(join(Repeat(AnyChar()),',';infix=:prefix),"a,b,c")
('a', [(',', 'b'), (',', 'c')])

julia> parse(join(Repeat(AnyChar()),',';infix=:suffix),"a,b,c")
([('a', ','), ('b', ',')], 'c')
```
"""
function Base.join(x::Repeat, delim_; infix=:skip)
    delim = parser(delim_)
    if infix==:prefix
        Sequence(x.parser, Repeat( Sequence( delim, x.parser ) ))
    elseif infix==:suffix
        Sequence(Repeat( Sequence( x.parser, delim ) ), x.parser)
    elseif infix==:skip
        ## todo: the get function could be optimized
        ##@show x.range
        map(x.parser * Repeat(
            max(0,x.range.start-1),
            x.range.stop == Repeat_max ? Repeat_max : x.range.stop-1,
            Sequence(2, delim,x.parser ))) do (f,r)
                pushfirst!(r,f)
                r::result_type(x)
            end
    else
        error("unknown delim=$delim, infix=$infix")
    end
end

"""
    Base.join(x::CombinedParser,delim)

Shorthand for `join(Repeat(x),delim)`.
"""
Base.join(x::CombinedParser,delim; kw...) =
    join(Repeat(x),delim;kw...)

"""
    Base.join(f::Function, x::CombinedParser, delim)

Shorthand for `map(f,join(x,delim))`.
"""
Base.join(f::Function,p::CombinedParser,delim_; kw...) =
    map(f,join(p,delim_; kw...))

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


reversed(x::Repeat) = x





@inline function _prevind(str,i::Int,parser::Repeat,x::Int)
    for e in 1:x
        i=_prevind(str,i,parser.parser,MatchState())
    end
    i
end

@inline function _nextind(str,i::Int,parser::Repeat,x::Int)
    for e in 1:x
        i=_nextind(str,i,parser.parser,MatchState())
    end
    i
end

@inline function _nextind(str,i::Int,parser::Repeat,x::Vector)
    for e in x
        i=_nextind(str,i,parser.parser,e)
    end
    i
end

@inline function _prevind(str,i::Int,parser::Repeat,x::Vector)
    for j in lastindex(x):-1:1
        @inbounds i=_prevind(str,i,parser.parser,x[j])
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
        posi = _prevind(sequence,next_i_,t.parser,lstate) ##state[end][1]
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
struct NoMatch end
Base.show(io::IO, ::NoMatch) = print(io,"n/a")
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
        new{typeof(p),Union{NoMatch,state_type(p)},T_}(p, default)
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
    printstyled(io, "|$(x.default)",color=:blue)
    #print(io, " |> Optional(default=$(x.default))")
end


@inline _prevind(str,i::Int,parser::Optional,x::NoMatch) = i
@inline _nextind(str,i::Int,parser::Optional,x::NoMatch) = i



_iterate(t::Optional, str, till, posi, next_i, state::NoMatch) =
    nothing

function _iterate(t::Optional, str, till, posi, next_i, state)
    posi = state === nothing ? next_i : _prevind(str,next_i,t.parser,state) ##state[end][1]
    r = _iterate(t.parser, str, till, posi, next_i, state)
    if r === nothing
        prune_captures(str,posi)
        return tuple(posi, NoMatch())
    else
        r
    end
end

function _iterate(t::Lazy{<:Optional}, str, till, posi, next_i, state::Nothing)
    next_i, NoMatch()
end

function _iterate(t::Lazy{<:Optional}, str, till, posi, next_i, state::NoMatch)
    _iterate(t.parser.parser, str, till, posi, next_i, nothing)
end

function _iterate(t::Lazy{<:Optional}, str, till, posi, next_i, state)
    _iterate(t.parser.parser, str, till, posi, next_i, state)
end


defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type) = missing
defaultvalue(V::Type{<:CombinedParser}) = Always()





export alt, Either
"""
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
    Either{S,T}(p::P) where {S,T,P} =
        new{P,S,T}(p)
end
Either(p::P) where P = 
    Either{either_state_type(p),either_result_type(p)}(p)
Either{T}(p) where T = 
    Either{either_state_type(typeof(p)),T}(p)

function Either{T}(p_...) where {T}
    p = Any[parser.(p_)...]
    for x in p
        result_type(x) <: T || error("$(result_type(x))<:$T")
    end
    Either{either_state_type(Vector{Any}),T}(p)
end

function Either(x...; kw...)
    Either(Any[ (parser(y) for y in x)..., (with_name(k,v) for (k,v) in kw)...  ])
end
"""
    Either(transform::Function, x::Vararg)

abbreviation for `map(transform, Either(x...))`.
"""
function Either(transform::Function, x...; kw...)
    map(transform, Either(x...;kw...))
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
sEither(x1::NamedParser,x...) =
    with_name(x1.name, sEither(x1.parser, x...), doc=x1.doc)


either_state_type(ts::Type{Vector{Any}}) = Tuple{Int,Any}
either_state_type(ts::Type{<:CombinedParser}) = Tuple{Int,Any}
either_state_type(ts::Type{<:Vector}) = Tuple{Int,state_type(eltype(ts))}
either_state_type(ts::Type{<:Tuple}) = Tuple{Int,promote_type(state_type.(fieldtypes(ts))...)}
either_state_type(ts::Type...) = Tuple{Int,promote_type(state_type.(ts))}
either_state_type(x) = either_state_type(typeof(x))
@inline with_state!(x::Nothing,k::Int,s) = (k,s)
function promote_type_union(Ts...)
    T = promote_type(Ts...)
    Any <: T ? Union{Ts...} : T
end

"return tuple(state_type,result_type)"
function either_result_type(ts)
    promote_type_union(result_type.(ts)...)
end

children(x::Either) = x.options
regex_string(x::Either) = join(regex_string.(x.options),"|")
regex_prefix(x::Either) = "|"
regex_inner(x::Either) = join([ regex_string(p) for p in x.options],"|")
regex_suffix(x::Either) = "..."
print_constructor(io::IO,x::Either) = print(io,"Either")


"""
    Base.push!(x::Either, option)

Push `option` to `x.options` as parser tried next if `x` fails.
Recursive parsers can be built with `push!` to `Either`.

See also [`pushfirst!`](@ref).
"""
function Base.push!(x::Either{<:Vector,<:Any}, y_)
    y = parser(y_)
    promote_type(result_type(y),result_type(x)) <: result_type(x) || error("$(result_type(y)) <: $(result_type(x)). Fix with `push!(x|$(typeof(y)),y)`.")
    push!(x.options,y)
    y
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

function Base.push!(x::NamedParser, y)
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

@inline function _prevind(str,i::Int,parser::Either,x)
    ## @show i
    _prevind(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end

@inline function _nextind(str,i::Int,parser::Either,x)
    ## @show i
    _nextind(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end
@inline function _nextind(str,i::Int,parser::Either{P,T},x::Tuple{Int,T}) where {P,T}
    _nextind(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end
 

@generated function _prevind(str,i::Int,parser::Either{pts},x::Union{Pair,MutablePair}) where {pts<:Tuple}
    fpts = fieldtypes(pts)
    parseoptions = [
        quote
        if j === $p
        return _prevind(str,i,parser.options[$p],s) # $(part[p]),s)
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

include("trie.jl")

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
function _nextind(sequence, i::Int, parser::Atomic, state::MatchState)
    @show a, s = _iterate(parser.parser,sequence,lastindex(sequence), i, i, nothing)
    _nextind(sequence, i, parser.parser, s)
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




include("reverse.jl")
include("get.jl")
include("operators.jl")

hex_digit = CharIn("[:xdigit:]",'A':'F','a':'f','0':'9')
export hex_digit, integer_base
"""
    integer_base(base,mind=0,maxd=Repeat_max)

Parser matching a integer format on base `base`.
"""
function integer_base(base=10,mind=0,maxd=Repeat_max)
    dig = if base == 16
        hex_digit
    elseif base == 8
        CharIn('0':'7')
    elseif base ==10
        CharIn('0':'9')
    else
        error()
    end
    Repeat(mind:maxd,dig) do v
        (isempty(v) ? 0 : parse(Int,join(v),base=base))::Int
    end
end

include("deepmap.jl")

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

export optimize
optimize(x) = deepmap_parser(_optimize,x)
_optimize(x,a...) = x
deepmap_parser(::typeof(_optimize),dict::AbstractDict,x::SideeffectParser) = x.parser

include("re.jl")


include("show.jl")



children(x::PositiveLookbehind) =
    children(x.parser)

include("memoize.jl")
include("caseless.jl")

children(x::MappingParser) =
    ( x.parser, x.f)


export trim
"""
    trim(; whitespace=CharIn(' '))

Match any whitespace and result in `tuple()`.
"""
trim(; whitespace=Atomic(Repeat(horizontal_space_char))) =
    map(whitespace) do v
        tuple()
    end

"""
    trim(p; whitespace=CharIn(' '))

Ignore whitespace at left and right of `p`.
"""
trim(p; whitespace=Atomic(Repeat(horizontal_space_char))) =
    Sequence(2, whitespace, p, whitespace)


export @trimmed
trimmed(x) = x
function trimmed(node::Expr)
    if node.head == :(=) && length(node.args) == 2 && isa(node.args[1], Symbol)
        node.args[2] = Expr(:call, :trim, node.args[2])
    end
    if node.head != :call 
        node.args = map(trimmed, node.args)
    end
    node
end


"""
    @trimmed

Sets names of parsers within begin/end block to match the variables they are asigned to.

so, for example
```jldoctest
julia> @trimmed foo = AnyChar()
. AnyChar |> with_name(:foo)
::Char

julia> parse(log_names(foo),"ab")
   match foo@1-2: ab
                  ^
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

See also [`log_names(parser)`](@ref), [`@syntax`](@ref).
"""
macro trimmed(block)
    esc(trimmed(block))
end

mutable struct Delayed
    name::Symbol
end

end # module



