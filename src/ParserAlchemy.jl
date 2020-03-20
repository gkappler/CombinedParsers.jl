module ParserAlchemy
import Base: (*), (|), cat, get, prevind, nextind
using Parameters
using Nullables

using TextParse
import TextParse: tryparsenext
import Base: ==, hash

using BasePiracy

export tryparsenext, tokenize, result_type

include("namedtuples.jl")


ParserTypes = Union{TextParse.AbstractToken, AbstractString, Char, Regex}
export parser
parser(x::ParserTypes) = x



############################################################
## Parsing with TextParse.AbstractToken, operators

function _iterate(parser::TextParse.AbstractToken, sequence, till, i, state)
    parser isa AbstractParser && @warn "define _iterate(parser::$(typeof(parser)), sequence, till, i, state)"
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
function Base.get(parser::TextParse.AbstractToken, sequence, till, after, i, state)
    parser isa AbstractParser && @warn "define Base.get(parser::$(typeof(parser)), sequence, till, after, i, state)"
    state[2]
    ## === missing ? missing : get(Parsing(x.parser.parser,x.sequence), after, (i, till, state))
end
@inline function nextind(str,i::Int,parser::TextParse.AbstractToken,x)
    parser isa AbstractParser && @warn "define nextind(str,i::Int,parser::$(typeof(parser)),x)"
    i+x[1]
end
@inline function prevind(str,i::Int,parser::TextParse.AbstractToken,x)
    parser isa AbstractParser && @warn "define prevind(str,i::Int,parser::$(typeof(parser)),x)"
    i-x[1]
end
result_type(x::ParserTypes) = result_type(typeof(x))
result_type(x::Type{<:TextParse.AbstractToken{T}}) where T = T

struct MatchState end

abstract type AbstractParser{T} <: TextParse.AbstractToken{T} end
Base.get(parser::AbstractParser{Nothing}, sequence, till, after, i, state) =
    nothing
_iterate(x::AbstractParser,str,i,till,state) =
    error("implement _iterate(x::$(typeof(x)),str,i,till,state::$(typeof(state)))")
function TextParse.tryparsenext(x::AbstractParser,str,i,till,opts=TextParse.default_opts)
    s = _iterate(x,str,i,till,nothing)
    if s === nothing
        Nullable{result_type(x)}(),i
    else
        Nullable(get(x,str,till,s[1],i,s[2])),s[1]
    end
end

"Abstract type for parser wrappers, providing default methods"
abstract type WrappedParser{T} <: AbstractParser{T} end
@inline prevind(str,i::Int,parser::WrappedParser,x) =
    prevind(str,i,parser.parser,x)
@inline nextind(str,i::Int,parser::WrappedParser,x) =
    nextind(str,i,parser.parser,x)
regex_string(x::WrappedParser) = regex_string(x.parser)
Base.get(x::WrappedParser, a...) =
    get(x.parser,a...)
Base.get(parser::WrappedParser, sequence, till, after, i, state) = 
    get(parser.parser, sequence, till, after, i, state)
_iterate(parser::WrappedParser, sequence, till, i, state) =
    _iterate(parser.parser, sequence, till, i, state)


"wrapper for stepping with ncodeunit length."
struct ConstantParser{N,T} <: WrappedParser{T}
    parser::T
end
parser(x::Char) =
    ConstantParser{Base.ncodeunits(x),Char}(x)
parser(x::AbstractString) =
    ConstantParser{Base.ncodeunits(x),SubString}(x)

regex_string(x::ConstantParser) = regex_string(x.parser)
@inline nextind(str,i::Int,parser::ConstantParser{L},x) where L =
    i+L
@inline prevind(str,i::Int,parser::ConstantParser{L},x) where L = 
    i-L

# Base.get(parser::ConstantParser, sequence, till, after, i, state) = parser.parser

Base.get(parser::ConstantParser{1,Char}, sequence, till, after, i, state) where L =
    sequence[i]

Base.get(parser::ConstantParser{L,SubString}, sequence, till, after, i, state) where L =
    parser=="" ? "" : SubString(sequence,i,prevind(sequence,i+L))

@inline function _iterate(parser::ConstantParser{L,Char}, sequence, till, i, state::Nothing) where L
    i>till && return nothing
    if parser.parser == ( sequence[i])
        i+L, MatchState()
    else
        nothing
    end
end

@inline function _iterate(parser::ConstantParser{L,<:AbstractString}, sequence, till, i, state::Nothing) where L
    state !==nothing && return(nothing)
    j = i
    k = 1
    while k<=L
        (j > till) && return(nothing)
         pc=parser.parser[k]
         sc=sequence[j]
        (pc != sc) && return(nothing)
         j = j + ncodeunits(sc)
         k = k + ncodeunits(pc)
    end
    return j, MatchState()
end

function _iterate(parser::ConstantParser, sequence, till, i, state::Nothing)
    _iterate(parser.parser, sequence, till, i, state)
end


"Abstract type for stepping with previndex/nextindex, accounting for ncodeunit length of chars at point."
abstract type NIndexParser{N,T} <: AbstractParser{T} end
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

Base.get(parser::NIndexParser{0}, sequence, till, after, i, state) =
    parser
Base.get(x::NIndexParser{1,Char}, sequence, till, after, i, state) =
    sequence[i]

export AnyChar, any
struct AnyChar <: NIndexParser{1,Char} end
any() = AnyChar()
regex_string(x::AnyChar) = "."


@inline function _iterate(parser::AnyChar, sequence, till, i, state::Nothing)
    i>till && return(nothing)
    nc = Base.ncodeunits(sequence[i])
    return i+nc, MatchState()
end


"Parsers that do not consume any input can inherit this type"
abstract type LookAround <: NIndexParser{0,Nothing} end

export AtStart, AtEnd
struct AtStart <: NIndexParser{0,AtStart} end
regex_string(x::AtStart) = "^"
_iterate(parser::AtStart, sequence, till, i, state::Nothing) =
    i == 1 ? (i, MatchState()) : nothing

struct AtEnd <: NIndexParser{0,AtEnd} end
regex_string(x::AtEnd) = "\$"
_iterate(parser::AtEnd, sequence, till, i, state::Nothing) =
    i > till ? (i, MatchState()) : nothing

Base.show(io::IO, x::Union{AtStart,AtEnd}) =
    print(io,regex_string(x))


export Always
"Parser matching always and not consuming any input"
struct Always <: LookAround
end
Base.show(io::IO,x::Always) = print(io,"")
regex_string(x::Always) = ""
_iterate(parser::Always, str, till, i, s::Nothing) =
    i, MatchState()

##_iterate(parser::Never, str, till, i, s) = nothing

export PositiveLookahead
"""
wraps a `parser::P`, succeeds if and only if `parser` succeeds, but consumes no input.
The match is returned.
Useful for checks like "must be followed by `parser`, but don't consume its match".
"""
struct PositiveLookahead{P} <: LookAround
    parser::P
    PositiveLookahead(p) =
        new{typeof(p)}(p)
end

function _iterate(t::PositiveLookahead, str, till, i, state::Nothing)
    r = _iterate(t.parser, str, till, i, nothing)
    if r === nothing
        nothing
    else
        i,MatchState()
    end
end


export NegativeLookahead
"""
wraps a `parser::P`, succeeds if and only if `parser` does not succeed, but consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct NegativeLookahead{P} <: LookAround
    parser::P
end

function _iterate(t::NegativeLookahead, str, till, i, state::Nothing)
    r = _iterate(t.parser, str, till, i, nothing)
    if r === nothing
        i,MatchState()
    else
        nothing
    end
end

export look_ahead
function look_ahead(match::Bool, p_)
    p = parser(p_)
    if match
        PositiveLookahead(p)
    else
        NegativeLookahead(p)
    end
end

 








struct PartialMatchException{S,P} <: Exception
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


############################################################
## Transformations

log_transform(transform, log, catch_error=false) =
    if catch_error
        (v,i) -> try
            r=transform(v,i)
            log && @info "transformed" transform v r
            return r
        catch f
            @error "transform error: " f RT v tokens transform
            rethrow(f)
        end
    elseif log
        (v,i) -> begin
            r=transform(v,i)
            @info "transformed" transform v r
            return r
        end  
    else
        transform
    end


export with_log
struct ParserPeek{T,P} <: WrappedParser{T}
    message::String
    length::Int
    parser::P
    ParserPeek(message,length, p::ParserTypes) =
        new{result_type(p),typeof(p)}(message, length, p)
end
Base.show(io::IO,x::ParserPeek) = print(io,"(?#$(x.message))",x.parser)
with_log(message, p_,length=5) =
    let p = parser(p_)
        ParserPeek(string(message),length,p)
    end
regex_string(x::ParserPeek{Nothing,Always}) = "$x"

################################################################################

export map_parser
map_parser(f::Function,x::ParserPeek,a...) =
    with_log(x.message,
             map_parser(f,x.parser,a...),x.length)

@inline function _iterate(parser::ParserPeek, sequence, till, i, state)
    r = _iterate(parser.parser, sequence, till, i, state)
    if r!==nothing
        @info parser.message
        #i r
    end
    r
end


export NamedToken, with_name
struct NamedToken{P,T} <: WrappedParser{Pair{Symbol,T}}
    name::Symbol
    parser::P
    NamedToken(name::Symbol,parser) =
        new{typeof(parser),result_type(parser)}(name,parser)
end
parser(x::Pair{Symbol, P}) where P =
    NamedToken(x.first, parser(x.second))
with_name(name::Symbol,x; doc="") = 
    NamedToken{typeof(x),result_type(x)}(name,x)

map_parser(f::Function,x::NamedToken,a...) =
    NamedToken(x.name,map_parser(f,x.parser,a...))


export InstanceParser, instance
struct InstanceParser{P,T, F<:Function} <: WrappedParser{T}
    transform::F
    parser::P
    InstanceParser{T}(transform::F, p::P) where {T, F<:Function,P} =
        new{P,T,F}(transform, p)
end
map_parser(f::Function,x::InstanceParser,a...) =
    InstanceParser{result_type(x)}(x.transform,map_parser(f,x.parser,a...))
parser(constant::Pair{<:ParserTypes}) =
    InstanceParser{typeof(constant.second)}(
        (v,i) -> constant.second,
        parser(constant.first))

regex_string(x::Union{NamedToken, InstanceParser}) = regex_string(x.parser)

function Base.get(parser::InstanceParser, sequence, till, after, i, state)
    parser.transform(
        get(parser.parser,sequence, till, after, i, state)
        ,i)
end

function _iterate(parser::InstanceParser, sequence, till, i, state)
    r = _iterate(parser.parser, sequence, till, i, state )
end


export instance 
function instance(::Type{T}, p::P, a...) where {T, P<:ParserTypes}
    InstanceParser{T}((v,i) -> T(a..., v), p)
end
function instance(::Type{T}, p::P) where {T, P<:ParserTypes}
    InstanceParser{T}((v,i) -> _convert(T,v), p)
end
function instance(f::Function, ::Type{T}, p::P, a...) where {T, P<:ParserTypes}
    InstanceParser{T}((v,i) -> _convert(T,f((v), i, a...)), p)
end
@deprecate instance(::Type{T}, f::Function, p::P, a...) where {T, P<:ParserTypes} instance(f,T,p,a...)



import Base: in
include("unicode.jl")

export CharIn
struct CharIn{S} <: NIndexParser{1,Char}
    sets::S
end
CharIn(x::String) = CharIn(tuple( (c for c in x)...) )
function Base.in(x::Char, set::CharIn{<:Tuple})
    for s in set.sets
        x in s && return true
    end
    return false
end
result_type(::Type{<:CharIn}) = Char
regex_string_(x::Char) = x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string(x::Char) = x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string_(x::StepRange) =
    if x.start == x.stop
        x.start
    else
        x.start*"-"*x.stop
    end
regex_string_(x::CharIn) = join([regex_string_(s) for s in x.sets])
regex_string(x::CharIn) =
    "["*regex_string_(x)*"]"




@inline function _iterate(parser::CharIn, sequence, till, i, state::Nothing)
    i>till && return(nothing)
    c = sequence[i]
    for s in parser.sets
        if c in s
            nc = Base.ncodeunits(c)
            return i+nc, MatchState()
        end
    end
    return nothing
end

export CharNotIn
"TODO: replace with CharIn{false,S}, xor logic"
struct CharNotIn{S} <: NIndexParser{1,Char}
    sets::S
end
result_type(::Type{<:CharNotIn}) = Char
regex_string(x::CharNotIn) =
    "[^"*join([regex_string_(s) for s in x.sets])*"]"
function Base.in(x::Char, set::CharNotIn{<:Tuple})
    for s in set.sets
        x in s && return false
    end
    return true
end

@inline function _iterate(parser::CharNotIn, sequence, till, i, state::Nothing)
    i>till && return(nothing)
    c = sequence[i]
    for s in parser.sets
        c in s && return nothing 
    end
    nc = Base.ncodeunits(c)
    return i+nc, MatchState()
end


export CharMatcher
CharMatcher = Union{Char, AnyChar, CharIn, CharNotIn, UnicodeClass,StepRange{Char,Int},AbstractString}


CharIn(x::Union{Missing,CharMatcher}...) =
    CharIn(tuple((e for e in x if e!==missing)...))

CharNotIn(x::Union{Missing,CharMatcher}...) =
    CharNotIn(tuple((e for e in x if e!==missing)...))


export rep_stop, rep_until
"""
rep_stop(p,stop)

Repeat `p` until `stop` (`NegativeLookahead`), not matching `stop`.
Map results of `p` only.
"""
rep_stop(p,stop) =
    rep(seq(NegativeLookahead(parser(stop)),parser(p); transform=2))

"""
rep\\_until(p,until, with_until=false;wrap=identity)

Repeat `p` until `stop` (see `rep_stop`), 
followed by `stop` (returned iif `wrap_until==true`).

To map repeats of `p`, the `rep_stop` is `wrap`ped by keyword argument, e.g.

parse(rep_until(AnyChar(),'b'),"acb")

[ 'a', 'c' ]

parse(rep_until(AnyChar(),'b';wrap=JoinSubstring),"acb")

"ac"
"""
rep_until(p,until, with_until=false;wrap=identity) =
    seq(wrap(rep_stop(p,until)), until;
        transform = with_until ? nothing : 1)


map_parser(f::Function,x::Union{ConstantParser,AtStart,AtEnd},a...) = f(x,a...)
map_parser(f::Function,x::Union{Char,AbstractString,AnyChar,CharIn,CharNotIn,UnicodeClass,Always},a...) = f(x,a...)

indexed_captures(x::Union{ConstantParser,AtStart,AtEnd,Char,AbstractString,AnyChar,CharIn,CharNotIn,UnicodeClass,Always,LookAround},context) = x

export FlatMap,after
struct FlatMap{T,P,Q<:Function} <: AbstractParser{T}
    left::P
    right::Q
    function FlatMap{T}(left::P, right::Q) where {T, P, Q<:Function}
        new{T,P,Q}(left, right)
    end
end

after(right::Function,left::ParserTypes,T::Type) =
    FlatMap{T}(left,right)

map_parser(f::Function,x::FlatMap,a...) =
    FlatMap{result_type(x)}(map_parser(f,x.left),x.right)

regex_string(x::FlatMap)  = error("regex determined at runtime!")


@inline nextind(str,i::Int,parser::FlatMap,x) =
    let li = nextind(str,i,parser.left,x[1])
        nextind(str,li,x[2],x[3])
    end

@inline prevind(str,i::Int,parser::FlatMap,x) =
    let li = prevind(str,i,x[2],x[3])
        prevind(str,li,parser.left,x[1])
    end

    


function Base.get(parser::FlatMap, sequence, till, after, i, state)
    # @show state[2]
    # @show till, after, i, state[1]
    # @show state[3]
    li = nextind(sequence,i,parser.left,state[1])
    ## @show sequence[li:till]
    ## @show typeof(state[2])
    get(state[2],sequence, till, after,
              li,
              state[3])
end

function _iterate(tokf::FlatMap, str, till, i, state)
    T = result_type(tokf)
    if state === nothing
        lr = _iterate(tokf.left, str, till, i, nothing)
        lr === nothing && return nothing
        @show i_ = lr[1]
        rightp = tokf.right(get(tokf.left, str, till, lr[1],i,lr[2]))
        rr = nothing
        while rr === nothing
            rr = _iterate(rightp, str, till, i_, nothing)
            if rr === nothing
                lr = _iterate(tokf.left, str, till, i, lr[2])
                lr === nothing && return nothing
                i_ = lr[1]
            else
                return rr[1], (lr[2], rightp, rr[2])
            end
        end
    end
end



export seq
struct Sequence{T,P<:Tuple,F<:Function} <: AbstractParser{T}
    parts::P
    transform::F
    function Sequence{T}(p::P, f::F) where {T, P<:Tuple,F<:Function}
        new{T,P,F}(p, f)
    end
end
parser_types(::Type{Sequence{T, P, F}}) where {T, P, F} =
    P
map_parser(f::Function,x::Sequence,a...) =
    Sequence{result_type(x)}(tuple( (map_parser(f,p,a...)
                                     for p in x.parts)... ),
                             x.transform)


@inline function prevind(str,i::Int,parser::Sequence,x::MatchState)
    for p in reverse(parser.parts)
        i=prevind(str,i,p,MatchState())
    end
    i
end

@inline function prevind(str,i::Int,parser::Sequence,x)
    for (p,e) in zip(reverse(parser.parts),reverse(x))
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
    parser.transform(tuple(r...),i)
end


@inline function start_index(sequence,after,parser,state)
    r=state === nothing ? after : prevind(sequence, after, parser, state)
end

"""
_iterate(parser, sequence, till, i, states)

Note: `i` is the index in `sequence` after `parser` match according to `state` (and not the start of the match), 
such that `start_index(sequence,after,parser,state)` returns the start of the matching subsequence,
and sequence[start_index(sequence,after,parser,state):prevind(sequence,i)] is the matched subsequence.
"""
function _iterate(parser::Sequence, sequence, till, i, states)
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
    while nexti<=length(states)
        ## compute before next iteration, because states[nexti] might change.
        ## only used if ns===nothing, but states[nexti] might still be modified.
        before_i = start_index(sequence, i_, parts[nexti], states[nexti])
        if before_i==0
            println(parts[nexti])
            error()
        end
        ns = _iterate(parts[nexti], sequence, till, i_, states[nexti])
        if ns === nothing
            nexti -= 1
            nexti == 0 && return nothing
            i_ = before_i
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

function seq(tokens::Vararg{ParserTypes};
             transform=nothing, kw...)
    parts = tuple( ( parser(x) for x = tokens )... )
    T = [ result_type(typeof(x)) for x in parts]
    ## error()
    if transform isa Integer
        seq(T[transform], parts...; 
            transform = (v,i) -> v[transform], kw...)
    elseif transform===nothing
        seq(Tuple{(T...)}, parts...; 
            transform = (v,i) -> tuple(v...), kw...)
    else
        seq(Tuple{(T...)}, parts...; 
            transform = transform, kw...)
    end
end

seq(transform::Function, a...; kw...) =
    seq(a...; transform=transform, kw...)

function seq(T::Type, tokens::Vararg;
             combine=false, outer=nothing,
             partial = false,
             log=false,
             transform=:instance, kw...)
    parts = tuple( ( parser(x) for x = tokens )... )
    if combine
        if outer===nothing
            outer = Regex("^"*join([ "("*regex_string(x)*")" for x in parts ]))
        else
            @assert outer==join([ "("*regex_string(x)*")" for x in parts ])
        end
    end
    if T==NamedTuple
        fnames = tuple( [ x.name for x in parts if x isa NamedToken ]... )
        ftypes = [ result_type(typeof(x.parser)) for x in parts if x isa NamedToken ]
        RT = isempty(fnames) ? T : NamedTuple{fnames, Tuple{ftypes...}}
    else
        RT = T
    end
    if transform == :instance
        transform = (v,i) -> construct(RT,Any[ remove_null(x) for x in v ])
    end
    tr = log_transform(transform, log)
    result = Sequence{RT}(parts, tr)
    if outer === nothing
        result
    else
        if true || regex_string(result) == regex_string(outer)
            re_inner = ( "^" * join([ "(" * regex_string(t) * ")" for t in parts ])) ## when??? * '$' )             
            ## @warn "compiling regex" re Regex(re_inner) maxlog=1
            TokenizerOp{:seq_combine, RT}(  ( outer=outer::Regex,
                                              parts=parts, log=log, partial=partial ) , tr)
        else
            tok(outer, result)
            # instance(RT, (v,i) -> tokenize(result, v), outer)
        end
    end
end





(*)(x::Any, y::TextParse.AbstractToken) = seq(parser(x),y)
(*)(x::TextParse.AbstractToken, y::Any) = seq(x,parser(y))
(*)(x::TextParse.AbstractToken, y::TextParse.AbstractToken) = seq(x,y)

regex_string(x::Sequence)  = join([ regex_string(p) for p in x.parts])





export rep, rep1
struct Repeat{P,T,F<:Function} <: WrappedParser{T}
    range::UnitRange{Int}
    parser::P
    transform::F
end
Repeat{T}(parser::P, transform::F) where {T,P,F<:Function} =
    Repeat{P,T,F}((0:typemax(Int)),parser, transform)
Repeat{T}(range::UnitRange{Int},parser::P, transform::F) where {T,P,F<:Function} =
    Repeat{P,T,F}(range,parser, transform)
Repeat{T}(min::Integer,max::Integer,parser::P, transform::F) where {T,P,F<:Function} =
    Repeat{P,T,F}((min:max),parser, transform)
Repeat{T}(min::Integer,parser::P, transform::F) where {T,P,F<:Function} =
    Repeat{P,T,F}((min:typemax(Int)),parser, transform)

map_parser(f::Function,x::Repeat,a...) =
    Repeat{result_type(x)}(x.range,
                           map_parser(f,x.parser,a...),
                           x.transform)
                           


rep_suffix(x::Repeat) =
    if x.range.start == 0
        if x.range.stop == typemax(Int)
            "*"
        else            
            "{,$(x.range.stop)}"
        end
    else
        if x.range.stop == typemax(Int)
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

function regex_string(x::Repeat)
    r = regex_string(x.parser)
    op = rep_suffix(x)
    "$r$op"
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
    for e in reverse(x)
        i=prevind(str,i,parser.parser,e)
    end
    i
end

function Base.get(parser::Repeat, sequence, till, after, i, state::Vector)
    r = Vector{Any}(undef,length(state))
    i_=i
    for (p,s) in enumerate(state)
        after_=nextind(sequence,i_,parser.parser,s)
        r[p] = get(parser.parser,sequence, till, after_, i_, s)
        i_=after_
    end
    parser.transform(r,i)
end

function Base.get(parser::Repeat, sequence, till, after, i, state::Int)
    r = Vector{Any}(undef,state)
    i_=i
    s=MatchState()
    for p in 1:state
        after_=nextind(sequence,i_,parser.parser,s)
        r[p] = get(parser.parser,sequence, till, after_, i_, s)
        i_=after_
    end
    parser.transform(r,i)
end

@inline pushstate!(state::Nothing,parser,substate::T) where T =
    T[substate]
@inline function pushstate!(state::Vector,parser,substate)
    push!(state,substate)
end
@inline function poplast!(state::Vector,parser)
    l=pop!(state)
    l,state
end
@inline state_length(parser::Repeat,x::Vector{}) = length(x)
@inline state_length(parser::Repeat,x::Nothing) = 0


@inline pushstate!(state::Nothing,parser,substate::MatchState) =
    1

@inline pushstate!(state::Int,parser,substate) =
    push!(Any[ MatchState() for i in 1:state ], substate)
@inline pushstate!(state::Int,parser,substate::MatchState) =
    state + 1
@inline poplast!(state::Int,parser) =
    MatchState(), state - 1
@inline state_length(parser,state::Int) = state

function _iterate(t::Repeat, sequence, till, i, state)
    function fill(j,state_)
        while (x = _iterate(t.parser,sequence, till,j,nothing))!==nothing && state_length(t,state_) < t.range.stop
            j==x[1] && state_length(t,state_)>0 && break
            state_=pushstate!(state_,t.parser,x[2])
            j==x[1] && break
            j = x[1]
        end
        j,state_
    end
    i_ = i
    if state === nothing
        state = 0
        i_,state_ = fill(i,state)
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
        goback = true
        while goback
            lstate, state_=poplast!(state,t.parser)
            before_i = prevind(sequence,i_,t.parser,lstate) ##state[end][1]
            x = _iterate(t.parser,sequence, till, i_, lstate)
            if x === nothing
                state_length(t,state_) in t.range && return before_i, state_
                i_ = before_i
                if state_length(t,state_)==0
                    goback = false
                end
                state = state_
            else
                state_=pushstate!(state_,t.parser,x[2])
                i_,state_ = fill(x[1],state_)
                goback = false
            end
        end
    end
    if state_length(t,state_) in t.range
        return i_, state_
    else
        nothing
    end
end

rep1(x_;  kw...) where T =
    let x=parser(x_)
        rep1(Vector{result_type(typeof(x))},x; kw...)
    end
rep1(T::Type, x;  log=false, transform=(v,i) -> v) =
    rep1(log_transform(transform, log),T, x)
rep1(transform::Function, T::Type, x) =
    Repeat{T}(1,typemax(Int),parser(x), transform)


rep1(T::Type, x,y::Vararg; log=false, transform=(v,i) -> v, kw...) =
    rep1(T, seq(x,y...; transform=log_transform(transform, log), kw...), transform=(v,i) -> v)

rep(x, minmax::Tuple{<:Integer,<:Integer}=(0,typemax(Int)); kw...) =
    let p=parser(x)
        rep(Vector{result_type(p)},p,minmax; kw...)
    end

rep(T::Type, x, minmax::Tuple{<:Integer,<:Integer}=(0,typemax(Int));  log=false, transform=(v,i) -> v ) = 
    Repeat{T}(minmax...,parser(x), log_transform(transform, log))

rep(T::Type, x,y::Vararg; log=false, transform=(v,i) -> v, kw...) =
    rep(T, seq(x,y...; transform=log_transform(transform, log), kw...); transform=(v,i) -> v)

rep(transform::Function, a...; kw...) =
    rep(a...; transform=transform, kw...)







export opt
struct Optional{T,P,F<:Function} <: WrappedParser{T}
    parser::P
    default::T
    transform::F
    function Optional{T}(parser::P,default::D,f::F) where {T,P,D,F<:Function}
        T_ = promote_type(T,D)
        T_ === Any && ( T_ = Union{T,D} )
        new{T_,P,F}(parser, default,f)
    end
end
Optional{T}(parser::P,f::F) where {T,P,F<:Function} =
    Optional{T}(parser, defaultvalue(T), f)
regex_string(x::Optional) where {T, E}  = regex_string(x.parser)*"?"
map_parser(f::Function,x::Optional,a...) =
    Optional{result_type(x)}(map_parser(f,x.parser,a...),
                             x.default,x.transform)


@inline prevind(str,i::Int,parser::Optional,x::Missing) = i
@inline nextind(str,i::Int,parser::Optional,x::Missing) = i


function Base.get(parser::Optional, sequence, till, after, i, state)
    state === missing ? parser.default : get(parser.parser,sequence, till, after, i, state)
end

function _iterate(t::Optional, str, till, i, state)
    if state === nothing
        r = _iterate(t.parser, str, till, i, nothing)
        if r === nothing
            i,missing
        else
            r[1], r[2]
        end
    elseif state === missing
        nothing
    else
        nothing
    end
end


defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type{<:VectorDict}) = VectorDict{keytype(V), valtype(V)}(eltype(V)[])
defaultvalue(V::Type) = missing

function opt(x...; 
             log=false,
             transform_seq=(v,i) -> v, kw...)
    ## @show transform_seq
    if length(x)==1
        els = parser(x[1])
    else
        ## @show x
        els = seq(x...; transform=transform_seq,
                  log=log)
    end
    opt(els; log=log, kw...)
end

opt(x_; kw...) =
    let x=parser(x_)
        opt(result_type(typeof(x)), x; kw...)
    end

function opt(T::Type, x;
             default=defaultvalue(T),
             log=false,
             transform=(v,i) -> v) where { D }
    ##@show default
    x=parser(x)
    RT = promote_type(T,typeof(default))
    Optional{RT}(x, default,
                 log_transform(transform, log))
end





export alt
struct Either{T,Ps,F<:Function} <: AbstractParser{T}
    options::Ps
    transform::F
    Either{T}(p::P, f::F) where {T,P,F<:Function} =
        new{T,P,F}(p::P, f::F)
end
==(x::Either,y::Either) =
    x.options==y.options && x.transform==y.transform
hash(x::Either, h::UInt) = hash(x.options, hash(x.transform,h))

map_parser(f::Function,x::Either,a...) =
    Either{result_type(x)}(
        tuple( ( map_parser(f,p,a...) for p in x.options )...),
        x.transform)


function Base.push!(x::Either, y)
    push!(x.options,y)
    x
end
function Base.pushfirst!(x::Either, y)
    pushfirst!(x.options,y)
    x
end
regex_string(x::Either)  = "(?:" * join([ regex_string(p) for p in x.options],"|") * ")"



mutable struct MutablePair{K,V}
    first::K
    second::V
    MutablePair{K,V}(f,s) where {K,V} =
        new{K,V}(f,s)
end

Base.show(io::IO, x::MutablePair) =
    print(io, x.first, "=>", x.second)
@inline function with_state!(x::MutablePair,s)
    x.second=s
    x
end
@inline function with_key_state!(x::MutablePair,k,s)
    x.first=k
    x.second=s
    x
end

@inline function prevind(str,i::Int,parser::Either,x)
    prevind(str,i,parser.options[x.first],x.second)
end

@inline function nextind(str,i::Int,parser::Either,x)
    nextind(str,i,parser.options[x.first],x.second)
end

function Base.get(parser::Either, sequence, till, after, i, state)
    j = state.first
    lstate = state.second
    parser.transform(
        (get(parser.options[j],sequence, till, after, i, lstate)),
        i)
end



function _iterate(t::Either, str, till, i, state::Union{Nothing,Pair{Int,<:Any},MutablePair{Int,<:Any}})
    s_ = if state !== nothing
        nstate = _iterate(t.options[state.first], str, till, i, state.second)
        nstate !== nothing && return nstate[1], state.first => nstate[2]
        state
    else
        0=>nothing
    end
    fromindex = s_.first+1
    for j in fromindex:length(t.options)
        if (sstate = _iterate(t.options[j], str, till, i, nothing)) !== nothing
            ns = j=>sstate[2]
            return sstate[1], ns
        end
    end
    nothing
end

function alt(x::Vararg{Union{ParserTypes,Pair}})
    parts = Any[ parser(y) for y in x ]
    T = promote_type([ result_type(typeof(x)) for x in parts]...)
    f = (v,i) -> v
    Either{T}(parts, f)
end


function alt(T::Type, x::Vararg; log=false, transform=(v,i) -> v)
    Either{T}(Any[ parser(y) for y in x ], log_transform(transform, log))
end



(|)(x::Any, y::TextParse.AbstractToken) = alt(parser(x),y)
(|)(x::TextParse.AbstractToken, y::Any) = alt(x,parser(y))
(|)(x::TextParse.AbstractToken, y::TextParse.AbstractToken) = alt(x,y)





############################################################


##import Base: findnext



# Base.convert(::Type{Nullable{Pair{Symbol, T}}}, x::Pair{Symbol, Nullable{S}}) where {T,S} =
#     isnull(x.second) ? Nullable{Pair{Symbol, T}}() :
#     Nullable(x.first => convert(T, x.second.value))

struct Greedy{Ps,A,F<:Function} <: AbstractParser{Any}
    pairs::Ps
    alt::A
    transform::F
end
    
export greedy
function greedy(tokens...;
                alt = [],
                transform=(v,i) -> v, log=false)
    Greedy([tokens...], alt, log_transform(transform, log))
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
                return Nullable{T}(_convert(T, tokf.transform(R,i))), i_
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
                   log=false,
                   agg = nothing,
                   kw...)
    T, S = result_type(typeof(x)), result_type(typeof(delim))
    af = if agg === nothing
        if S <: T
            ( r, xmatch, delimmatch ) -> begin
                xmatch !== missing && push!(r,xmatch)
                delimmatch !== missing && push!(r,delimmatch)
            end
        else
            ( r, xmatch, delimmatch ) -> begin
                xmatch !== missing && push!(r,xmatch)
            end 
        end
    else
        agg
    end
    
    function tf(v,i)
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
        r
    end
    
    seq(Vector{T},
        opt(T, x; default=missing),
        rep(seq(delim, x)),
        opt(delim;default=missing);
        log=log,
        ## todo: factor out this transform condition!!
        transform = tf, kw...)
end


export rep_delim
rep_delim(x::TextParse.AbstractToken{T}, delim::TextParse.AbstractToken{S}; kw...) where {T,S} =
    rep_delim(promote_type(S,T), x, delim; kw...)
function rep_delim(
    T::Type, x, delim;
    log=false,repf=rep,
    transform=(v,i) -> v,
    transform_each=(v,i) -> v, kw...)
    x = parser(x)
    delim = parser(delim)
    function t(v,i)
        L = vcat(v...)
        transform(map(p -> transform_each(p,i),  L  ),i)
    end
    seq(Vector{T},
        opt(delim; default=T[], log=log),
        repf(Vector{T},
             seq(x, delim; log=log); log=log,
             transform = (v,i) -> vcat([ [x...] for x in v ]...)),
        opt(x; default=T[], log=log)
        ; log=log,
        ## todo: factor out this transform condition!!
        transform = (t)
        , kw...)
end


export rep_delim_par
function rep_delim_par(x, delim; repf=rep, transform=(v,i) -> v, transform_each=v -> v, kw...)
    x = parser(x)
    delim = parser(delim)
    T = result_type(typeof(x))
    D = result_type(typeof(delim))
    seq(Vector{T},
        opt(Vector{D}, delim; transform = (v,i) -> D[v]),
        repf(seq(T, x, delim; 
                 transform = (v, i) -> v[1]);
             transform=(v,i) -> v),
        opt(Vector{T}, x; transform = (v,i) -> T[v])
        ; 
        ## todo: factor out this transform condition!!
        transform = (v,i)  -> transform(
            map(p -> transform_each(p),
                vcat(v[2:end]...)),i)
        , kw...)
end


export atomic
struct AtomicGroup{P,T} <: WrappedParser{T}
    parser::P
    AtomicGroup(parser) =
        new{typeof(parser),result_type(parser)}(parser)
end
map_parser(f::Function,x::AtomicGroup,a...) =
    AtomicGroup(
        map_parser(f,x.parser,a...))

atomic(x) =
    AtomicGroup(parser(x))

@inline function _iterate(parser::AtomicGroup, sequence, till, i, state)
    if state !== nothing
        nothing
    else
        _iterate(parser.parser, sequence, till, i, state)
    end
end

Base.get(x::AtomicGroup, a...) =
    get(x.parser,a...)

regex_string(x::AtomicGroup) =
    "(?>$(regex_string(x.parser)))"

include("show.jl")
include("deprecated.jl")
include("pcre.jl")
include("re.jl")
include("tokens.jl")
export Parsing2
struct Parsing2{P,S}
    parser::P
    sequence::S
    till::Int
    Parsing2(parser,sequence) =
        new{typeof(parser),typeof(sequence)}(parser,sequence,lastindex(sequence))
end

import Base: parse
"""
parse(p::ParserTypes, s::AbstractString)

parse `s` with parser `p`.
"""
function Base.parse(p::TextParse.AbstractToken, s)
    i = _iterate(p,s)
    i === nothing && return nothing
    get(p,s,lastindex(s),i[1],1,i[2])
end

import Base: iterate
Base.parse(x::Parsing2) =
    get(x,iterate(x)...)
Base.iterate(x::Parsing2) =
    _iterate(x.parser,x.sequence)

    

_iterate(parser,sequence) =
    _iterate(parser, sequence, lastindex(sequence),1,nothing)

export parse_all
function parse_all(parser::ParserTypes, sequence::AbstractString)
    p=Parsing2(parser,sequence)
    R=Any[]
    x=iterate(p)
    while x!==nothing
        push!(R,get(p.parser,p.sequence,p.till,x[1],1,x[2]))
        x = _iterate(p.parser,p.sequence,p.till,x[1],x[2])
    end
    R
end




export _iterate

export Numeric
Numeric = TextParse.Numeric

include("reverse.jl")
include("textparse.jl")

end # module
