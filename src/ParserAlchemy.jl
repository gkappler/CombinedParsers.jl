module ParserAlchemy
import Base: (*), (|), cat, get, prevind, nextind
using Parameters
using Nullables

using TextParse
import Base: ==, hash

using BasePiracy
export AbstractParser
export result_type

include("namedtuples.jl")

@inline _prevind(str,i,parser,x::Nothing) = i
@inline _nextind(str,i,parser,x::Nothing) = i
@inline _prevind(str,i,parser,x) = prevind(str,i,parser,x)
@inline _nextind(str,i,parser,x) = nextind(str,i,parser,x)

ParserTypes = Union{TextParse.AbstractToken, AbstractString, Char, Regex}
export parser
parser(x::ParserTypes) = x



############################################################
## Parsing with TextParse.AbstractToken, operators

state_type(p::Type{<:TextParse.AbstractToken}) = Tuple{Int,result_type(p)}

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
state_type(p::Type{<:AbstractParser}) =  error("implement state_type(::Type{$(p)})")
_iterate(x::AbstractParser,str,i,till,state) =
    error("implement _iterate(x::$(typeof(x)),str::$(typeof(str)),i,till,state::$(typeof(state)))")
function TextParse.tryparsenext(x::AbstractParser,str,i,till,opts=TextParse.default_opts)
    s = _iterate(x,str,i,till,nothing)
    if s === nothing
        Nullable{result_type(x)}(),i
    else
        Nullable(get(x,str,till,s[1],i,s[2])),s[1]
    end
end

"Abstract type for parser wrappers, providing default methods"
abstract type WrappedParser{P,T} <: AbstractParser{T} end
state_type(::Type{<:WrappedParser{P,T}}) where {P,T} = state_type(P)
@inline prevind(str,i::Int,parser::WrappedParser,x) =
    prevind(str,i,parser.parser,x)
@inline nextind(str,i::Int,parser::WrappedParser,x) =
    nextind(str,i,parser.parser,x)
Base.get(x::WrappedParser, a...) =
    get(x.parser,a...)
Base.get(parser::WrappedParser, sequence, till, after, i, state) = 
    get(parser.parser, sequence, till, after, i, state)
_iterate(parser::WrappedParser, sequence, till, i, state) =
    _iterate(parser.parser, sequence, till, i, state)


"wrapper for stepping with ncodeunit length."
struct ConstantParser{N,T} <: WrappedParser{T,T}
    parser::T
end
state_type(p::Type{<:ConstantParser}) = MatchState
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
    if parser.parser == (@inbounds sequence[i])
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
        @inbounds pc=parser.parser[k]
        @inbounds sc=sequence[j]
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
    nc = Base.ncodeunits(@inbounds sequence[i])
    return i+nc, MatchState()
end


"Parsers that do not consume any input can inherit this type"
abstract type LookAround{T} <: NIndexParser{0,T} end

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




 

export Never
"""
wraps a `parser::P`, succeeds if and only if `parser` does not succeed, but consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct Never <: LookAround{Never} end
regex_string(x::Never) = "(*FAIL)"
_iterate(x::Never,str,i,till,state) =
    nothing



export Always
"Parser matching always and not consuming any input"
struct Always <: LookAround{Always}
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
struct PositiveLookahead{T,P} <: LookAround{T}
    parser::P
    PositiveLookahead(p_) =
        let p = parser(p_)
            new{result_type(p),typeof(p)}(p)
        end
end
regex_string(x::PositiveLookahead) =
    "(?="*regex_string(x.parser)*")"
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
struct NegativeLookahead{T,P} <: LookAround{T}
    parser::P
    NegativeLookahead(p_) =
        let p = parser(p_)
            new{result_type(p),typeof(p)}(p)
        end
end
regex_string(x::NegativeLookahead) =
    "(?!"*regex_string(x.parser)*")"
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
struct SideeffectParser{P,T,A} <: WrappedParser{P,T}
    parser::P
    args::A
    effect::Function
    SideeffectParser(f::Function, p::ParserTypes,a...) =
        new{typeof(p),result_type(p),typeof(a)}(p,a,f)
end

export map_parser
map_parser(f::Function,mem::AbstractDict,x::SideeffectParser,a...) =
    SideeffectParser(x.effect,
                     map_parser(f,mem,x.parser,a...),
                     x.args...)

function log_effect(s,start,after,state,log,delta=5)
    if state === nothing
        printstyled("no match ",bold=true,color=:underline)
    else
        printstyled("   match ";bold=true,color=:green)
    end
    print(log)
    if prevind(s,start)<start
        printstyled(s[max(1,start-delta):(prevind(s,start))])
        printstyled(s[start:prevind(s,after)];bold=true,color=:green)
    end
    if state === nothing 
        printstyled(s[after:min(end,after+delta)],bold=true,color=:underline)
    else
        printstyled(s[after:min(end,after+delta)],color=:yellow)
    end
    println()
end

function log_effect_match(s,start,after,state,log,delta=5)
    if state!==nothing
        log_effect(s,start,after,state,log,delta)
    end
end

with_log(s::AbstractString,p_, delta=5;nomatch=false) =
    let p = parser(p_), log=s*": "
        SideeffectParser(nomatch ? log_effect : log_effect_match ,p, log, delta)
        ##with_log(p_; log=s*": ",delta=5)    
        #with_log(p_;log="", delta=5,nomatch=false) =
    end

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
export NamedParser, with_name
struct NamedParser{P,T} <: WrappedParser{P,T}
    name::Symbol
    parser::P
    NamedParser(name::Symbol,p_) =
        let p=parser(p_)
            new{typeof(p),result_type(p)}(name,p)
        end
end


parser(x::Pair{Symbol, P}) where P =
    NamedParser(x.first, parser(x.second))
with_name(name::Symbol,x; doc="") = 
    NamedParser(name,x)

map_parser(f::Function,x::NamedToken,a...) =
    NamedToken(x.name,map_parser(f,x.parser,a...))


export Transformation, instance
struct Transformation{P,T, F<:Function} <: WrappedParser{P,T}
    transform::F
    parser::P
    Transformation{T}(transform::Function, p_) where {T} =
        let p = parser(p_)
            new{typeof(p),T,typeof(transform)}(transform, p)
        end
end
map_parser(f::Function,x::InstanceParser,a...) =
    InstanceParser{result_type(x)}(x.transform,map_parser(f,x.parser,a...))
parser(constant::Pair{<:ParserTypes}) =
    Transformation{typeof(constant.second)}(
        (v,i) -> constant.second,
        parser(constant.first))

regex_string(x::Union{NamedParser, Transformation}) = regex_string(x.parser)

function Base.get(parser::Transformation, sequence, till, after, i, state)
    parser.transform(
        get(parser.parser,sequence, till, after, i, state)
        ,i)
end

function _iterate(parser::Transformation, sequence, till, i, state)
    r = _iterate(parser.parser, sequence, till, i, state )
end

function infer_result_type(f,Tc,p,onerror,ts...)
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

export instance,instance_at 
function instance(Tc::Type, p, a...)
    Transformation{Tc}((v,i) -> Tc(a..., v), p)
end
function instance(Tc::Type, p)
    Transformation{Tc}((v,i) -> _convert(Tc,v), p)
end
function instance(f::Function, Tc::Type, p, a...)
    T = infer_result_type(f,Tc,p,"call seq(function,type,parts...)",typeof.(a)...)
    Transformation{Tc}((v,i) -> (f(v, a...)), p)
end
function instance_at(f::Function, Tc::Type, p, a...)
    T = infer_result_type(f,Tc,p,"call seq(function,type,parts...)",Int,typeof.(a)...)
    Transformation{Tc}((v,i) -> (f((v), i, a...)), p)
end
function instance(f::Function, p_, a...)
    p=parser(p_)
    T = infer_result_type(f,Any,p,"call seq(function,type,parts...)",typeof.(a)...)
    Transformation{T}((v,i) -> (f(v, a...)), p)
end
function instance_at(f::Function, p, a...)
    T = infer_result_type(f,Any,p,"call seq(function,type,parts...)",Int,typeof.(a)...)
    Transformation{T}((v,i) -> (f(v, i, a...)), p)
end
@deprecate instance(::Type{T}, f::Function, p::P, a...) where {T, P<:ParserTypes} instance(f,T,p,a...)



import Base: in
include("unicode.jl")

export CharIn
struct CharIn{S} <: NIndexParser{1,Char}
    sets::S
    CharIn(x) = new{typeof(x)}(x)
end
CharIn(x::CharIn) = CharIn(x.sets)
CharIn(x::CharIn{Tuple{<:CharIn}}) = CharIn(x.sets[1])
==(x::CharIn,y::CharIn) =
    x.sets==y.sets
hash(x::CharIn, h::UInt) = hash(x.sets,h)


CharIn(x::String) = CharIn( tuple(Char[ c for c in x]) )
function Base.in(x::Char, set::CharIn{<:Tuple})
    for s in set.sets
        x in s && return true
    end
    return false
end
result_type(::Type{<:CharIn}) = Char
regex_string_(x::Union{Vector,Set}) = join(regex_string_.(x))
regex_string_(x::Char) = x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string(x::Char) = x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string_(x::StepRange) =
    if x.start == x.stop
        x.start
    else
        x.start*"-"*x.stop
    end
regex_string_(x::Tuple) = join([regex_string_(s) for s in x])
regex_string_(x::CharIn) = regex_string_(x.sets)
regex_string(x::CharIn) =
    "["*regex_string_(x)*"]"

regex_string(x::CharIn{Tuple{Char}}) =
    "$(x.sets[1])"




@inline function _iterate(parser::CharIn, sequence, till, i, state::Nothing)
    i>till && return(nothing)
    @inbounds c = sequence[i]
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
    @inbounds c = sequence[i]
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
    rep(seq(2,NegativeLookahead(parser(stop)),parser(p)))

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


@inline nextind(str,i::Int,parser::FlatMap,x::Tuple) =
    let li = nextind(str,i,parser.left,x[1])
        nextind(str,li,x[2],x[3])
    end

@inline prevind(str,i::Int,parser::FlatMap,x::Tuple) =
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

state_type(p::Type{<:FlatMap{T,P}}) where {T,P} = Tuple{state_type(P),<:Any,<:Any}
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
struct Sequence{T,P<:Tuple} <: AbstractParser{T}
    parts::P
    function Sequence{T}(p) where T
        new{T,typeof(p)}(p)
    end
    function Sequence(p)
        parts = tuple( ( parser(x) for x = p )... )
        T = ( result_type(typeof(x)) for x in parts )
        new{Tuple{T...},typeof(parts)}(parts)
    end
end
parser_types(::Type{Sequence{T, P}}) where {T, P} =
    P
map_parser(f::Function,x::Sequence,a...) =
    Sequence{result_type(x)}(tuple( (map_parser(f,p,a...)
                                     for p in x.parts)... ))


seq(transform::Function, T::Type, a...; kw...) =
    instance(transform, T, Sequence(a))

seq(transform::Function, a...; kw...) =
    instance(transform, Sequence(a))


function seq(tokens_::Vararg{Union{Pair{Symbol,<:ParserTypes},ParserTypes}})
    s = Sequence( ( t isa Pair{Symbol,<:ParserTypes} ? NamedParser(t.first,t.second) : t for t in tokens_ ))
    names = [ t.first=>i
              for (i,t) in enumerate(tokens_)
              if t isa Pair{Symbol,<:ParserTypes} ]
    T = result_type(s)
    NT= NamedTuple{ tuple( (n.first for n in names)...),
                    Tuple{ (fieldtype(T,n.second) for n in names)... }}
    NTn = NamedTuple{ tuple( (n.first for n in names)...) }
    function transform(v,i)
        ##@show v
        NT( tuple( (v[k.second] for k in names )... ))
    end
    instance_at(transform, NT, s)
end

function seq(tokens::Vararg{ParserTypes};
             transform=nothing, kw...)
    s = Sequence(tokens)
    if transform isa Integer
        seq(transform,tokens...)
    elseif transform===nothing
        s
    elseif transform isa Function
        instance(transform, s)
    end
end

macro select_instance_index(transform)
    quote
        function self(v)
            v[$(transform)]
        end
    end |>esc
end

seq(transform::Integer,tokens::Vararg{ParserTypes}) =
    seq(Val{transform}(),tokens...)

function seq(::Val{transform},tokens::Vararg{ParserTypes}) where {transform}
    s = Sequence(tokens)
    instance(v -> v[transform], fieldtype(result_type(s),transform), s)
end


function seq(T::Type, tokens::Vararg;
             log=false,
             transform=:instance)
    parts = tuple( ( parser(x) for x = tokens )... )
    if T==NamedTuple
        fnames = tuple( [ x.name for x in parts if x isa NamedParser ]... )
        ftypes = [ result_type(typeof(x.parser)) for x in parts if x isa NamedParser ]
        RT = isempty(fnames) ? T : NamedTuple{fnames, Tuple{ftypes...}}
    else
        RT = T
    end
    if transform == :instance
        transform = (v,i) -> construct(RT,Any[ remove_null(x) for x in v ])
    end
    tr = log_transform(transform, log)
    instance(tr,RT,Sequence(parts))
end



@inline function prevind(str,i::Int,parser::Sequence,x::MatchState)
    for p in lastindex(x):-1:1
        i=prevind(str,i,parser.parts[p],MatchState())
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


state_type(p::Type{<:Sequence}) = Vector{Any}
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

    ## error()
@generated function _iterate_(parser::Sequence, sequence, till, i, states)
    pts = parser_types(parser)
    subsearch = Symbol[ gensym(:subsearch) for i in fieldtypes(pts) ]
    subresult = Symbol[ gensym(:r) for i in fieldtypes(pts) ]
    substate = Symbol[ gensym(:s) for i in fieldtypes(pts) ]
    init = if states===Nothing
        [
        quote
        $(substate[i]) = nothing
        end
        for (i,t) in enumerate(fieldtypes(pts))
        ]
    elseif states===MatchState
        [
        quote
        $(substate[i]) = MatchState()
        end
        for (i,t) in enumerate(fieldtypes(pts))
        ]
    else
        [
        quote
        $(substate[i]) = states[$i]
        end
        for (i,t) in enumerate(fieldtypes(pts))
        ]
    end
        
    parseparts = [
        quote
        @label $(subsearch[i])
        @inbounds before_i = start_index(sequence, i_, parts[$i], $(substate[i]))
        $(subresult[i]) = @inbounds _iterate(parts[$i], sequence, till, i_, $(substate[i]))
        if $(subresult[i]) === nothing
        i_ = before_i
        prune_captures(sequence,i_)
        @goto $(i == 1 ? :theend : subsearch[i-1])
        else
        i_, $(substate[i]) = $(subresult[i])
        end
        end
        for (i,t) in enumerate(fieldtypes(pts))
    ]
    R = quote
        i_ = i
        parts=parser.parts
        $(init...)
        states !== nothing && @goto $(subsearch[end])
        $(parseparts...)
        
        R = if (&)($(( :(($(s) isa MatchState)) for s in substate )...) )
            MatchState()
        else
            tuple( $([ :(($(s))) for s in substate ]...) )
        end
        return i_, R
        @label theend
        return nothing
    end
    ##Core.println( R )
    R
end









(*)(x::Any, y::TextParse.AbstractToken) = seq(parser(x),y)
(*)(x::TextParse.AbstractToken, y::Any) = seq(x,parser(y))
(*)(x::TextParse.AbstractToken, y::TextParse.AbstractToken) = seq(x,y)

regex_string(x::Sequence)  = join([ regex_string(p) for p in x.parts])

export lazy
struct Lazy{P,T} <: WrappedParser{P,T}
    parser::P
end
lazy(p_) =
    let p = parser(p_)
        Lazy{typeof(p),result_type(p)}(p)
    end

map_parser(f::Function,mem::AbstractDict,x::Lazy,a...) =
    get!(mem,x) do
        lazy(map_parser(f,mem,x.parser,a...))
    end

regex_string(x::Lazy) =
    regex_string(x.parser)*"?"


export rep, rep1
struct Repeat{P,T} <: WrappedParser{P,T}
    range::UnitRange{Int}
    parser::P
    Repeat(range::UnitRange{Int},p) =
        let p_=parser(p)
            new{typeof(p_),Vector{result_type(p_)}}(range,p_)
        end
end
Repeat(parser) =
    Repeat((0:typemax(Int)),parser)
Repeat(min::Integer,max::Integer,parser) =
    Repeat((min:max),parser)
Repeat(min::Integer,parser) =
    Repeat((min:typemax(Int)),parser)


rep1(x) =
    Repeat(1,x)
rep1(T::Type, x;  log=false, transform) =
    rep1(log_transform(transform, log),T, x)
rep1(transform::Function, T::Type, x) =
    instance(transform, T, Repeat(1,typemax(Int),parser(x)))


rep1(T::Type, x,y::Vararg; log=false, transform, kw...) =
    rep1(T, seq(x,y...; transform=log_transform(transform, log), kw...), transform=(v,i) -> v)


rep(x::ParserTypes, minmax::Tuple{<:Integer,<:Integer}=(0,typemax(Int))) =
    Repeat(minmax...,x)

rep(minmax::Tuple{<:Integer,<:Integer},x::ParserTypes,y::Vararg{ParserTypes}) =
    rep(seq(x,y...),minmax)

rep(x::ParserTypes,y::Vararg{ParserTypes}) =
    rep(seq(x,y...) )

rep(transform::Function, a...) =
    instance(transform, rep(a...))

rep(transform::Function, T::Type, a...) =
    instance(transform, T, rep(a...))

rep(transform::Function, minmax::Tuple{<:Integer,<:Integer}, a...) =
    instance(transform, rep(minmax, a...))

rep(T::Type, x, minmax::Tuple{<:Integer,<:Integer}=(0,typemax(Int)); transform) =
    instance(transform,T,
             Repeat(minmax...,parser(x)))



map_parser(f::Function,x::Repeat,a...) =
    Repeat(x.range,
           map_parser(f,x.parser,a...))
                           


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


function fill_rep(t::Repeat, sequence, till, j,state_::S) where S
    j_ = -1
    while state_length(t,state_) < t.range.stop && ( x = _iterate(t.parser,sequence, till,j,nothing) )!==nothing
        ##@info "rep fill..." x state_
        ## e.g. match(re"(?:a|(?=b)|.)*\z","abc")
        j_==x[1] && state_length(t,state_)>t.range.start && break
        state_=pushstate!(state_,t.parser, x[2])
        j_=j
        j = x[1]
    end
    j,state_,state_length(t,state_) < t.range.start
end


function _iterate(t::Repeat, sequence, till, i, state)
    i_,state_,goback = if state === nothing
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
        return i_, state_
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











export opt
struct None end
struct Optional{P,T} <: WrappedParser{P,T}
    parser::P
    default::T
    function Optional(parser,default)
        T = result_type(parser)
        D = typeof(default)
        T_ = promote_type(T,D)
        T_ === Any && ( T_ = Union{T,D} )
        new{typeof(parser),T_}(parser, default)
    end
end
state_type(::Type{<:Optional{P}}) where P = Union{None,state_type(P)}##Tuple{Int, promote_type( state_type.(t.options)...) }

opt(x;kw...) = opt(parser(x);kw...)

opt(x1,x...;kw...) = opt(seq(x1,x...);kw...)

opt(x::Union{TextParse.AbstractToken,Regex};default=defaultvalue(result_type(x))) =
    Optional(x,default)

opt(T::Type, x_; transform, kw...) =
    opt(transform, T, x; kw...)

function opt(transform::Function, T::Type, x;
             default=defaultvalue(T))
    instance(transform,T,Optional(x, default))
end


regex_string(x::Optional)  = regex_string(x.parser)*"?"
map_parser(f::Function,x::Optional,a...) =
    Optional(map_parser(f,x.parser,a...),
             x.default)


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
defaultvalue(V::Type{<:VectorDict}) = VectorDict{keytype(V), valtype(V)}(eltype(V)[])
defaultvalue(V::Type) = missing






export alt
struct Either{T,Ps} <: AbstractParser{T}
    options::Ps
    Either{T}(p::P) where {T,P} =
        new{T,P}(p::P)
end
==(x::Either,y::Either) =
    x.options==y.options
hash(x::Either, h::UInt) = hash(x.options)
state_type(::Type{<:Either}) = Pair{Int,<:Any}##Tuple{Int, promote_type( state_type.(t.options)...) }

function alt(x::Union{ParserTypes,Pair}...)
    parts = Any[ parser(y) for y in x ]
    Ts = ( result_type(typeof(x)) for x in parts )
    T = promote_type(Ts...)
    Any <: T && ( T = Union{Ts...} )
    Either{T}(parts)
end


function alt(T::Type, x::Vararg; transform::Function)
    instance(transform, T, alt(x...))
end

map_parser(f::Function,x::Either,a...) =
    Either{result_type(x)}(
        tuple( ( map_parser(f,p,a...) for p in x.options )...))


function Base.push!(x::Either, y)
    @assert result_type(y) <: result_type(x)
    push!(x.options,y)
    x
end
function Base.pushfirst!(x::Either, y)
    @assert result_type(y) <: result_type(x)
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
    ##s isa Tuple{Int,Nothing} && error()
    x.second=s
    x
end
@inline function with_key_state!(x::MutablePair,k,s)
    ##s isa Tuple{Int,Nothing} && error()
    x.first=k
    x.second=s
    x
end

@inline function prevind(str,i::Int,parser::Either,x)
    ## @show i
    prevind(str,i,parser.options[x.first],x.second)
end

@inline function nextind(str,i::Int,parser::Either,x)
    ## @show i
    nextind(str,i,parser.options[x.first],x.second)
end

function Base.get(parser::Either, sequence, till, after, i, state)
    j = state.first
    lstate = state.second
    get(parser.options[j],sequence, till, after, i, lstate)
end



function _iterate(t::Either, str, till, i, state::Union{Nothing,Pair{Int,<:Any},MutablePair{Int,<:Any}})
    ##sleep(1)
    i, s_ = if state !== nothing
        before_i = _prevind(str,i,t.options[state.first],state.second) ##state[end][1]
        nstate = _iterate(t.options[state.first], str, till, i, state.second)
        nstate !== nothing && return nstate[1], state.first => nstate[2]
        #with_state!(state, nstate[2])
        prune_captures(str,before_i)
        before_i, state
    else
        i, 0=>nothing
        ##MutablePair{Int,Any}(0,nothing)
    end
    fromindex = s_.first+1
    ##sstate = nothing
    for j in fromindex:length(t.options)
        ## @info "alt" j str[i:till] typeof(t.options[j]) #t.options[j] 
        if (sstate = _iterate(t.options[j], str, till, i, nothing)) !== nothing
            ## i_::Int, nstate_ = sstate
            ns = j=>sstate[2]
            ## with_key_state!(s_,j,sstate[2])
            return sstate[1], ns
        end
    end
    nothing
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
    
    function tf(v,i)::Vector{T}
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

    instance_at(tf, Vector{T},
             seq(
                 opt(x; default=missing),
                 rep(seq(delim, x)),
                 opt(delim;default=missing)))
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
struct AtomicGroup{P,T} <: WrappedParser{P,T}
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

map_parser(f::Function,x::Numeric,a...) = x

import AbstractTrees: print_tree, children, printnode
include("reverse.jl")
include("textparse.jl")
include("deprecated.jl")
include("re.jl")
include("pcre.jl")
include("tokens.jl")

include("show.jl")

end # module
