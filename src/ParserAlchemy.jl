module ParserAlchemy

using Parameters
using Nullables

using TextParse
import TextParse: tryparsenext
using BasePiracy
import Base: ==, hash

export tryparsenext, tokenize, result_type

export trimstring
trimstring(x::AbstractString) =
    replace(x, r"^[ \r\n\t]*|[ \r\n\t]*$" => s"")

include("namedtuples.jl")

export parser
parser(x::TextParse.AbstractToken) = x
revert(x::TextParse.AbstractToken) = error("implement!")
parser(v::Vector) = [ parser(x) for x in v ]

struct Suffix{S} s::S end
parser(x::AbstractString) = x
revert(x::AbstractString) = Suffix(x)
regex_flags(x) = replace(string(x), r"^.*\"([^\"]*)$"s => s"\1")
parser(x::Regex) = Regex("^" * regex_string(x), regex_flags(x))
revert(x::Regex) = Regex(regex_string(x) * '$', regex_flags(x))
parser(x::Pair{Symbol, P}) where P =
    NamedToken{P,result_type(P)}(x.first, parser(x.second))

parser(t::Tuple) = tuple([ parser(x) for x in t ]...)
parser(x::Pair{Symbol, Tuple{P, Type}}) where P =
    NamedToken{P,x.second[2]}(x.first, x.second[1])


result_type(x::Type{<:TextParse.AbstractToken{T}}) where T = T
result_type(x::Type{Pair{Symbol, <:T}}) where T =
    Pair{Symbol, result_type(T)}
result_type(x::Type{<:AbstractString}) = AbstractString
result_type(x::Type{Regex}) = AbstractString

############################################################
## Parsing with TextParse.AbstractToken, operators

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
    res = replace(string(s), r"([()[\]{}?*+\-|^\$\\.&~#\s=!<>|:])" => s"\\\1")
    replace(res, "\0" => "\\0")
end


export regex_string
regex_string(x::AbstractString) = regex_escape(x)
regex_string(::TextParse.Numeric{<:Integer}) = "[[:digit:]]+"
function regex_string(x::Regex)
    p=x.pattern
    if p[1]=='^'
        p=p[2:end]
    end
    if p[end]=='$'
        p=p[1:end-1]
    end
    p
end



export regex_tempered_greedy, regex_neg_lookahead
# https://www.rexegg.com/regex-quantifiers.html#tempered_greed
regex_tempered_greedy(s,e, flags="s"; withend=true) =
    Regex("^"*regex_string(s)*"((?:(?!"*regex_string(e)*").)*)"*
          ( withend ? regex_string(e) : ""),flags)

# https://www.rexegg.com/regex-quantifiers.html#tempered_greed
regex_neg_lookahead(e, match=r".") =
    instance(String,
             (v,i) -> v[1],
             Regex("^((?:(?!"*regex_string(e)*")"*regex_string(match)*")*)","s"))


ParserTypes = Union{TextParse.AbstractToken, AbstractString, Regex,
                    Pair{Symbol,
                         <:Union{TextParse.AbstractToken, AbstractString, Regex}}}
result_type(x::T) where {T<:ParserTypes} =
    result_type(T)


export tokenize
tokenize(x, str::RegexMatch) = tokenize(x, str.match)


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
    println(io, "incomplete parsing at $(x.index):")
    println(io, "\"$(context(x))\"")
    println(io, "in \"$(x.str)\"\n")
    println(io, x.pattern)
end

"""
tokenize(x, str; delta=200, errorfile=nothing)

Tokenize string or iterator `str` with parser `x`.
"""
function tokenize(x, str; partial=:error)
    i=firstindex(str)
    till=lastindex(str)
    r, i_ = tryparsenext(x, str, i, till, TextParse.default_opts)
    if i_<=till
        if partial isa AbstractString ## remove?
            make_org(s) = replace(s, r"^\*"m => " *")
            open(partial, "a") do io
                println(io, "* incomplete parsing stopped at $i_ ")
                println(io, "error at")
                println(io, make_org(str[min(i_,end):min(end, nextind(str,i_,200))]))
                println(io, "** data")
                println(io, make_org(str))
            end
        elseif partial == :warn
            @warn "incomplete parsing stopped at $i_ " str[min(i_,end):min(end, nextind(str,i_,200))]
        elseif partial == :error
            throw(PartialMatchException(i_, str, x))
        elseif partial == :nothing
            return nothing
        end
    end
    if isnull(r)
        if partial == :error
            error("no match")
        elseif partial == :warn
            @warn "no match"
        else
            nothing
        end
    else
        get(r)
    end
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

import Base: Regex
function Regex(x::ParserTypes) 
    Regex("^"*regex_string(x))
end



function TextParse.tryparsenext(tok::AbstractString, str::AbstractString, i, till, opts=TextParse.default_opts)
    if startswith(str[i:end], tok)
        e = nextind(str, i, lastindex(tok))
        Nullable(tok), e
    else
        Nullable{String}(), i
    end
end


function TextParse.tryparsenext(tok::Regex, str, i, till, opts=TextParse.default_opts)
    m = match(tok, SubString(str,i,till)) ## idx not working with ^, and without there is no option to force it at begin
    if m === nothing
        Nullable{AbstractString}(), i
    else
        ni = m.match =="" ? i : nextind(str, i, length(m.match))
        ##@show str[i:min(end,ni)] m str[min(end,ni):end]
        ( Nullable(isempty(m.captures) ? m.match : m.captures)
          , ni
          )
    end
end

export ParserPeek
struct ParserPeek{T,P} <: TextParse.AbstractToken{T}
    message::String
    length::Int
    parser::P
    ParserPeek(message,length, p::P) where P =
        new{result_type(P),P}(message, length, p)
end

function TextParse.tryparsenext(tok::ParserPeek, str, i, till, opts=TextParse.default_opts) where {P,T}
    r, i_ = tryparsenext(tok.parser, str,i,till,opts)
    if i<=till
        if isnull(r)
            l = tok.length
            at = str[min(lastindex(str), i):min(lastindex(str), nextind(str,i,l))]
            @info "no match" at msg=tok.message
        else
            at = str[i:min(lastindex(str), prevind(str,i_,1))]
            @info "match" at msg=tok.message get(r)
        end
    end
    r, i_
end


export NamedToken
struct NamedToken{P,T} <: TextParse.AbstractToken{Pair{Symbol,T}}
    name::Symbol
    parser::P
end

function TextParse.tryparsenext(tok::NamedToken{P,T}, str, i, till, opts=TextParse.default_opts) where {P,T}
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    if isnull(result)
        Nullable{Pair{Symbol,T}}(), i
    else
        Nullable(tok.name => get(result)), i_
    end
end


export InstanceParser, instance
struct InstanceParser{P,T, F<:Function} <: TextParse.AbstractToken{T}
    transform::F
    parser::P
end
InstanceParser{T}(transform::F, p::P) where {T, F<:Function,P} = InstanceParser{P,T,F}(transform, p)
regex_string(x::Union{NamedToken, InstanceParser}) = regex_string(x.parser)

export instance 

function instance(::Type{T}, p::P, a...) where {T, P<:ParserTypes}
    InstanceParser{T}((v,i) -> T(a..., v), p)
end
function instance(::Type{T}, p::P) where {T, P<:ParserTypes}
    InstanceParser{T}((v,i) -> _convert(T,v), p)
end
function instance(::Type{T}, f::Function, p::P, a...) where {T, P<:ParserTypes}
    InstanceParser{T}((v,i) -> _convert(T,f((v), i, a...)), p)
end


function TextParse.tryparsenext(tok::InstanceParser{P,T}, str, i, till, opts=TextParse.default_opts) where {P,T}
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    if isnull(result)
        Nullable{T}(), i
    else
        ## cz@show tok (result)
        Nullable(
            tok.transform(get(result), i)), i_
    end
end




export Filter
"""
wraps a `parser::P`, succeeds if `parser` does succeed and a predicate function returns true on the match, otherwise fails.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct Filter{T,P,F<:Function} <: TextParse.AbstractToken{T}
    parser::P
    filter::F
end
Filter(f::Function,p::P) where P =
    Filter{result_type(P),P,typeof(f)}(p,f)
result_type(p::Type{Filter{T}}) where T = T

function TextParse.tryparsenext(tok::Filter, str, i, till, opts=TextParse.default_opts)
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    if isnull(result)
        result, i
    elseif tok.filter(get(result))
        result, i_
    else
        Nullable{result_type(typeof(tok))}(), i
    end
end

export FullText
struct FullText <: TextParse.AbstractToken{AbstractString}
end
TextParse.tryparsenext(tok::FullText, str, i, till, opts=TextParse.default_opts) = 
    Nullable(str[i:till]), till+1



export PositiveLookahead
"""
wraps a `parser::P`, succeeds if and only if `parser` succeeds, but consumes no input.
The match is returned.
Useful for checks like "must be followed by `parser`, but don't consume its match".
"""
struct PositiveLookahead{T,P} <: TextParse.AbstractToken{T}
    parser::P
end
PositiveLookahead(p::P) where P = PositiveLookahead{result_type(P),P}(p)
result_type(p::Type{PositiveLookahead{T}}) where T = T

function TextParse.tryparsenext(tok::PositiveLookahead, str, i, till, opts=TextParse.default_opts)
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    result, i
end

export Never
"""
wraps a `parser::P`, succeeds if and only if `parser` does not succeed, but consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct Never <: TextParse.AbstractToken{Nothing}
end

TextParse.tryparsenext(tok::Never, str, i, till, opts=TextParse.default_opts) =
    Nullable{Nothing}(), i

export Always
"""
wraps a `parser::P`, succeeds if and only if `parser` does not succeed, but consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct Always <: TextParse.AbstractToken{Nothing}
end

TextParse.tryparsenext(tok::Always, str, i, till, opts=TextParse.default_opts) =
    Nullable(nothing), i



export NegativeLookahead
"""
wraps a `parser::P`, succeeds if and only if `parser` does not succeed, but consumes no input.
`nothing` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".
"""
struct NegativeLookahead{P} <: TextParse.AbstractToken{Nothing}
    parser::P
end

export rep_stop, rep_until
rep_stop(p,stop) =
    rep(seq(NegativeLookahead(stop),p; transform=2))
rep_until(p,until, with_until=false) =
    seq(rep_stop(p,until), until;
        transform = with_until ? nothing : 1)

function TextParse.tryparsenext(tok::NegativeLookahead, str, i, till, opts=TextParse.default_opts)
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    if isnull(result)
        ## @info "match at" str[i:till]
        Nullable(nothing), i
    else
        Nullable{Nothing}(), i
    end
end


export FlatMap
struct FlatMap{T,P,Q<:Function} <: TextParse.AbstractToken{T}
    left::P
    right::Q
    function FlatMap{T}(left::P, right::Q) where {T, P, Q<:Function}
        new{T,P,Q}(left, right)
    end
end

regex_string(x::FlatMap)  = error("regex determined at runtime!")
function TextParse.tryparsenext(tokf::FlatMap, str, i, till, opts=TextParse.default_opts)
    T = result_type(tokf)
    lr, i_ = tryparsenext(tokf.left, str, i, till, opts)
    if !isnull(lr)
        rightp = tokf.right(get(lr))
        !( result_type(rightp) <: T ) && error("$(result_type(rightp)) <: $T")
        rr, i__ = tryparsenext(rightp, str, i_, till, opts)
        if !isnull(rr)
            return rr, i__
        end
    end
    return Nullable{T}(), i
end


export seq
struct Sequence{T,P<:Tuple,F<:Function} <: TextParse.AbstractToken{T}
    parts::P
    transform::F
    function Sequence{T}(p::P, f::F) where {T, P<:Tuple,F<:Function}
        new{T,P,F}(p, f)
    end
end
parser_types(::Type{Sequence{T, P, F}}) where {T, P, F} =
    P

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





import Base: (*), (|), cat
(*)(x::Regex, y::Regex) =
    Regex(x.pattern * y.pattern)
(*)(x::String, y::Regex) =
    Regex(regex_escape(x) * y.pattern)
(*)(x::Regex, y::String) =
    Regex(x.pattern * regex_escape(y))

(*)(x::Any, y::TextParse.AbstractToken) = seq(parser(x),y)
(*)(x::TextParse.AbstractToken, y::Any) = seq(x,parser(y))
(*)(x::TextParse.AbstractToken, y::TextParse.AbstractToken) = seq(x,y)

regex_string(x::Sequence)  = join([ regex_string(p) for p in x.parts])
@generated function TextParse.tryparsenext(tokf::Sequence, str, i, till, opts=TextParse.default_opts)
    pts = parser_types(tokf)
    ## Core.println(pts)
    subresult = Symbol[ gensym(:r) for i in fieldtypes(pts) ]
    parseparts = [
        quote
        $(subresult[i]), i_ = tryparsenext(parts[$i], str, i_, till, opts)
        if isnull($(subresult[i]))
        return Nullable{T}(), i
        end
        end
        for (i,t) in enumerate(fieldtypes(pts))
    ]
    ## Core.println( parseparts )
    quote
        T = result_type(tokf)
        i_ = i
        parts=tokf.parts
        $(parseparts...)
        R = tokf.transform(tuple( $([ :(($(s)).value) for s in subresult ]...) ), i)
        ( Nullable(_convert(T, R)), i_)
    end
end




export rep, rep1
struct Repeat{T,P,F<:Function} <: TextParse.AbstractToken{T}
    range::Tuple{Int,Int}
    parser::P
    transform::F
end
Repeat{T}(parser::P, transform::F) where {T,P,F<:Function} =
    Repeat{T,P,F}((0,typemax(Int)),parser, transform)
Repeat{T}(min::Integer,max::Integer,parser::P, transform::F) where {T,P,F<:Function} =
    Repeat{T,P,F}((min,max),parser, transform)

regex_operator(x::Repeat) =
    if x.range[1] == 0
        if x.range[2] == typemax(Int)
            "*"
        else            
            "{,$(x.range[2])}"
        end
    else
        if x.range[2] == typemax(Int)
            if x.range[1] == 1
                "+"
            else
                "{1,$(x.range[2])}"
            end
        else
            "{$(x.range[1]),$(x.range[2])}"
        end
    end

function regex_string(x::Repeat)
    r = regex_string(x.parser)
    op = regex_operator(x)
    "(?:$r)$op"
end


rep1(x::ParserTypes;  kw...) where T =
    rep1(Vector{result_type(typeof(x))},x; kw...)
rep1(T::Type, x;  log=false, transform=(v,i) -> v) =
    Repeat{T}(1,typemax(Int),parser(x), log_transform(transform, log))


rep1(T::Type, x,y::Vararg; log=false, transform=(v,i) -> v, kw...) =
    rep1(T, seq(x,y...; transform=log_transform(transform, log), kw...), transform=(v,i) -> v)

rep(x::T; kw...) where T =
    rep(Vector{result_type(T)},x; kw...)
    
rep(x::TextParse.AbstractToken{T};  kw...) where T =
    rep(Vector{T},x; kw...)

rep(T::Type, x;  log=false, transform=(v,i) -> v ) = 
    Repeat{T}(parser(x), log_transform(transform, log))

rep(x::Regex) =
    Regex(regex_string(rep(x; log=false)))

rep(T::Type, x,y::Vararg; log=false, transform=(v,i) -> v, kw...) =
    rep(T, seq(x,y...; transform=log_transform(transform, log), kw...); transform=(v,i) -> v)


function TextParse.tryparsenext(t::Repeat, str, i, till, opts=TextParse.default_opts)
    T = result_type(t)
    hist = Any[]
    i_=i
    repval, i__ = tryparsenext(t.parser, str, i_, till)
    while !isnull(repval) && i_ != i__ && length(hist)<=t.range[2]
        push!(hist, repval.value)
        i_=i__
        repval, i__ = tryparsenext(t.parser, str, i_, till)
    end
    if length(hist)<t.range[1]
        Nullable{T}(), i
    else
        ( Nullable(_convert(T,t.transform(hist,i_))), i_)
    end
end


export opt
struct Optional{T,P,F<:Function} <: TextParse.AbstractToken{T}
    parser::P
    default::T
    transform::F
    function Optional{T}(parser::P,default::D,f::F) where {T,P,D,F<:Function}
        T_ = promote_type(T,D)
        T_ === Any && ( T_ = Union{T,D} )
        new{T_,P,F}(parser, default,f)
    end
end
defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type{<:VectorDict}) = VectorDict{keytype(V), valtype(V)}(eltype(V)[])
defaultvalue(V::Type) = missing
Optional{T}(parser::P,f::F) where {T,P,F<:Function} =
    Optional{T}(parser, defaultvalue(T), f)
regex_string(x::Optional) where {T, E}  = "(?:"*regex_string(x.parser)*")?"

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

opt(x; kw...) =
    opt(result_type(typeof(x)), x; kw...)

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


function TextParse.tryparsenext(t::Optional, str, i, till, opts=TextParse.default_opts)
    T = result_type(t)
    r, i_ = tryparsenext(t.parser, str, i, till)
    if !isnull(r)
        try
            r_ = _convert(T,t.transform(r.value, i))
            return Nullable(r_), i_
        catch e
            @error "error transforming" e t
            rethrow(e)
        end
    end
    ## @show default(t.els)
    r = t.default
    Nullable{T}(r), i ## todo: t.default
end



export alt
struct Either{T,Ps,F<:Function} <: TextParse.AbstractToken{T}
    options::Ps
    transform::F
    Either{T}(p::P, f::F) where {T,P,F<:Function} =
        new{T,P,F}(p::P, f::F)
end
==(x::Either,y::Either) =
    x.options==y.options && x.transform==y.transform
hash(x::Either, h::UInt) = hash(x.options, hash(x.transform,h))

function Base.push!(x::Either, y)
    push!(x.options,y)
    x
end
function Base.pushfirst!(x::Either, y)
    pushfirst!(x.options,y)
    x
end
regex_string(x::Either)  = "(?:" * join([ regex_string(p) for p in x.options],"|") * ")"

function alt(x::Vararg{ParserTypes})
    parts = Any[ parser(y) for y in x ]
    T = promote_type([ result_type(typeof(x)) for x in parts]...)
    f = (v,i) -> v
    Either{T}(parts, f)
end

function alt(x::Vararg{Union{String,Regex}})
    T = AbstractString
    instance(T, (v,i) -> v, Regex("^(?:" * join([regex_string(p) for p in x], "|") *")"))
end

function alt(T::Type, x::Vararg; log=false, transform=(v,i) -> v)
    Either{T}(Any[ parser(y) for y in x ], log_transform(transform, log))
end

function TextParse.tryparsenext(t::Either, str, i, till, opts=TextParse.default_opts)
    T = result_type(t)
    fromindex=1
    for j in fromindex:length(t.options)
        ## @info "alt" str[i:till] t.options[j]
        r, i_ = tryparsenext(t.options[j], str, i, till)
        if !isnull(r)            ## @show i_ seq_join(r.value)
            # @show t.transform t.options[j] r.value
            ## @show match(Regex(regex_string(t.options[j])), str[i:end])
            try
                r_ = t.transform(r.value, i)
                return Nullable(r_), i_
            catch e
                @error "cannot transform " t.transform e r
                rethrow(e)
                return r,i
            end            
            ## return r, i_
        end
    end
    return Nullable{T}(), i
end

(|)(x::Regex, y::Regex) =
    Regex("(?:",x.pattern * "|" * y.pattern * ")")
(|)(x::String, y::Regex) =
    Regex("(?:",regex_escape(x) * "|" * y.pattern * ")")
(|)(x::Regex, y::String) =
    Regex("(?:",x.pattern * "|" * regex_escape(y) * ")")


(|)(x::Any, y::TextParse.AbstractToken) = alt(parser(x),y)
(|)(x::TextParse.AbstractToken, y::Any) = alt(x,parser(y))
(|)(x::TextParse.AbstractToken, y::TextParse.AbstractToken) = alt(x,y)





############################################################


##import Base: findnext

export splitter
splitter(S, parse; transform_split = v -> tokenize(S, v), kw...) =
    splitter(Regex(regex_string(S)), parse;
             transform_split = transform_split, kw...)

function splitter(## R::Type,
                  split::InstanceParser{Regex,S},
                  parse::TextParse.AbstractToken{T};
                  log=false,
                  transform = (v,i) -> v) where {S, T}    
    transform_split = split.transform ## (v,i) -> v
    R = promote_type(S,T)
    function tpn(str, i, n, opts) ## from util.jl:_split
        ## @show str
        ## @show R
        strs = Vector{R}(undef, 0)#[]
        lstr = str[i:min(end,n)]
        r = eachmatch(split.parser, lstr)
        j = 0
        for m in r
            if j <= m.match.offset
                ## m.match.offset  is indexed at 0!!
                ## @show lstr nextind(lstr,j) m.match.offset m.match
                before = SubString(lstr,nextind(lstr,j),prevind(lstr, m.match.offset + (transform_split===nothing ? sizeof(m.match) : 1)))
                log && @info "before" before
                push!(strs, (tokenize(parse, before))) # , i+nextind(lstr,j))) ## todo pass pos!
            end
            if transform_split!==nothing
                log && @info "split" before
                push!(strs, ( transform_split(m, i))) # , i+j) )
            end
            j = m.match.offset + sizeof(m.match) # =.ncodeunits
        end
        ## j = prevind(lstr,j)
        if j <= n-i
            after = SubString(str,i+j,min(lastindex(str),n))
            log && @info "after" after
            push!(strs,
                  (tokenize(parse, after))) ## , i+j)) ## todo pass pos!
        end
        result = transform(strs,i)
        ## error()
        log && @info "split" lstr strs result i j n
        return Nullable(result), nextind(str,n)
    end
    CustomParser(tpn, R)
end



# Base.convert(::Type{Nullable{Pair{Symbol, T}}}, x::Pair{Symbol, Nullable{S}}) where {T,S} =
#     isnull(x.second) ? Nullable{Pair{Symbol, T}}() :
#     Nullable(x.first => convert(T, x.second.value))

struct Greedy{Ps,A,F<:Function} <: TextParse.AbstractToken{Any}
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

## export tokenizer_regex
##function tokenizer_regex()
##    re = (
lf          = r"\n"
newline     = r"\r?\n"
whitespace  = r"[ \t]+"
whitenewline = r"[ \t]*\r?\n"
quotes      = r"[\"'`]"
inline      = r"[^\n\r]*"
indentation = r"[ \t]*"
content_characters = r"[^\t\r\n]+"
number      = r"[0-9]+"  ## TODO alt(...) csv
letters     = r"[A-Za-z*-]*"
# 
parenthesisP(open,close) = seq(String,
    open, r"[^][{}()]*", close;
    transform=(v,i) -> join(v))
delimiter   = r"[-, _/\.;:*\|]"
word        = r"\p{L}+" # r"[^!\[\]\(\){<>},*;:=\| \t_/\.\n\r\"'`⁰¹²³⁴⁵⁶⁷⁸⁹]+"
footnote    = r"^[⁰¹²³⁴⁵⁶⁷⁸⁹]+"
enum_label = r"(?:[0-9]{1,3}|[ivx]{1,6}|[[:alpha:]])[\.\)]"
wdelim = r"^[ \t\r\n]+"



export emptyline
emptyline = r"^[ \t]*\r?\n"

extension   = r"\.[[:alnum:]~#]+"

email_regexp = r"[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+"

## is this official??
author_email = seq(NamedTuple,
                   :name => opt(r"^[^<]+"),
                   r" <", :email => rep_until(email_regexp, r">"))


pad(x) = seq(opt(whitespace), x, opt(whitespace), transform = v->v[2])

### from tokens????
## match(r::Regex, x::TokenTuple) =  match(r, show(x))
# endswith(x::TokenTuple, suffix::AbstractString) =  endswith(x[end].value, suffix)


struct MemoTreeChildren{P}
    visited::Dict
    child::P
    descend::Bool
end





include("show.jl")
include("deprecated.jl")
include("tokens.jl")

end # module
