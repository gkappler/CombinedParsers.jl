module ParserAlchemy

using Parameters
using Nullables

using TextParse
import TextParse: tryparsenext
import BasePiracy: _convert

export tryparsenext, tokenize, result_type

export trimstring
trimstring(x::AbstractString) =
    replace(x, r"^[ \r\n\t]*|[ \r\n\t]*$" => s"")


############################################################
## Parsing with TextParse.AbstractToken, operators

ParserTypes = Union{TextParse.AbstractToken, AbstractString, Regex,
                    Pair{Symbol,
                         <:Union{TextParse.AbstractToken, AbstractString, Regex}}}
# Parser = Union{TextParse.AbstractToken}

## import Regex: match
export match
Base.match(r::TextParse.AbstractToken, str) =
    match(Regex(regex_string(r)), str)

export tokenize
tokenize(x, str::RegexMatch) = tokenize(x, str.match)

"""
tokenize(x, str; delta=200, errorfile=nothing)

Tokenize string or iterator `str` with parser `x`.
"""
function tokenize(x, str; delta=200, errorfile=nothing)
    i=firstindex(str)
    till=lastindex(str)
    r, i_ = tryparsenext(x, str, i, till, TextParse.default_opts)
    if i_<=till && errorfile!==nothing
        open(errorfile, "a") do io
            println(io, "* incomplete parsing stopped at $i_ ")
            println(io, "error at")
            println(io, str[min(i_,end):min(end, nextind(str,i_,delta))])
            println(io, "** data")
            println(io, str)
        end
    end
    if isnull(r)
        nothing
    else
        get(r)
    end
end

import Base: Regex
function Regex(x::ParserTypes) 
    Regex("^"*regex_string(x))
end

export opt, seq, rep, rep_splat, rep1, alt

export NamedToken, InstanceParser, instance
struct NamedToken{P,T} <: TextParse.AbstractToken{Pair{Symbol,T}}
    name::Symbol
    parser::P
end
struct InstanceParser{P,T} <: TextParse.AbstractToken{T}
    transform::Function
    parser::P
    a::Vector
end
InstanceParser{T}(transform::Function, p::P) where {T, P} = InstanceParser{P,T}(transform, p)


function Base.show(io::IO, x::InstanceParser{P,T}) where {P,T}
    compact = get(io, :compact, false)
    if false && !compact
        print_tree(io, x.value) ##!!
    else
        print_tree(io,x)
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


export instance 

import Nullables: Nullable, isnull
remove_null(x::Nullable{Vector{T}}) where T =
    isnull(x) ? T[] : x.value

remove_null(x::Nullable, default=missing) =
    isnull(x) ? default : remove_null(x.value)

remove_null(x::Pair, default=missing) =
    x.first => remove_null(x.second)

remove_null(x) =
    x

import VectorDicts: VectorDict

function instance(t::Type{NamedTuple{n,ts}}, v::Vector, i; kw...) where {n,ts}
    vs = Any[ remove_null(x) for x in v ]
    kvs = VectorDict(Pair{Symbol}[ x
                       for x in vs
                       if (x isa Pair && x.first !=:_match) ])
    ks = Any[ x.first for x in kvs ]
    NamedTuple{n, ts}(tuple([ let fn = fieldname(t,i)
         _convert(fieldtype(t, i),
                       get(kw, fn) do
                       get(kvs, fn, :missing)
                       end)
         end
         for i =1:length(n) ]...) )
end

function instance(::Type{T}, p::P, a...) where {T, P<:ParserTypes}
    InstanceParser{P,T}((v,i) -> T(a..., v), p,[a...])
end
function instance(::Type{T}, p::P) where {T, P<:ParserTypes}
    InstanceParser{P,T}((v,i) -> _convert(T,v), p, [])
end
function instance(::Type{T}, f::Function, p::P, a...) where {T, P<:ParserTypes}
    InstanceParser{P,T}((v,i) -> _convert(T,f((v), i, a...)), p,[a...])
end


struct TokenizerOp{op, T, F} <: TextParse.AbstractToken{T}
    els::F
    f::Function
end

quantifier(x::TokenizerOp{:rep,T,F}) where {T, F}  =  "*"
# quantifier(x::TokenizerOp{:rep_splat,T,F}) where {T, F}  = "*"
quantifier(x::TokenizerOp{:rep1,T,F}) where {T, F}  =  "+"
quantifier(x::TokenizerOp{:opt,T,F}) where {T, F}  = "?"



export regex_string
regex_string(::TextParse.Numeric{<:Integer}) = "[[:digit:]]+"
regex_string(x::Union{NamedToken, InstanceParser}) = regex_string(x.parser)
regex_string(x::Pair{Symbol,T}) where T = regex_string(x.second)
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

regex_string(x::TokenizerOp) = "(?:" * regex_string(x.els) * ")" * quantifier(x)
regex_string(x::TokenizerOp{:not,T,F}) where {T, F}  = regex_string(x.els[2])
regex_string(x::TokenizerOp{:seq,T,F}) where {T, F}  = join([ regex_string(p) for p in x.els.parts])
regex_string(x::TokenizerOp{:opt,T,F}) where {T, F}  = regex_string(x.els.parser)
regex_string(x::TokenizerOp{:tokenize,T,F}) where {T, F}  = regex_string(x.els.outer)
regex_string(x::TokenizerOp{:alt,T,F}) where {T, F}  = "(?:" * join([ regex_string(p) for p in x.els],"|") * ")"



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
    NamedToken{P,result_type(x.second)}(x.first, parser(x.second))

parser(t::Tuple) = tuple([ parser(x) for x in t ]...)
parser(x::Pair{Symbol, Tuple{P, Type}}) where P =
    NamedToken{P,x.second[2]}(x.first, x.second[1])


function TokenizerOp{op,T}(x::F, f) where {op, T, F}    
    TokenizerOp{op,T,F}(x, f)
end
# function TokenizerOp{op,T}(x::F, f) where {op, T, F}    
#     r = TokenizerOp{op,T,F}(x, f)
#     TokenizerOp{op,T,F}(x, f)
# end

result_type(x::TextParse.AbstractToken{T}) where T = T
result_type(x::Pair{Symbol, <:T}) where T =
    Pair{Symbol, result_type(x.second)}
result_type(x::AbstractString) = AbstractString
result_type(x::Regex) = SubString{String}


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
    opt(result_type(x), x; kw...)

defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type{<:VectorDict}) = VectorDict{keytype(V), valtype(V)}(eltype(V)[])
defaultvalue(V::Type) = missing

function opt(T::Type, x;
             default=defaultvalue(T),
             log=false,
             transform=(v,i) -> v) where { D }
    ##@show default
    x=parser(x)
    RT = promote_type(T,typeof(default))
    TokenizerOp{:opt,RT}(
        (parser=x, default=default),
        log_transform(transform, log))
end

function alt(x::Vararg{ParserTypes})
    parts = [ parser(y) for y in x ]
    T = promote_type([ result_type(x) for x in parts]...)
    TokenizerOp{:alt,T}(parts, (v,i) -> v)
end

function alt(x::Vararg{Union{String,Regex}})
    T = AbstractString
    instance(T, (v,i) -> v, Regex("^(?:" * join([regex_string(p) for p in x], "|") *")"))
end

function alt(T::Type, x::Vararg; log=false, transform=(v,i) -> v)
    TokenizerOp{:alt,T}(
        (Any[ parser(y) for y in x ]), log_transform(transform, log))
end

## @deprecate join_seq(tokens::Vararg; kw...) seq(tokens...; transform=seq_join, kw...)
## import Base.map
## Base.map(f::Function) = x -> map(f,x)

function seq(tokens::Vararg{ParserTypes};
             transform=nothing, kw...)
    parts = [ parser(x) for x = tokens ]
    T = [ result_type(x) for x in parts]
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

# struct InnerParser{I,P,T} <: TextParse.AbstractToken{T}
#     inner::I
#     outer::P
# end

function seq(T::Type, tokens::Vararg;
             combine=false, outer=nothing,
             partial = false,
             log=false,
             transform=:instance, kw...)
    parts = [ parser(x) for x = tokens ]
    ## todo: tuple?    
    if combine
        if outer===nothing
            outer = Regex("^"*join([ "("*regex_string(x)*")" for x in parts ]))
        else
            @assert outer==join([ "("*regex_string(x)*")" for x in parts ])
        end
    end
    if T==NamedTuple
        fnames = tuple( [ x.name for x in parts if x isa NamedToken ]... )
        ftypes = [ result_type(x.parser) for x in parts if x isa NamedToken ]
        RT = isempty(fnames) ? T : NamedTuple{fnames, Tuple{ftypes...}}
    else
        RT = T
    end
    if transform == :instance
        transform = (v,i) -> instance(RT,v,i)
    end
    tr = log_transform(transform, log)
    result = TokenizerOp{:seq, RT}((parts=parts, log=log, partial=partial),
                                   tr)
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

export tok
tok(outer::Regex, result::TextParse.AbstractToken{T}) where T =
    TokenizerOp{:tokenize, T}(  ( outer=parser(outer), parser=result ), identity )

@deprecate rep_splat(x) TokenizerOp{:rep_splat,String}([parser(x)])
# rep(x)

rep1(x::ParserTypes;  kw...) where T =
    rep1(Vector{result_type(x)},x; kw...)
rep1(T::Type, x;  log=false, transform=(v,i) -> v) =
    TokenizerOp{:rep1,T}(parser(x), log_transform(transform, log))


rep1(T::Type, x,y::Vararg; log=false, transform=(v,i) -> v, kw...) =
    rep1(T, seq(x,y...; transform=log_transform(transform, log), kw...), transform=(v,i) -> v)

rep(x::T; kw...) where T =
    rep(Vector{result_type(x)},x; kw...)
    
rep(x::TextParse.AbstractToken{T};  kw...) where T =
    rep(Vector{T},x; kw...)

rep(T::Type, x;  log=false, transform=(v,i) -> v ) = #[ convert(T,i) for i in v ] ) =
    TokenizerOp{:rep,T}(parser(x), log_transform(transform, log))

rep(x::Regex) =
    Regex(regex_string(rep(x; log=false)))


rep(T::Type, x,y::Vararg; log=false, transform=(v,i) -> v, kw...) =
    rep(T, seq(x,y...; transform=log_transform(transform, log), kw...); transform=(v,i) -> v)

# rep(x) =
#     TokenizerOp{:rep,String}([parser(x)], seq_vcat)
# rep(x,y::Vararg; transform=seq_vcat, kw...) =
#     rep(seq(x,y...; transform=transform, kw...); transform=seq_vcat, kw...)
# rep(f::Function,x) =
#     TokenizerOp{:rep,String}([parser(x)],f)


export not
"""
will always return a string
"""
not(exclude, from;  log=false) =
    TokenizerOp{:not,String}( ## todo: result_type(from)
        (parser(exclude), Regex("^"*regex_string(from))),
        (v,i) -> v)

# cutright(full,tokens::Vararg) =
#     TokenizerOp{:cutright,String}([parser(full),
#                                    [ parser(x,reverse=true) for x = tokens ]...])



import Base: (*), (|), cat
(*)(x::Regex, y::Regex) =
    Regex(x.pattern * y.pattern)
(*)(x::String, y::Regex) =
    Regex(regex_escape(x) * y.pattern)
(*)(x::Regex, y::String) =
    Regex(x.pattern * regex_escape(y))

#(*)(x::TokenizerOp{op,T,F}, y::P) where {op,T,F,P} =
#    TokenizerOp{:seq,T}([x,parser(y)])
# (*)(x::P, y::TokenizerOp) where {P} = seq(x,y)



(*)(x::Any, y::TextParse.AbstractToken) = seq(parser(x),y)
(*)(x::TextParse.AbstractToken, y::Any) = seq(x,parser(y))
(*)(x::TextParse.AbstractToken, y::TextParse.AbstractToken) = seq(x,y)

(|)(x::Regex, y::Regex) =
    Regex("(?:",x.pattern * "|" * y.pattern * ")")
(|)(x::String, y::Regex) =
    Regex("(?:",regex_escape(x) * "|" * y.pattern * ")")
(|)(x::Regex, y::String) =
    Regex("(?:",x.pattern * "|" * regex_escape(y) * ")")


(|)(x::Any, y::TextParse.AbstractToken) = alt(parser(x),y)
(|)(x::TextParse.AbstractToken, y::Any) = alt(x,parser(y))
(|)(x::TextParse.AbstractToken, y::TextParse.AbstractToken) = alt(x,y)

#Base.show(io::IO, x::NamedToken{P,T}) where{P,T} = print(io,x.name," => $T ",x.parser)




function TextParse.tryparsenext(tok::NamedToken{P,T}, str, i, till, opts=TextParse.default_opts) where {P,T}
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    if isnull(result)
        Nullable{Pair{Symbol,T}}(), i
    else
        ## cz@show tok (result)
        Nullable(tok.name => get(result)), i_
    end
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


# function Base.show(io::IO, v::Vector{<:ParserTypes})
#     print(io, join(string.(v),", "))
# end




export regex_escape
## https://github.com/JuliaLang/julia/pull/29643/commits/dfb865385edf19b681bc0936028af23b1f282b1d
## escaping ##
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
regex_string(x::AbstractString) = regex_escape(x)



# import Base.join
# export join
# Base.join(f::Function, transform::Function, x, delim) = 
#     seq(transform, opt(delim), f(x * delim), opt(x))

export seq_vcat
seq_vcat(r, i) = seq_vcat(r)
seq_vcat(r::Nothing)  = [ ]
seq_vcat(r::T) where T = T[ (r) ]
seq_vcat(r::Vector) = vcat( [ ( seq_vcat( x )) for x in r]... )






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

function TextParse.tryparsenext(tok::AbstractString, str::AbstractString, i, till, opts=TextParse.default_opts)
    if startswith(str[i:end], tok)
        e = nextind(str, i, lastindex(tok))
        Nullable(tok), e
    else
        Nullable{String}(), i
    end
end

function TextParse.tryparsenext(tok::Regex, str, i, till, opts=TextParse.default_opts)
    m = match(tok, str[i:end]) ## idx not working with ^, and without there is no option to force it at begin
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

function parser(outer::Regex, x::TokenizerOp{op, T, F}) where {op, T, F}
    TokenizerOp{op, T, F}((outer, x.els), x.f)
end

export pair_value
"""
pair_value?
transform all values as instances of pairs for key.
"""
function pair_value(key)
    v -> [ Symbol( j.value.second)
           for j in v
           if (j.value) isa Pair && key == j.value.first
           ]
end

@deprecate value_tag(key) pair_value(key)


# Base.convert(::Type{Nullable{Pair{Symbol, T}}}, x::Pair{Symbol, Nullable{S}}) where {T,S} =
#     isnull(x.second) ? Nullable{Pair{Symbol, T}}() :
#     Nullable(x.first => convert(T, x.second.value))

export greedy
function greedy(tokens...;
                alt = [],
                transform=(v,i) -> v, log=false)
    TokenizerOp{:greedy,Any}(
        (pairs=[tokens...], alt=alt), log_transform(transform, log))
end

function TextParse.tryparsenext(tokf::TokenizerOp{:greedy, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    sections=tokf.els.pairs
    RT(key, value) = if value[2] isa ParserTypes
        if Missing <: result_type(key)
            result_type(value[2])
        else
            promote_type(result_type(key), result_type(value[2]))
        end
    else
        result_type(key)
    end
    R = Dict([ value[1] => Vector{RT(key,value)}() for (key,value) in sections]...,
             [ key => Vector{result_type(value)}() for (key,value) in tokf.els.alt]...
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
            while ai < lastindex(tokf.els.alt) && isnull(cr)
                ai = ai+1
                cr, ci = tryparsenext(tokf.els.alt[ai].second, str, i_, till)
            end
            if isnull(cr)
                return Nullable{T}(_convert(T, tokf.f(R,i))), i_
            elseif ai == 0
                push!(hist, get(cr))
                i__ = ci
                false
            else
                hist = [get(cr)]
                (aggregator, last_content) = tokf.els.alt[ai]
                last_section = ai
            end
        else
            if last_section !== nothing
                c=R[aggregator]
                R[aggregator] = vcat(c,hist)
            end
            hist = get(r) !== missing ? [get(r)] : Vector{RT(key,content)}()
            aggregator, last_content = content
            last_section = key
        end
        i_ = i__
    end
    error("unreachable")
end

## todo: replace with isequalfields?
export isa_reordered
isa_reordered(x::T,S::Type{NamedTuple{n2,t2}}) where {T, n2,t2} =
    all([ fieldtype(T,key) <: fieldtype(S,key)
          for key in n2 ])

isa_reordered(x::T,S::Type) where {T} =
    x isa S

function TextParse.tryparsenext(tokf::TokenizerOp{:seq, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    toks::Vector = tokf.els.parts
    result=Vector{Any}(undef, length(toks))
    i_::Int = i
    for j in 1:length(toks)
        ##@show toks[j]
        ##@info "seq" typeof(toks[j])
        r, i__ = tryparsenext(toks[j], str, i_, till)
        if !isnull(r)
            # @info "seq" str[i_:min(i__,end)] r.value typeof(toks[j]) str[min(i__,end):end]
            ## @show toks[j] typeof(r) r.value
            result[j] = r.value
            i_ = i__
        else
            # @info "seq" str[i_:min(i__,end)] (toks[j]) str[min(i__,end):end]
            ## j>1 && @info "abort match $j=$(toks[j])" (toks) str[i_:end]  i_, till result[j-1]
            j>1 && tokf.els.partial && return ( Nullable(result[1:j-1]), i_)
            return Nullable{T}(), i
        end
    end
    ## @show result
    R = tokf.f(result, i)
    !isa_reordered(R, T) && let S = typeof(R)
        @warn "transformed wrong " result S T
    end
    return ( Nullable(_convert(T, R)), i_)
end


regex_string(x::TokenizerOp{:seq_combine,T,F}) where {T, F}  = regex_string(x.els[1])
function TextParse.tryparsenext(tokf::TokenizerOp{:seq_combine, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    re, toks = tokf.els.outer, tokf.els.parts
    # inner regex compiled in els?
    m = match(re, str[i:end])
    m===nothing && return Nullable{Vector{Any}}(), i
    result=Vector{Any}(undef, length(toks))
    i_ = i 
    for j in 1:length(toks)
        r = if toks[j] isa Union{AbstractString, Regex}
            m.captures[j] ##, i+m.captures[j].offset))
        else
            tokenize((toks[j]), m.captures[j] === nothing ? "" : m.captures[j])
        end 
        if r !== nothing
            result[j] = r  ## todo: in tokenize! shift_match_start(, i_-1)
            i_ = i_ + sizeof(m.captures[j]) ##.ncodeunits
        else
            ##j>1 && @info "abort match" toks[j] str[i_:end]  i_, till
            return Nullable{T}(), i
        end
    end
    ## @show toks
    ( Nullable(tokf.f(result, i)), i_)
end

function TextParse.tryparsenext(t::TokenizerOp{:tokenize, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    r, i_ = tryparsenext(t.els.outer, str, i, till, opts)
    isnull(r) && return Nullable{T}(), i
    inner = tokenize(t.els.parser, get(r))
    if inner === nothing
        @warn "matched outer but not inner parser" get(r) t.els.parser
        ( Nullable{T}(), i )
    else
        ( Nullable{T}(inner), i_ )
    end
    ## instance(RT, (v,i) -> tokenize(result, v), outer)
end

function TextParse.tryparsenext(t::TokenizerOp{:rep, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    hist = Any[]
    i_=i
    repval, i__ = tryparsenext(t.els, str, i_, till)
    while !isnull(repval) && i_ != i__
        push!(hist, repval.value)
        i_=i__
        repval, i__ = tryparsenext(t.els, str, i_, till)
    end
    try
        ( Nullable(_convert(T,t.f(hist,i_))), i_)
    catch e
        @error "cannot convert to $T" e hist t
        error()
    end
end

function TextParse.tryparsenext(t::TokenizerOp{:rep1, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    ## @info "rep1" t.els
    hist = Any[]
    i_=i
    repval, i__ = tryparsenext(t.els, str, i_, till)
    while !isnull(repval) && i_ != i__
        push!(hist, repval.value)
        i_=i__
        repval, i__ = tryparsenext(t.els, str, i_, till)
    end
    if isempty(hist)
        Nullable{T}()
    else
        ( Nullable(_convert(T,t.f(hist,i_))), i_)
    end
end


default(x) = nothing
default(x::Regex) = ""
default(x::NamedToken) = x.name => missing
default(x::TokenizerOp{:seq,T,V}) where {T,V} = tuple()



function TextParse.tryparsenext(t::TokenizerOp{:opt, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    r, i_ = tryparsenext(t.els.parser, str, i, till)
    if !isnull(r)        # @show typeof(r) r
        try
            r_ = _convert(T,t.f(r.value, i))
            return Nullable(r_), i_
        catch e
            @error "error transforming" e t t.f
            rethrow(e)
        end
    end
    ## @show default(t.els)
    r = t.els.default
    Nullable(r), i ## todo: t.default
end

function TextParse.tryparsenext(t::TokenizerOp{:alt, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    fromindex=1
    for j in fromindex:length(t.els)
        ## @info "alt" str[i:till] t.els[j]
        r, i_ = tryparsenext(t.els[j], str, i, till)
        if !isnull(r)            ## @show i_ seq_join(r.value)
            # @show t.f t.els[j] r.value
            ## @show match(Regex(regex_string(t.els[j])), str[i:end])
            try
                r_ = t.f(r.value, i)
                return Nullable(r_), i_
            catch e
                @error "cannot transform " t.f e r
                rethrow(e)
                return r,i
            end            
            ## return r, i_
        end
    end
    return Nullable{T}(), i
end

function TextParse.tryparsenext(t::TokenizerOp{:not, T, F}, str, i, till, opts=TextParse.default_opts) where {T,F}
    exclude, from = t.els
    ## @show str[i:till]
    r_,i_ = tryparsenext(from, str, i, till, opts)
    if !isnull(r_)
        re, ie = tryparsenext(exclude, str, i, till, opts)
        !isnull(re) && return Nullable{T}(), i
    end
    ## @show isnull(r_)
    r_, i_
end


# function TextParse.tryparsenext(tokf::TokenizerOp{:cutright, T, F}, a...) where {T,F}
#     @show r, i = tryparsenext(tokf.els,a...)
#     isnull(r) && return r,i
#     toks=tokf.els
#     result=Vector{Tuple{Any, Int}}(undef, length(toks))
#     @show str = r.value[1]
#     str isa Pair && (str =str.second)
#     till=lastindex(str)
#     i_::Int = 1
#     for j in length(toks):-1:1
#         @show parser(toks[j],reverse=true) str i_, till
#         @show r, i__ = tryparsenext(toks[j], str, i_, till)
#         if !isnull(r)
#             if r.value isa Vector{Tuple{T,Int}} where T
#                 result[j] = (r.value, r.value[1][2])
#             else
#                 # typeof(r.value), ( r.value isa Vector{Tuple{T,Int64}}  where T )
#                 result[j] = r.value
#             end
#             i_ = i__
#         else
#             ## dump(toks[j])
#             ##j>1 && @info "abort match" toks[j] str[i_:end]  i_, till            
#             return Nullable{Vector{Tuple{String,Int}}}(), i
#         end
#     end
#     return Nullable(vcat(result...)), i_
# end






export alternate
    
alternate(x::Vector, delim; kw...) = alternate(alt(x...), delim; kw...)
function alternate(x::ParserTypes, delim::ParserTypes;
                   log=false,
                   repf=rep,
                   appendf = nothing,
                   kw...)
    T, S = result_type(x), result_type(delim)
    af = if appendf === nothing
        ( ( v, nl, i ) -> T[ v ] )
    else
        appendf
    end
    seq(Vector{T},
        repf(Vector{T},
             seq(Vector{T}, x, delim; log=log,
                 transform = (v,i) -> af(v...,i));
             transform = (v,i) -> vcat(v...),
             log=log),
        opt(Vector{T}, x; default=T[], log=log, transform = (v,i) -> T[v] )
        ; log=log,
        ## todo: factor out this transform condition!!
        transform = ( v, i ) -> vcat(v[1], v[2]) 
        , kw...)
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
    T = result_type(x)
    D = result_type(delim)
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
quotes      = r"[\"'`]+"
inline      = r"[^\n\r]*"
whitespace  = r"[ \t]+"
indentation = r"[ \t]*"
content_characters = r"[^\t\r\n]+"
number      = r"[0-9]+"  ## TODO alt(...) csv
letters     = r"[A-Za-z*-]*"
# 
parenthesisP(open,close) = seq(String,
    open, r"[^][{}()]*", close;
    transform=(v,i) -> join(v))
delimiter   = r"[-, _/\.;:*]+"
word        = r"[^\[\]\(\){<>},*;:=\| \t_/\.\n\r\"'`⁰¹²³⁴⁵⁶⁷⁸⁹]+"
footnote    = r"^[⁰¹²³⁴⁵⁶⁷⁸⁹]+"
enum_label = r"(?:[0-9]{1,3}|[ivx]{1,6}|[[:alpha:]])[\.\)]"


whitenewline = Regex(regex_string(seq(opt(whitespace), newline)))
export emptyline
emptyline = r"^[ \t]*\r?\n"

extension   = r"\.[[:alnum:]~#]+"

email_regexp = r"[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+"
author_email = seq(NamedTuple,
                   :name => opt(instance(String,r"^[^<]+")),
                   r" <",
                   :email => email_regexp,
                   r">"; combine=true)


pad(x) = seq(opt(whitespace), x, opt(whitespace), transform = v->v[2])

### from tokens????
## match(r::Regex, x::TokenTuple) =  match(r, show(x))
# endswith(x::TokenTuple, suffix::AbstractString) =  endswith(x[end].value, suffix)


struct MemoTreeChildren{P}
    visited::Dict
    child::P
    descend::Bool
end





import AbstractTrees: print_tree, children, printnode
import AbstractTrees: print_tree, children, printnode
# Base.show(io::IO, x::TokenizerOp) =
#     print_tree(io, x)
function Base.show(io::IO, x::TextParse.AbstractToken{T}) where {T}
    compact = get(io, :compact, false)
    if false && !compact
        print(io, x) ##!!
    else
        print_tree(io, MemoTreeChildren(Dict(),x, true))
    end
end
printnode(io::IO, x::TextParse.AbstractToken{T}) where {T} =
    print(io, "$T = ", x)
printnode(io::IO, x::TokenizerOp{op, T, F}) where {op, T, F} =
    print(io, "$T = $op")
function printnode(io::IO, x::TokenizerOp{:opt, T, F}) where {T, F}
    print(io, "$T = opt ")
    printnode(io, x.els.parser)
end
function printnode(io::IO, x::TokenizerOp{:tokenize, T, F}) where {T, F}
    print(io, "$T = tokenize ")
    printnode(io, x.els.outer)
end
function printnode(io::IO, x::NamedToken{P, T}) where {P, T} 
    print(io, x.name, "::")
    printnode(io, x.parser)
end
    
function printnode(io::IO, x::MemoTreeChildren{P}) where {P}
    printnode(io, x.child)
    x.descend || print(io, "(see above)")
end
function printnode(io::IO, x::InstanceParser{P,T}) where {P,T} 
    print(io,"",T,"(",join(string.(x.a)), ") = ")
    printnode(io, x.parser)
end

function MemoTreeChildren(children::Union{Vector, Tuple}, visited::Dict=Dict())
    children_ = [ MemoTreeChildren(visited, x, !haskey(visited, x)) for x in children ]
    for c in children
        visited[c] = true
    end
    children_
end

children(x::MemoTreeChildren) =
    x.descend ?  MemoTreeChildren(children(x.child), x.visited ) : []

children(x::Union{Regex,AbstractString}) =
    ()
children(x::InstanceParser) =
    children(x.parser)
children(x::NamedToken) =
    children(x.parser)
children(x::TokenizerOp{:rep, T, F}) where {T, F} =
    [ x.els ]
children(x::TokenizerOp{:rep1, T, F}) where {T, F} =
    [ x.els ]
children(x::TokenizerOp{:alt, T, F}) where {T, F} =
    x.els
# children(x::TokenizerOp{:greedy, T, F}) where {T, F} =
#     x.els
children(x::TokenizerOp{:opt, T, F}) where {T, F} =
    children(x.els.parser)
children(x::TokenizerOp{:seq_combine, T, F}) where {T, F} =
    x.els.parts
children(x::TokenizerOp{:seq, T, F}) where {T, F} =
    x.els.parts
children(x::TokenizerOp{:tokenize, T, F}) where {T, F} =
     [ x.els.parser ]

## TODO: Tree printing
# AbstractTrees.print_node(io::IO, x::NamedTuple) =
#     for (n,v) in pairs(x)
#         print(io, "$n = $v")
#     end
# import AbstractTrees: printnode
# export printnode
# AbstractTrees.printnode(io::IO, x::Pair{K,T}) where {K,T} = print(io, x.first, ": ", x.second)
# AbstractTrees.printnode(io::IO, x::NamedTuple) = print(io, x)
# AbstractTrees.children(x::Pair{K,T}) where {K,T} = []

# AbstractTrees.children(x::NamedTuple) =
#     collect(pairs(x))


# AbstractTrees.children(x::TokenString) = []


Base.show(io::IO, x::Tuple{Nullable,Int}) =
    !isnull(x[1]) &&  show(io, x[1].value)

include("tokens.jl")

end # module
