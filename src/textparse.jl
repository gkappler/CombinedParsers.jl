
export tokenize

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





function TextParse.tryparsenext(tok::AnyChar, str, i, till, opts=TextParse.default_opts)
    if i <= till
        Nullable(str[i]), nextind(str,i)
    else
        Nullable{Char}(), i
    end
end

function TextParse.tryparsenext(tok::ConstantParser{L,<:AbstractString}, str::AbstractString, i, till, opts=TextParse.default_opts) where L
    if startswith(str[i:end], tok.parser)
        e = i+L
        ## nextind(str, i, lastindex(tok))
        Nullable(tok.parser), e
    else
        Nullable{String}(), i
    end
end
function TextParse.tryparsenext(tok::ConstantParser{L,Char}, str, i, till, opts=TextParse.default_opts) where L
    if i <= till && str[i] == tok.parser
        return(Nullable(str[i]), i+L)
        ## nextind(str,i))
    else
        Nullable{Char}(), i
    end
end


function TextParse.tryparsenext(tok::ParserPeek, str, i, till, opts=TextParse.default_opts) where {P,T}
    r, i_ = tryparsenext(tok.parser, str,i,till,opts)
    if i<=till
        if isnull(r)
            l = tok.length
            at = str[min(lastindex(str), i):min(lastindex(str), nextind(str,i,l))]
            @info "$(tok.message) no match" at # tok.parser
        else
            at = str[i:min(lastindex(str), prevind(str,i_,1))]
            @info "$(tok.message) match" at get(r) # tok.parser
        end
    end
    r, i_
end


function TextParse.tryparsenext(tok::CharIn, str, i, till, opts=TextParse.default_opts)
    if i <= till
        for s in tok.sets
            str[i] in s && return(Nullable(str[i]), nextind(str,i))
        end
    end
    Nullable{Char}(), i
end

function TextParse.tryparsenext(tok::CharNotIn, str, i, till, opts=TextParse.default_opts)
    if i <= till
        for s in tok.sets
            str[i] in s && return(Nullable{Char}(), i)
        end
    end
    (Nullable(str[i]), nextind(str,i))
end

# TextParse.tryparsenext(tok::FullText, str, i, till, opts=TextParse.default_opts) = 
#     Nullable(str[i:till]), till+1


TextParse.tryparsenext(tok::Always, str, i, till, opts=TextParse.default_opts) =
    Nullable(nothing), i
# TextParse.tryparsenext(tok::Never, str, i, till, opts=TextParse.default_opts) =
#     Nullable{Nothing}(), i

function TextParse.tryparsenext(tok::NamedToken{P,T}, str, i, till, opts=TextParse.default_opts) where {P,T}
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    if isnull(result)
        Nullable{Pair{Symbol,T}}(), i
    else
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



function TextParse.tryparsenext(tok::PositiveLookahead, str, i, till, opts=TextParse.default_opts)
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    result, i
end


function TextParse.tryparsenext(tok::NegativeLookahead, str, i, till, opts=TextParse.default_opts)
    result, i_ = tryparsenext(tok.parser, str, i, till, opts)
    if isnull(result)
        ## @info "match at" str[i:till]
        Nullable(nothing), i
    else
        Nullable{Nothing}(), i
    end
end

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
        R = tuple( $([ :(($(s)).value) for s in subresult ]...) )
        ( Nullable(_convert(T, R)), i_)
    end
end

function TextParse.tryparsenext(t::Repeat, str, i, till, opts=TextParse.default_opts)
    T = result_type(t)
    hist = eltype(T)[]
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
        ( Nullable(hist), i_ )
    end
end

function TextParse.tryparsenext(t::Optional, str, i, till, opts=TextParse.default_opts)
    T = result_type(t)
    r, i_ = tryparsenext(t.parser, str, i, till)
    if !isnull(r)
        return r, i_
    end
    ## @show default(t.els)
    r = t.default
    Nullable{T}(r), i ## todo: t.default
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
            return r, i_
        end
    end
    return Nullable{T}(), i
end

