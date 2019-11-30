
# cutright(full,tokens::Vararg) =
#     TokenizerOp{:cutright,String}([parser(full),
#                                    [ parser(x,reverse=true) for x = tokens ]...])

struct TokenizerOp{op, T, E,F<:Function} <: TextParse.AbstractToken{T}
    els::E
    f::F
end

export tok
tok(outer::Regex, result::TextParse.AbstractToken{T}) where T =
    TokenizerOp{:tokenize, T}(  ( outer=parser(outer), parser=result ), identity )
regex_string(x::TokenizerOp{:not,T,E}) where {T, E}  = regex_string(x.els[2])
regex_string(x::TokenizerOp{:tokenize,T,E}) where {T, E}  = regex_string(x.els.outer)
function TokenizerOp{op,T}(x::E, f::F) where {op, T, E, F<:Function}
    TokenizerOp{op,T,E,F}(x, f)
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

export not
"""
will always return a string
"""
not(exclude, from;  log=false) =
    TokenizerOp{:not,String}( ## todo: result_type(from)
        (parser(exclude), Regex("^"*regex_string(from))),
        (v,i) -> v)



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
