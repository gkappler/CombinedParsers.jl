regex_flags(x) = replace(string(x), r"^.*\"([^\"]*)$"s => s"\1")

(*)(x::Regex, y::Regex) =
    Regex(x.pattern * y.pattern)
(*)(x::String, y::Regex) =
    Regex(regex_escape(x) * y.pattern)
(*)(x::Regex, y::String) =
    Regex(x.pattern * regex_escape(y))

result_type(x::Type{Regex}) = AbstractString
parser(x::Regex) = Regex("^" * regex_string(x), regex_flags(x))
## revert(x::Regex) = Regex(regex_string(x) * '$', regex_flags(x))
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
tokenize(x, str::RegexMatch) = tokenize(x, str.match)

(|)(x::Regex, y::Regex) =
    Regex("(?:",x.pattern * "|" * y.pattern * ")")
(|)(x::String, y::Regex) =
    Regex("(?:",regex_escape(x) * "|" * y.pattern * ")")
(|)(x::Regex, y::String) =
    Regex("(?:",x.pattern * "|" * regex_escape(y) * ")")


export trimstring
trimstring(x::AbstractString) =
    replace(x, r"^[ \r\n\t]*|[ \r\n\t]*$" => s"")

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
delimiter   = r"[-, _/\.;:*\|!?&]"
word        = r"\p{L}+" # r"[^!\[\]\(\){<>},*;:=\| \t_/\.\n\r\"'`⁰¹²³⁴⁵⁶⁷⁸⁹]+"
footnote    = r"^[⁰¹²³⁴⁵⁶⁷⁸⁹]+"
enum_label = r"(?:[0-9]{1,3}|[ivx]{1,6}|[[:alpha:]])[\.\)]"
wdelim = r"^[ \t\r\n]+"

pad(x) = seq(opt(whitespace), x, opt(whitespace), transform = v->v[2])



export emptyline
emptyline = r"^[ \t]*\r?\n"

extension   = r"\.[[:alnum:]~#]+"

email_regexp = r"[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+"

## is this official??
author_email = seq(:name => JoinSubstring(rep(CharNotIn('<'))),
                   " <", :email => rep_until(email_regexp, r">"))




import Base: Regex
function Regex(x::ParserTypes) 
    Regex("^"*regex_string(x))
end

## todo: optimize function
## rep(x::Regex) = Regex(regex_string(rep(x; log=false)))
function Base.get(parser::Regex, sequence, after, i, till, state)
    isempty(state.captures) ? state.match : state.captures
end


"""
Match a regex greedily, and iterate only over that result.
Caveat: If shorter matches exist these will not be iterated because julia PCRE does not support states.
"""
function _iterate(tok::Regex, str, i, till, state)
    state !== nothing && return nothing
    m = match(tok, SubString(str,i,till))
    if m === nothing
        nothing
    else
        ni = m.match =="" ? i : nextind(str, i, length(m.match))
        ni, (i,till,m)
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
# function alt(x::Vararg{Union{String,Regex}})
#     T = AbstractString
#     instance(T, (v,i) -> v, Regex("^(?:" * join([regex_string(p) for p in x], "|") *")"))
# end

export splitter
splitter(S, parse; transform_split = v -> tokenize(S, v), kw...) =
    splitter(Regex(regex_string(S)), parse;
             transform_split = transform_split, kw...)

function splitter(## R::Type,
                  split::Transformation{Regex,S},
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

export regex_tempered_greedy, regex_neg_lookahead
# https://www.rexegg.com/regex-quantifiers.html#tempered_greed
regex_tempered_greedy(s,e, flags="s"; withend=true) =
    Regex("^"*regex_string(s)*"((?:(?!"*regex_string(e)*").)*)"*
          ( withend ? regex_string(e) : ""),flags)

# https://www.rexegg.com/regex-quantifiers.html#tempered_greed
regex_neg_lookahead(e, match=r".") =
    instance_at(String,
                Regex("^((?:(?!"*regex_string(e)*")"*regex_string(match)*")*)","s")) do v,i
                    v[1]
                end
