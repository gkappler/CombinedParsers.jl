import TextParse: tryparsenext
export tryparsenext, tokenize

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


function TextParse.tryparsenext(x::AbstractParser,str,i,till,opts=TextParse.default_opts)
    s = _iterate(x,str,till,i,nothing)
    if s === nothing
        Nullable{result_type(x)}(),i
    else
        Nullable(get(x,str,till,s[1],i,s[2])),s[1]
    end
end























