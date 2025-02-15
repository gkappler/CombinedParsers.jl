"""
    Base.escape_string(x::AbstractVector)

for printing a non-string sequence when parsing.
!!! note
    type piracy? module local `_escape_string`?
"""
Base.escape_string(x::AbstractVector) = "["*join(repr.(x),",")*"]"

log_effect(s,start,after,state,log; io=stdout, kw...) =
    log_effect(io, s,start,after,state,log; kw...)
function log_effect(io::IO, s,start,after,state,log;delta_char=5)
    at = "@$(start)-$(after)"
    print(io, at, " in ")
    firsti = _prevind(s,start,delta_char)
    lasti = (_prevind(s,start))
    before, matched = if _prevind(s,start)<start
        escape_string(s[max(1,firsti):lasti]), escape_string(s[start:_prevind(s,after)])
    else
        "",""
    end
    if lastindex(matched)>100
        matched=matched[1:_nextind(matched,1,20)]*"[...]"*matched[_prevind(matched,end,20):end]
    end
    printstyled(io, before; color=treecolor.outside_match)
    printstyled(io, matched; color=treecolor.match)
    li = after>lastindex(s) ? lastindex(s) : _nextind(s,after,delta_char)
    if state === nothing 
        printstyled(io, escape_string(s[after:min(end,li)]),
                    color=treecolor.nomatch)
    elseif after<=lastindex(s)
        printstyled(io, escape_string(s[after:min(end,li)]),
                    color=treecolor.outside_match)
    end
    if !get(io,:compact,true)
        println(io)
    else#
        print(io," ")
    end
    if state === nothing
        printstyled(io, "is no ", color=treecolor.nomatch)
    else
    end
        #print(io, "   ")
        printstyled(io, "is a";)
        print(io, " ")
    printstyled(IOContext(io, :compact => true),log,"\n",color=treecolor.match, bold=false)
end

function log_effect_match(s,start,after,state,log; kw...)
    if state!==nothing && start!=after
        log_effect(s,start,after,state,log; kw...)
    end
end
