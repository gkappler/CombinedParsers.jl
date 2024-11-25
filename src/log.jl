"""
    Base.escape_string(x::AbstractVector)

for printing a non-string sequence when parsing.
!!! note
    type piracy? module local `_escape_string`?
"""
Base.escape_string(x::AbstractVector) = "["*join(repr.(x),",")*"]"

log_effect(s,start,after,state,log,delta=5) =
    log_effect(stdout, s,start,after,state,log,delta)
function log_effect(io::IO, s,start,after,state,log,delta=5)
    at = "@$(start)-$(after)"
    if state === nothing
        printstyled(io, "no match ", color=:underline)
    else
        print(io, "   ")
        printstyled(io, "match";
                    bold=false,color=:underline)
        print(io, " ")
    end
    printstyled(IOContext(io, :compact => true),log,"\n",color=:green, bold=false)
    print(io, at,": ")
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
    printstyled(io, before; bold=true)
    printstyled(io, matched; bold=true,color=:underline)
    li = after>lastindex(s) ? lastindex(s) : _nextind(s,after,delta)
    if state === nothing 
        printstyled(io, escape_string(s[after:min(end,li)]),
                    bold=true,color=:underline)
    elseif after<=lastindex(s)
        printstyled(io, escape_string(s[after:min(end,li)]),
                    color=:darkgray)
    end
    println(io)
    if !get(stdout,:color,false)
        print(io, " "^(11+length(at)+length(log)+length(before)),"^")
        if length(matched)>1
            print(io, "_"^(length(matched)-2),"^")
        end
        println(io)
    end
end

function log_effect_match(s,start,after,state,log,delta)
    if state!==nothing && start!=after
        log_effect(s,start,after,state,log,delta)
    end
end
