"""
    Base.escape_string(x::AbstractVector)

for printing a non-string sequence when parsing.
!!! note
    type piracy? module local `_escape_string`?
"""
Base.escape_string(x::AbstractVector) = "["*join(repr.(x),",")*"]"

log_effect(s,start,after,state,log; io=stdout, kw...) =
    log_effect(io, s,start,after,state,log; kw...)
function log_effect(io::IO, s,start,after,state,log;delta_char=5, shorten=100)
    at = "@$(start)-$(after)"
    print(io, at, " in ")
    firsti = _prevind(s,start,delta_char)
    lasti = (_prevind(s,start))
    if _prevind(s,start)<start
        # before
        escape_string_styled(io,s[max(1,firsti):lasti]; color_unescaped=treecolor.outside_match)
        # matched
        if _prevind(s,after) - start > shorten
            escape_string_styled(io, s[start:(start + shorten ÷ 2)]; color_unescaped=treecolor.match)
            printstyled(io,"[...]"; color=treecolor.match)
            escape_string_styled(io, s[(_prevind(s,after) - shorten ÷ 2):_prevind(s,after)]; color_unescaped=treecolor.match)
        else
            escape_string_styled(io, s[start:_prevind(s,after)]; color_unescaped=treecolor.match)
        end
    else
    end
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
    printstyled(IOContext(io, :compact => true),log,"\n",color=treecolor.name, bold=false)
end

function log_effect_match(s,start,after,state,log; kw...)
    if state!==nothing && start!=after
        log_effect(s,start,after,state,log; kw...)
    end
end
