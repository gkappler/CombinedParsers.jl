import AbstractTrees: print_tree, printnode

"""
decurse recursive patterns
"""
struct MemoTreeChildren{P}
    visited::Dict
    child::P
    descend::Bool
end

children(x::MemoTreeChildren) =
    x.descend ?  MemoTreeChildren(children(x.child), x.visited ) : []

function printnode(io::IO, x::MemoTreeChildren)
    printnode(io, x.child)
    x.descend || isempty(children(x.child)) || printstyled(io, " # branches hidden", color=:light_black)
end

children(x::Union{Regex,AbstractString}) = ()


function Base.show(io::IO, x::Union{ConstantParser,NIndexParser})
    print(io, "re\"",regex_string(x),"\"") ##!!
end
function Base.show(io::IO, x::CombinedParser)
    if get(io,:compact,false)
        print(io, regex_string(x)) ##!!
    else
        print_tree(io, MemoTreeChildren(Dict(),x, true))
        println(io,"::",result_type(x))
    end
end


printnode(io::IO, x::CombinedParser) =
    printnode_(io, x)

function printnode_(io::IO, x::CombinedParser)
    printstyled(io, regex_prefix(x), bold=true, color=:cyan)
    if isempty(children(x))
        printstyled(io, regex_inner(x), bold=true, color=:cyan)
        ##printnode(io, x.parser)
    else
        printstyled(io, children_char,bold=true)
    end
    printstyled(io, regex_suffix(x), bold=true, color=:cyan)
    printstyled(io, " ")
    print_constructor(io,x) # , color=:yellow)
end

function MemoTreeChildren(children::Union{Vector, Tuple}, visited::Dict=Dict())
    children_ = [ MemoTreeChildren(visited, x, !haskey(visited, x)) for x in children ]
    for c in children
        visited[c] = true
    end
    children_
end
