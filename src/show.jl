

import AbstractTrees: print_tree, children, printnode
function Base.show(io::IO, x::TextParse.AbstractToken)
    compact = get(io, :compact, false)
    if false && !compact
        print(io, x) ##!!
    else
        print_tree(io, MemoTreeChildren(Dict(),x, true))
    end
end
printnode(io::IO, x::TextParse.AbstractToken) =
    print(io, "Parser::",result_type(x))
printnode(io::IO, x::FlatMap) =
    print(io, "FlatMap::",result_type(x))
function printnode(io::IO, x::NegativeLookahead)
    print(io, "not at ")
    printnode(io, x.parser)
end
printnode(io::IO, x::Sequence) =
    print(io, "seq::",result_type(x))
printnode(io::IO, x::Repeat) =
    print(io, "rep ", regex_operator(x),"::",result_type(x))
printnode(io::IO, x::Either) =
    print(io, "alt::",result_type(x))
printnode(io::IO, x::Optional) =
    print(io, "opt::",result_type(x))
function printnode(io::IO, x::NamedToken) 
    print(io, x.name, " ")
    printnode(io, x.parser)
end
    
function printnode(io::IO, x::MemoTreeChildren)
    printnode(io, x.child)
    x.descend || print(io, "(see above)")
end
function printnode(io::IO, x::InstanceParser) 
    print(io,"",result_type(x),"(", ") = ")
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
children(x::Missing) =
    ()
children(x::FlatMap) =
    [ x.left, x.right(missing) ]
children(x::InstanceParser) =
    children(x.parser)
children(x::NamedToken) =
    children(x.parser)
children(x::Repeat) where {T, F} =
    [ x.parser ]
children(x::Either) where {T, F} =
    x.options
# children(x::TokenizerOp{:greedy, T, F}) where {T, F} =
#     x.els
children(x::Optional) where {T, F} =
    children(x.parser)
children(x::Sequence) =
    x.parts

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

