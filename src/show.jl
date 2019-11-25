

import AbstractTrees: print_tree, children, printnode
function Base.show(io::IO, x::TextParse.AbstractToken)
    compact = get(io, :compact, false)
    if false && !compact
        print(io, x) ##!!
    else
        print_tree(io, MemoTreeChildren(Dict(),x, true))
    end
end
printnode(io::IO, x::TextParse.AbstractToken{T}) where {T} =
    print(io, "Parser::$T")
printnode(io::IO, x::Sequence{T}) where {T} =
    print(io, "seq::$T")
printnode(io::IO, x::TokenizerOp{op, T}) where {op, T} =
    print(io, "$op::$T")
function printnode(io::IO, x::NamedToken{P, T}) where {P, T} 
    print(io, x.name, " ")
    printnode(io, x.parser)
end
    
function printnode(io::IO, x::MemoTreeChildren{P}) where {P}
    printnode(io, x.child)
    x.descend || print(io, "(see above)")
end
function printnode(io::IO, x::InstanceParser{P,T}) where {P,T} 
    print(io,"",T,"(", ") = ")
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
children(x::TokenizerOp{:tokenize, T, F}) where {T, F} =
     [ x.els.parser ]
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

