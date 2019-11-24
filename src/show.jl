
function Base.show(io::IO, x::InstanceParser{P,T}) where {P,T}
    compact = get(io, :compact, false)
    if false && !compact
        print_tree(io, x.value) ##!!
    else
        print_tree(io,x)
    end
end





import AbstractTrees: print_tree, children, printnode
# Base.show(io::IO, x::TokenizerOp) =
#     print_tree(io, x)
# function Base.show(io::IO, x::TextParse.AbstractToken{T}) where {T}
#     compact = get(io, :compact, false)
#     if false && !compact
#         print(io, x) ##!!
#     else
#         print_tree(io, MemoTreeChildren(Dict(),x, true))
#     end
# end
printnode(io::IO, x::TextParse.AbstractToken{T}) where {T} =
    print(io, "$T = ", x)
printnode(io::IO, x::TokenizerOp{op, T, F}) where {op, T, F} =
    print(io, "$T = $op")
function printnode(io::IO, x::TokenizerOp{:opt, T, F}) where {T, F}
    print(io, "$T = opt ")
    printnode(io, x.els.parser)
end
function printnode(io::IO, x::TokenizerOp{:tokenize, T, F}) where {T, F}
    print(io, "$T = tokenize ")
    printnode(io, x.els.outer)
end
function printnode(io::IO, x::NamedToken{P, T}) where {P, T} 
    print(io, x.name, "::")
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
children(x::TokenizerOp{:seq, T, F}) where {T, F} =
    x.els.parts
children(x::TokenizerOp{:tokenize, T, F}) where {T, F} =
     [ x.els.parser ]

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


Base.show(io::IO, x::Tuple{Nullable,Int}) =
    !isnull(x[1]) &&  show(io, x[1].value)
