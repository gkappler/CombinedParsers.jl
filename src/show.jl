
struct MemoTreeChildren{P}
    visited::Dict
    child::P
    descend::Bool
end



function Base.show(io::IO, x::TextParse.AbstractToken)
    compact = get(io, :compact, false)
    if false && !compact
        print(io, x) ##!!
    else
        print_tree(io, MemoTreeChildren(Dict(),x, true))
    end
end
function printnode(io::IO, x::ParserPeek)
    print(io, x.message)
end
function printnode(io::IO,x::Lazy)
    print(io,"lazy ")
    printnode(io,x.parser)
end
printnode(io::IO,x::ConstantParser) =
    print(io,regex_string(x.parser))
printnode(io::IO,x::CharIn) =
    print(io,regex_string(x))
printnode(io::IO, x::Always) =
    print(io, "always::Nothing")
printnode(io::IO, x::AnyChar) =
    print(io, regex_string(x))
printnode(io::IO, x::CharNotIn) =
    print(io, regex_string(x))
printnode(io::IO, x::TextParse.AbstractToken) =
    print(io, "Parser::",result_type(x))
printnode(io::IO, x::FlatMap) =
    print(io, "FlatMap::",result_type(x))
printnode(io::IO, x::Sequence) =
    print(io, "seq::",result_type(x))
printnode(io::IO, x::Repeat) =
    print(io, "rep ", rep_suffix(x),"::",result_type(x))
printnode(io::IO, x::Either) =
    print(io, "alt::",result_type(x))
function printnode(io::IO, x::Optional)
    print(io, "(")
    printnode(io,x.parser)
    print(io,")? || ",x.default)
end
function printnode(io::IO, x::NamedParser) 
    print(io, x.name, " ")
    printnode(io, x.parser)
end
    
function printnode(io::IO, x::MemoTreeChildren)
    printnode(io, x.child)
    x.descend || isempty(children(x)) || print(io, " (see at higher level)")
end
function printnode(io::IO, x::Transformation) 
    print(io,"map(")
    printnode(io, x.parser)
    print(io,")::",result_type(x))
end

function MemoTreeChildren(children::Union{Vector, Tuple}, visited::Dict=Dict())
    children_ = [ MemoTreeChildren(visited, x, !haskey(visited, x)) for x in children ]
    for c in children
        visited[c] = true
    end
    children_
end
printnode(io::IO,x::WrappedParser) = printnode(io,x.parser)
children(x::WrappedParser) = (x.parser,)
printnode(io::IO,x::PositiveLookbehind) = print(io,"(?<=")
printnode(io::IO,x::NegativeLookbehind) = print(io,"(?<!")
printnode(io::IO,x::PositiveLookahead) = print(io,"(?=")
printnode(io::IO,x::NegativeLookahead) = print(io,"(?!")
printnode(io::IO,x::AtomicGroup) = print(io,"(?>")
children(x::LookAround) = (x.parser,)
children(x::Always) = tuple()

children(x::MemoTreeChildren) =
    x.descend ?  MemoTreeChildren(children(x.child), x.visited ) : []

children(x::Union{Regex,AbstractString}) =
    ()
children(x::Missing) =
    ()
children(x::FlatMap) =
    [ x.left, x.right ]
children(x::Transformation) =
    children(x.parser)
children(x::NamedParser) =
    children(x.parser)
children(x::Repeat) =
    [ x.parser ]
children(x::Either) =
    x.options
# children(x::TokenizerOp{:greedy, T, F}) where {T, F} =
#     x.els
children(x::Optional) =
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

