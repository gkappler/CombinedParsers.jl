
using AbstractTrees
import AbstractTrees: children
import AbstractTrees: print_tree, printnode
import AbstractTrees: print_child_key
@nospecialize



struct ChainableTree{T,F<:Function}
    can_collapse::F
    tree::T
    ChainableTree(t) =
        new{Any, typeof(can_collapse)}(can_collapse, t)
    ChainableTree(f::Function,t) =
        new{Any, typeof(f)}(f, t)
end
function AbstractTrees.children(μ::ChainableTree)
    t = μ.tree
    ch = children(t)
    while length(ch) == 1 && μ.can_collapse(t)
        t = ch[1]
        ch = children(t)
    end
    [ ChainableTree(μ.can_collapse, c) for c in ch ]
end

AbstractTrees.childrentype(::Type{ChainableTree{T}}) where {T} = childrentype(T)

AbstractTrees.nodevalue(μ::ChainableTree) = nodevalue(μ.tree)

AbstractTrees.ChildIndexing(::ChainableTree) = IndexedChildren()

AbstractTrees.printnode(io::IO,x::ChainableTree;kw...) = printnode(io,nodevalue(x);kw...)



"""
    DeepMapNode{T,C}

A node in a tree which is returned by [`treemap`](@ref).  It consists of a value which is the result of the function
call and an array of the children, which are also of type `DeepMapNode`.

Every `DeepMapNode` is itself a tree with the [`IndexedChildren`](@ref) trait and therefore supports indexing via
[`getdescendant`](@ref).

Use [`AbstractTrees.nodevalue`](@ref) or `mapnode.value` to obtain the wrapped value.
"""
struct DeepMapNode{T,C} <: AbstractNode{T}
    value::T
    children::C
    
    function DeepMapNode(f, t, a...; cache, kw...)
        t´, ch´´ = if t isa Union{<:Either{<:Vector}} && haskey(cache,t)
            if cache[t] isa RecursionMarker
                f(nodevalue(t), tuple(), a...;kw...)
            else
                cache[t]
            end
        else
            ch = children(t)
            cache[t] = RecursionMarker()
            ch´ = [ tree_deepmap(f, c, a...; cache=cache,kw...) for c in ch ]
            f(nodevalue(t), ch´,a...;kw...)
        end
        cache[t] = (t´, ch´´)
        new{Any, typeof(ch´´)}(t´, ch´´)
    end
end
DeepMapNode(node) = DeepMapNode(n -> (nodevalue(n), children(n)), nodevalue(node))

AbstractTrees.children(μ::DeepMapNode) = μ.children

AbstractTrees.childrentype(::Type{DeepMapNode{T,C}}) where {T,C} = C

AbstractTrees.nodevalue(μ::DeepMapNode) = μ.value

AbstractTrees.ChildIndexing(::DeepMapNode) = IndexedChildren()

AbstractTrees.printnode(io::IO,x::DeepMapNode;kw...) = printnode(io,nodevalue(x);kw...)

export tree_deepmap
tree_deepmap(f, t, a...; cache=IdDict{Any,Any}(), kw...) =
    DeepMapNode(f, t, a...; cache=cache, kw...)



can_collapse(x::FlatMap) = false
can_collapse(x::DeepMapNode) = can_collapse(nodevalue(x))
can_collapse(x::Union{WrappedParser,WrappedAssertion}) = true
#can_collapse(x::NamedParser) = false
can_collapse(x::Union{Sequence, Either, LeafParser}) = false



# children(x::CombinedParser, visited::Dict) =
#     children(x)
children(x::WrappedAssertion) = Any[x.parser]
#     children(x.parser, visited)
children(x::WrappedParser) = Any[x.parser]
#children(x::PositiveLookbehind) =  children(x.parser)
#children(x::WrappedAssertion) = children(x.parser)
# children(x::NamedParser)     = x.doc=="" ? children(x.parser)     : tuple()
#children(x::Optional) = children(x.parser)
#children(x::Transformation) = children(x.parser)
children(x::FlatMap) = Any[ x.left, x.right ]
children(x::Sequence) = isliteralsequence(x) ? [] : Any[x.parts...]
children(x::Either) = Any[x.options...]
children(x::Either{<:AbstractTrie}) =  children(x.options)

children(x::Union{LeafParser, ConstantParser,Never,Always}) = Any[]
children(x::Union{PositiveLookbehind,NegativeLookbehind}) =
    reverse(children(x.parser))

children(x::MappedSequenceParser) = Any[x.parser, x.f]

# function print_constructor(io::IO,x::FlatMap)
#     print(io, "FlatMap" )
# end
# print_constructor(io::IO,x::Sequence) =
#     if !get(io,:compact, false)
#         printstyled(io,"Sequence"; color=treecolor.julia_structure)
#     else
#         printstyled(io,"Sequence"; color=treecolor.julia_structure)
#     end

# function print_constructor(io::IO, x::Lazy)
#     print_constructor(io,x.parser)
#     print_pipe(io)
#     print(io, "Lazy" )
# end
# function print_constructor(io::IO,x::Repeat)
#     if !get(io,:compact, false)
#         print_constructor(io,x.parser)
#     end
# end
# function print_constructor(io::IO, x::Optional)
#     if !get(io,:compact, false)
#         printstyled(io, "Optional",color=treecolor.julia_structure)
#         printstyled(io, " $(x.default)",color=treecolor.julia)
#     end
#     #print(io, " |> Optional(default=$(x.default))")
# end

# """
# decurse recursive patterns
# """
# struct HiddenChildren{P}
#     tree::P
# end
# function printnode(io::IO, x::HiddenChildren)
#     printnode(io, x.tree)
#     isempty(children(x.tree)) || printstyled(io, " # $children_char branches hidden", color=treecolor.comment)
# end
# children(x::HiddenChildren) =
#     tuple()

children_char = "▽ " #"\U251C" 
treecolor = (
    name            = 227, #:light_yellow,
    reference       = 144, 
    julia_structure = 250, #:green,
    textparse       = 75, # :light_blue,
    pcre            = 45, # :light_white,#light_cyan,
    pcre_name       = 227, 
    pcre_index      = 227, 
    pcre_Capture    = 227, 
    pcre_Backreference    = 227, 
    pcre_Subroutine    = 227, 
    pcre_charrange  = 195, 
    pcre_matcher    = 195, 
    pcre_Matcher    = 195,#:cyan,
    match_function    = 45, 
    pcre_unescaped  = 15, 
    pcre_escaped    = 225, 
    pcre_escape     = 238, 
    pcre_structure  = 221, #:light_green,
    #
    constructor     = 23,
    pcre_options    = 24, 
    pcre_Assertion  = 251, #:white,
    pcre_NegativeLookbehind  = 216, #:white,
    pcre_NegativeLookahead  = 217, #:white,
    pcre_PositiveLookbehind  = 158, #:white,
    pcre_PositiveLookahead  = 159, #:white,
    pcre_Either     = 69, 
    pcre_Atomic    = 31, 
    pcre_Repeat     = 117, #:light_green,
    pcre_Optional   = 116, #:light_green,
    pcre_Sequence   = 67, 
    #pcre_Capture    = 32,#:cyan,
    match           = :bold,
    julia           = 38,
    pipe            = :light_black, 
    children_char   = :light_black,
    map             = :light_black,
    type            = :light_black,
    comment         = :light_black)


"""
decurse recursive patterns
"""
struct MemoTree{P}
    tree::P
    visited::Dict{Any,Bool}
    descend::Bool
    params::IdDict{Symbol,Any}
end
MemoTree(x; kw...) =
    MemoTree{typeof(x)}(x,Dict{Any,Bool}(x=>true), true, IdDict{Symbol,Any}(kw...))

function children(x::MemoTree)
    if x.descend
        memochildren(x.tree, x)
    else
        # @info "no descend"
        Any[]
    end
end
can_collapse(x::MemoTree) = can_collapse(x.tree)

memochildren(x, root) =  memochildren(children(x), root)

function memochildren(children::Union{Vector, Tuple}, root)
    children_ = Any[]
    for x in children
        push!(children_,
              MemoTree{Any}(
                  x, root.visited, 
                  !haskey(root.visited, x),
                  root.params
              ))
        x isa Union{NamedParser} && (root.visited[x] = true)
    end
    children_
end
Base.show(io::IO, x::MemoTree) =
    show(io,x.tree)





# function printregex(io::IO, x::CombinedParser)
#     printstyled(io, regex_prefix(x), color=treecolor.pcre_structure)
#     printstyled(io, regex_inner(x), color=treecolor.pcre)
#     printstyled(io, regex_suffix(x), color=treecolor.pcre_structure)
# end
# function printregex(io::IO, x::SubTrie)
#     printstyled(io, regex_inner(nodevalue(x)), color=treecolor.pcre)
# end
#printregex(io::IO, x) = show(io,x)

# function print_regex_fixes(io, x)
#     trail = Any[x]
#     while length(children(trail[end])) == 1 && can_collapse(trail[end])
#         push!(trail, children(trail[end])[1])
#     end
#     for (i,e) in enumerate((trail))
#         printstyled(io, regex_prefix(e), color=treecolor.pcre_structure)
#     end
#     for (i,e) in enumerate((trail))
#         printstyled(io, regex_inner(e), color=treecolor.pcre)
#     end
#     for (i,e) in enumerate(reverse(trail))
#         printstyled(io, regex_suffix(e), color=treecolor.pcre_structure)
#     end
# end


function print_constructors(io, x; kw...)
    trail = Any[x]
    while length(children(trail[end])) == 1 && can_collapse(trail[end])
        push!(trail, children(trail[end])[1])
    end
    for (i,e) in enumerate(filter( e -> e isa Transformation ? false : true, reverse(trail)))
        i > 1 && print_pipe(io)
        print_constructor(io, e; kw...)
    end
end


function printnode(io::IO, x::CombinedParser; kw...)
    print_regex(io, x)
    printstyled(io, " ")
    print_constructors(io,x; kw...) # , color=:yellow)
end

function printnode(io::IO, x_::MemoTree; kw...)
    x = x_.tree
    if !x_.descend && x isa NamedParser
        printstyled(io, "see ", color = treecolor.comment)
        printstyled(io, x.name, color = treecolor.reference)
        #printstyled(io, " # $children_char branches hidden", color=treecolor.comment)
    else
        printnode(io,x; kw...)
        if !get(io,:compact, false)
            printstyled(io,"::$(result_type(x))"; color=treecolor.type)
        end
    end
end


function print_child_key(io::IO, x::CombinedParser)
    printnode(io, x)
end
@specialize
