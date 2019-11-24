"""
Transform a Vector{Line} with path prefix into its nested representation.

isappendable compares prefix tokens of a line to the token in nested representation.
(This allows e.g. for appending indented lines into list prefixes.)

agg is, when 2-ary, an aggregation function(::Vector[Any],x)::Vector[Any];
        when 1-ary, an init function (x)::Vector[Any]


"""
function pushtail_nested!(
    tree::Vector{Any}, path::Vector, x;
    isappendable=(l1,l2)->l1==l2, agg)
    ## TODO: factor out aggregation into transformation of a nest
    ## (useful in reassembling token_lines)
    if !isempty(tree) && tree[end] isa Pair && isappendable(tree[end].first,path)
        if length(path)==1
            agg(tree[end].second, x)
        else
            pushtail_nested!(
                tree[end].second, path[2:end], x;
                isappendable=isappendable, agg=agg)
        end
    else
        if length(path)==0
            ##push!(tree, Token(:root,"remove?") => agg(x) )
            push!(tree, agg(x) )
        elseif length(path)==1
            push!(tree, NamedString(path[1]) => agg(x) )
        else
            push!(tree, NamedString(path[1]) => pushtail_nested!(Any[], path[2:end], x;
                                                    isappendable=isappendable, agg=agg))
        end
    end
    tree
end
pushtail_nested!(tree::Vector{Any}, path::LinePrefix, a...; kw...) =
    pushtail_nested!(tree, path.prefix, a...; kw...)
    

# function append_tokens(x,y::Line)
#     @show x,y 
#     pushtail_nested!(x,y.prefix,y.tokens; isappendable=(l1,l2)->false, agg=append_tokens)
# end
@enum IndentStrategy  NewParagraph NewIndent Append
function isappendable_line(l1,l2)
    isempty(l2) && return false
    l1==l2[1] || 
        variable(l2[1]) == :whitespace &&
        variable(l1) in [ :whitespace, :list ] &&
        lastindex(value(l1)) == lastindex(value(l1))
end

copy1_append!2(x)=Any[x]
copy1_append!2(x,y)=push!(x,y) ## append!(x,y)

export nested
function nested(v::Paragraph; isappendable=isappendable_line, agg=copy1_append!2)
    tree = Any[]
    for x in v
        pushtail_nested!(tree, x.prefix, x.tokens; isappendable=isappendable, agg=agg)
    end
    tree
end

function nested(v::Body; isappendable=isappendable_line, agg=copy1_append!2)
    tree = Any[]
    for x in v
        pushtail_nested!(tree, x.prefix, x.tokens; isappendable=isappendable, agg=agg)
    end
    tree
end


# function pushtail_nested(v)
#     string_(x)=join(string.(x))
#     string_(x,y)=x*string_(y)
#     pushtail_nested(v; isappendable=isappendable_line, agg=copy1_append!2)
# end

export append_tokens
"""
Replace all T <: AbstractToken elements that are T <: Token with 
"""
append_tokens(typenames::TypeNames) =
    (a...) -> append_tokens(a..., typenames)

append_tokens(v::Vector{Any}, x, typenames::TypeNames) = 
    push!(v, append_tokens(x,typenames))

append_tokens(v::Vector, typenames::TypeNames) = 
    Any[ append_tokens(x, typenames) for x in v ]

function append_tokens(v::Vector{AbstractToken}, typenames::TypeNames)
    out = Any[]
    li=0
    for (i,t) in enumerate(v)
        if !(t isa Token)
            li!=0 && push!(out, v[li:i-1])
            push!(out, append_tokens(v[i], typenames))
            li=0
        elseif li==0
            li=i
        end
    end
    li!=0 && push!(out, v[li:end])
    out
end

append_tokens(v::Line{Token,AbstractToken}, typenames::TypeNames) =
    isempty(v.prefix) ? append_tokens(v.tokens, typenames) : NamedString(v.prefix[1]) => append_tokens(Line(v.prefix[2:end],v.tokens), typenames)

append_tokens(t::Token, typenames::TypeNames) = t
function append_tokens(t, typenames::TypeNames)
    if isempty(propertynames(t))
        ## todo: check if string, etc?
        Token(typenames[typeof(t)], ## todo: struct TypeNames (TableAlchemy, GraphQLAlchemy)
              "$t")
    else
        # [
        NamedString(:type, "$(typenames[typeof(t)])") =>
            Any[ NamedString(:field, "$f") => append_tokens(getproperty(t, f), typenames)
                 for f in propertynames(t)
                 ]
        # ]
    end
end


# token_typenames(a...) =
#     TypeNames(Node{Line{Token,AbstractToken}} => :Node,
#               Template{Token,AbstractToken} => :Template,
#               TokenPair{Symbol,Vector{AbstractToken}} => :Wrapped,
#               String => :String,
#               Symbol => :Symbol,
#               a...)

export nested_tokens, token_lines, nested_wrap_types
nested_tokens(x; isappendable=isappendable_line, typenames) =
    nested(x; isappendable=isappendable, agg=append_tokens(typenames))


wrap_convert(T::Type, x::Vector{Any}) =
    if length(x)==1
        wrap_convert(T, x[1])
    else
        dump(x)
        error("cannot convert $x to $T")
    end

wrap_convert(::Type{Vector{T}}, v::Vector{Any}) where T =
    T[ wrap_convert(T,x) for x in v ]

function wrap_convert(::Type{Vector{AbstractToken}}, v::Vector{Any})
    r = AbstractToken[]
    for x in v
        if x isa AbstractToken
            push!(r,x)
        elseif x isa Vector && eltype(x) <: AbstractToken
            append!(r, x)
        else
            error("cannot insert $(typeof(x)) to Vector{AbstractToken}")
        end
    end
    r
end

BasePiracy._convert(::Type{Token}, x::NamedString) =
    Token(variable(x), value(x))

function wrap_convert(::Type{Vector{Line{I,T}}}, v::Vector{Any}) where {I,T}
    r = Line{I,T}[]
    for x in v
        if x isa Vector
            if eltype(x) <: T
                push!(r, Line(I[],x))
            else
                inner=wrap_convert(Vector{Line{I,T}}, x)
                append!(r, inner)
            end
        elseif x isa Pair
            inner=wrap_convert(Vector{Line{I,T}}, x.second)
            for y in inner
                pushfirst!(y.prefix, _convert(I,x.first))
            end
            append!(r, inner)
        elseif x isa T
            if isempty(r)
                push!(r, Line(I[],T[x]))
            else
                push!(r[end].tokens, x)
            end
        else
            error("cannot insert $x::$(typeof(x)) to Vector{Line}")
        end
    end
    r
end


wrap_convert(::Type{T}, x::T) where T = x

wrap_convert(T::Type, x::Vector{Token}) =
    if length(x)==1
        wrap_convert(T, x[1])
    else
        error("cannot convert $x to $T")
    end

wrap_convert(::Type{String}, x::Token) =
    if variable(x)==:String
        value(x)
    else
        error("cannot convert $x to $T")
    end

wrap_convert(::Type{Symbol}, x::Token) =
    if variable(x)==:Symbol
        Symbol(value(x))
    else
        error("cannot convert $x to $T")
    end

## nested_wrap_types(x; typenames) = nested_wrap_types(x; typenames=typenames)
nested_wrap_types(x::Union{Integer,String};kw...) = x
nested_wrap_types(v::Vector{AbstractToken}; kw...) =
    v
nested_wrap_types(v::Vector{Token}; kw...) =
    v
nested_wrap_types(v::Vector{Any}; kw...) =
    Any[ nested_wrap_types(x; kw...) for x in v ]

nested_wrap_types(x::Token; typenames, kw...) =
    if haskey(typenames, variable(x))
        _convert(typenames[variable(x)],(value(x)))
    else
        x
    end
nested_wrap_types(x::AbstractToken; kw...) = x
function nested_wrap_types(b::Pair; kw...)
    l, inner = b
    nested_wrap_types(NamedString(l), inner; kw...)
end
nested_wrap_types(l::NamedString{:whitespace}, inner; typenames, kw...) =
    l => nested_wrap_types(inner; typenames=typenames, kw...)

nested_wrap_types(l::NamedString{:list}, inner; typenames, kw...) =
    l => nested_wrap_types(inner; typenames=typenames, kw...)


function nested_wrap_types(l::NamedString{:type}, inner; typenames, kw...)
    T=typenames[Symbol(value(l))]
    f=nested_wrap_types(inner; typenames=typenames, type=T, kw...)
    try
        construct(T; f...)
    catch e
        @error "cannot create $T" inner e
        rethrow(e)
    end
end
function nested_wrap_types(l::NamedString{:field}, inner; typenames, type, kw...)
    f = Symbol(value(l))
    FT = _fieldtype(type,f)
    f => wrap_convert(FT, nested_wrap_types(inner; typenames=typenames, kw...))
end

## remove, use termination token
function nested_wrap_types(l::NamedString{:index}, inner; typenames, kw...)
    nested_wrap_types(inner; typenames=typenames, kw...)
end

token_lines(x::Paragraph; kw...) =
    unnest_lines(Token, nested_tokens(x; kw...))


export unnest_lines
unnest_lines(x) = unnest_lines(Line{NamedString,AbstractToken}[],x, tuple())
unnest_lines(T::Type{<:AbstractToken},x) = unnest_lines(Line{NamedString,T}[],x, tuple())
# function unnest_lines(io::Vector{Line{NamedString,Token}},x)
#     push!(io.tokens,x)
#     io
# end
function unnest_lines(io::Vector{Line{NamedString,T}},x::Vector{AbstractToken}, path) where {T<:AbstractToken}
    push!(io,Line(path,x))
    io
end
function unnest_lines(io::Vector{Line{NamedString,T}},x::T, path) where {T<:AbstractToken}
    push!(io,Line(path,[x]))
    io
end
function unnest_lines(io::Vector{Line{NamedString,T}},x::Line{NamedString,T}, path) where {T<:AbstractToken}
    push!(io,Line(vcat(path, x.prefix),x.tokens))
    io
end
function unnest_lines(io::Vector{Line{NamedString,T}},tree::Vector, path) where {T<:AbstractToken}
    l::Any = nothing
    for (i,b) in enumerate(tree)
        if b isa Pair ## todo: this could be dne better
            unnest_lines(io, b, l == b.first ? ( tuple(path..., NamedString(:index,"$i"))) : path)
            l = b.first
        else
            unnest_lines(io, b, path)
            l = nothing
        end
    end
    io
end
function unnest_lines(io::Vector{Line{NamedString,T}}, b::Pair, path) where {T<:AbstractToken}
    unnest_lines( io, b.second, (path..., b.first) )
end


export print_org_tree
print_org_tree(x) = print_org_tree(stdout,x)

print_org_tree(io::IO, x) =
    print(io,x)
print_org_tree(io::IO, x::AbstractToken) =
    x != Token(:whitespace, "\n") && print(io,x)
function print_org_tree(io::IO, tree::Vector)
    for b in tree
        print_org_tree(io, b)
    end
end
function print_org_tree(io::IO, b::Pair)
    l, inner = b
    print_org_tree(io::IO, NamedString(l), inner)
end
function print_org_tree(io::IO, b::NamedString{:name}, inner)
    print(io, "#+name: ", value(l), "\n")
    print_org_tree(io, inner)
end
function print_org_tree(io::IO, b::NamedString{:orgblock}, inner)
    print(io, "#+begin_", value(l), "\n")
    print_org_tree(io, inner)
    print(io,  "#+end_", value(l), "\n")
end
function print_org_tree(io::IO, b::NamedString{:orgdrawer}, inner)
    print(io, ":", value(l), ":\n")
    print_org_tree(io, inner)
    print(io, ":END:\n" )
end
# function print_org_tree(io::IO, b::NamedString{:inline}, inner)
#     print(io, ":", value(l), ":\n")
#     print_org_tree(io, inner)
# end
# function print_org_tree(io::IO, b::NamedString{:inline}, inner)
#     print_org_tree(io, inner)
# end



