############################################################
## Tokens

import InternedStrings: intern

export AbstractToken, variable, value
abstract type AbstractToken{Tt, Tv} end

variable(x::AbstractToken{Tt, Tv}) where {Tt, Tv} = nothing

value(x::AbstractToken{Tt, Tv}) where {Tt, Tv} = missing


variable_colors=Dict(
    :ext => 36,
    :macro => 36,
    :number => 36,
    :paren => :darkgray,
    Symbol("wiktionary.de") => 36,
    :meaning => :darkgray
)

# ==(x::A, y::B) where {A<:AbstractToken,B<:AbstractToken} = A==B &&
#     (value(x) === missing ? value(y) === missing : value(x)==value(y))
export value_empty
value_empty(x::Pair) = value_empty(x.second) ## needed in tryparsenext
value_empty(x::Vector) = isempty(x)
value_empty(x) = false
value_empty(::Union{Nothing,Missing}) = true
value_empty(x::String) = x==""
value_empty(x::AbstractToken) = value(x) === missing || value(x)==""

export isinformative, isvariable
isinformative(i) = false
isinformative(i::AbstractToken)  =
    !(variable(i) in [ :delimiter, :indent, :list, :enum, :whitespace ])
isvariable(i::AbstractToken)  =
    !(variable(i) in [ :literal ]) && isinformative(i)

function Base.show(io::IO, z::AbstractToken)
    color=get(variable_colors,
              Symbol(variable(z)), 36)
    if variable(z)==:literal || variable(z)==:delimiter
        value(z)!==missing && print(io,value(z))
    elseif !isinformative(z)
        printstyled(io, value(z); bold=true,
                    color=:darkgray)
    elseif value(z)===missing || value(z)==""
        printstyled(io, variable(z); bold=true,
                    color=color)
    else
        col=get(variable_colors, variable(z), missing)
        if variable(z) == :macro
            printstyled(
                io, "{{{", value(z),"}}}";
                bold=true, color=color
            )
        elseif col === missing 
            printstyled(
                io, "[[",variable(z), "][", value(z),"]]";
                bold=true, color=color
            )
        else
            printstyled(
                io, value(z);
                bold=true, color=color
            )
        end
    end
end

# ## TODO: design parallel to elm, after some 
# export Indent
# struct Block <: AbstractToken{Symbol, AbstractString}
#     name::Symbol
#     indent::AbstractString
#     function Indent(enclos::Symbol,name::Symbol, value::T) where {T<:AbstractString}
#         new(enclos, name, intern(value))
#     end
# end

export TokenPair
struct TokenPair{K,V} <: AbstractToken{K, V}
    key::K
    value::V
end
parentheses = Dict(:paren=>("(", ")"),
                   :bracket=>("[", ")"),
                   :curly=>("{", "}"),
                   :angle=>("<", ">"))
function Base.show(io::IO, z::TokenPair{Symbol,V}) where V
    open, close = get(parentheses, z.key, ("",""))
    print(io, open, z.value, close)
end
function Base.show(io::IO, z::TokenPair{String,V}) where V
    print(io, z.key, z.value, z.key)
end



export Token
struct Token <: AbstractToken{Symbol, String}
    name::Symbol
    value::String
    function Token(name::Symbol, value::T) where {T<:AbstractString}
        new(name, intern(value))
    end
end
Token(name::Symbol, value::Union{Missing, Nothing}) = Token(name, "")
Token(name::Symbol) = Token(name, "")
Token(x::Pair) = Token(x.first, x.second)
Token(x::Token) = x
function Token(name::AbstractString, value)
    Token(Symbol(name), value)
end
import Base: convert
Base.convert(::Type{Token},e::Pair) =
    Token(Symbol(e.first), e.second)

export @l_str, @ws_str, @delim_str, @T
macro T(name, value)
    Token(name, string(value))
end
macro l_str(x)
    Token(:literal, x)
end
macro ws_str(x)
    Token(:whitespace, x)
end
macro delim_str(x)
    Token(:delimiter, x)
end
ws(x) = Token(:whitespace, x)




export ReferringToken
struct ReferringToken{Tt, Tv, I} <: AbstractToken{Tt, Tv}
    name::Tt
    value::Tv
    reference::I
end
value(x::Union{Token, TokenPair, ReferringToken}) = x.value
variable(x::Union{Token, ReferringToken}) = x.name
variable(x::TokenPair) = x.key


## needed?
function Base.show(buffer::IO, ts::Array{T,1}) where { T <: AbstractToken }
    for t in ts
        show(buffer, t)
    end
end

export TokenString
const TokenString = Vector{AbstractToken{Symbol, String}}

# @deprecate TokenString(x...) tokenize(x...)

# const TokenTuple = Tuple{Vararg{Token, N} where N}
# TokenString{Tt, T} = Tuple{Vararg{Token{t,T} where {t <:Tt}, N} where N}
# TokenNest{Tv} = Tuple{Vararg{Union{TokenTuple{t},Token{t,s}} where {t, s <:Tv}, N} where N}
## TokenTuple(x::AbstractToken{Any, Tv}...) where {Tv} = x
# TokenString(x::TokenTuple) = x



export Line, Paragraph, Body

struct Line{I,T}
    indent::Vector{I}
    tokens::Vector{T}
end
function Line(indent::Vector{I}, t::Vector{T}, newline::AbstractString) where {I,T}
    Line{I,T}(
        indent,
        vcat(t, Token(:whitespace, newline)))
end
import Base: ==, hash
==(a::Line, b::Line) = a.indent==b.indent && a.tokens== b.tokens
hash(x::Line, h::UInt) = hash(x.indent, hash(x.tokens))
import Base: convert
Base.convert(::Type{Line{I,T}}, x::Line{J,S}) where {I,J,S,T} =
    Line(convert(Vector{I}, x.indent), convert(Vector{T}, x.tokens))

function Base.show(io::IO, i::Line{I,T}) where {I,T}
    if !isempty(i.indent) && variable(i.indent[1]) == :headline
        level = parse(Int, value(i.indent[1]))
        wikihead = repeat("=", level)
        print(io, wikihead, " ")
        tail = Token[]
        for x in i.tokens
            if !isequal(value(x), "\n")
                print(io, x)
            else
                push!(tail,x)
            end
        end
        print(io, wikihead)
        for x in tail
            print(io, x)
        end 
    else
        for x in i.indent
            print(io, x.value === missing ? "" : x)
        end
        print(io, i.tokens)
    end
end
Paragraph{I,T} = Vector{Line{I,T}}
Paragraph(x::Paragraph) = x
Base.show(io::IO, v::Type{<:Paragraph})  = print(io, "Paragraph")
## Base.show(io::IO, v::Type{Paragraph{T}}) where T = print(io, "Paragraph{$T}")
Base.show(io::IO, v::Vector{<:Line}) =
    for x in v
        print(io,x)
    end

Body{I,T} = Vector{Paragraph{I,T}}
Base.show(io::IO, v::Type{Body})  = print(io, "Body")
## Base.show(io::IO, v::Type{Body{T}}) where T = print(io, "Body{$T}")
Base.show(io::IO, v::Body) =
    for x in v
        print(io,x)
    end
# Base.show(io::IO, m::MIME"text/markdown", x::Token) =
#     if x.name in [ :literal, :delimiter, :whitespace ]
#         print(io, x.value)
#     else
#         print(io,"""[$(x.value)]($(x.name) "$(x.name)")""")
#     end
# Base.show(io::IO, m::MIME"text/markdown", x::Line) = println(io,m,x.indent,x.tokens...)
# Base.show(io::IO, m::MIME"text/markdown", x::Vector{Line}) = println(io,m,x...)
# Base.show(io::IO, m::MIME"text/markdown", x::Vector{Vector{Line}}) = println(io,m,x...)

export Template, LineContent
TemplateArgument{I,T} = Pair{String,Vector{Line{I,T}}}
struct Template{I,T} <: AbstractToken{Symbol, Vector{TemplateArgument{I,T}}}
    template::String
    arguments::Vector{TemplateArgument{I,T}}
    Template(t,a::Vector{TemplateArgument{I,T}}) where {I,T} =
        new{I,T}(intern(t),[ intern(k) => v for (k,v) in a])
end
Template(a::String) = Template(a,TemplateArgument{Token,LineContent}[])
==(a::Template, b::Template) = a.template==b.template && a.arguments==b.arguments
hash(x::Template, h::Template) = hash(x.template, hash(x.arguments))

function Base.show(io::IO, x::Template) where T 
    print(io, "{{")
    print(io, x.template)
    for a in x.arguments
        print(io, "|")
        if a isa Pair
            if !isempty(a.first)
                print(io, a.first,"=",a.second)
            else
                print(io, a.second)
            end
        else
            print(io, a)
        end        
    end
    print(io, "}}")
end

LineContent = Union{Token, Template, TokenPair}
function Base.show(buffer::IO, ts::Vector{<:LineContent};
                   vals=Dict())  #::Array{Array{
    for t in ts
        show(buffer, t)
    end
end

emptyLine(x::Vararg{T}) where T = Line{T,T}([ Token(:whitespace,"") ],
                                   T[x...])


export tokens
tokens(x::Vector{<:Union{AbstractToken, AbstractString, Symbol}}) =
    Iterators.repeated(1  => x, 1)
tokens(x::Union{Number,Symbol, AbstractString, AbstractToken}) =
    Iterators.repeated(1 => Iterators.repeated(x, 1), 1)
## tokens(d::Dict{Symbol,<:AbstractString}) =
##     [ 1 => [ Token(x.first, x.second) for x in d ] ]
tokens(d::Dict) =
    [] #Iterators.flatten( tokens(x.second) for x in d )
tokens(x::Vector) =
    Iterators.flatten(tokens(y) for y in x)

function tokens(n::NamedTuple{names,t}) where {names,t}    
    val(field) = tokens(getproperty(n,field))
    R = Iterators.flatten( val(field)
                           for field in names
                           # if !isempty(val(field))
                           )
    R
    # Pair[ w => collect(v) for (w,v) in R ]    
end

function tokens(n::T) where {T}
    val(field) = tokens(getfield(n,field))
    R = Iterators.flatten( val(field)
                           for field in fieldnames(T)
                           # if !isempty(val(field))
                           )
    R
    # Pair[ w => collect(v) for (w,v) in R ]    
end
