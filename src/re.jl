
function printopts(io,opts)
    if (opts & Base.PCRE.CASELESS ) != 0; print(io, 'i'); end
    if (opts & Base.PCRE.MULTILINE) != 0; print(io, 'm'); end
    if (opts & Base.PCRE.DOTALL   ) != 0; print(io, 's'); end
    if (opts & Base.PCRE.EXTENDED ) != 0; print(io, 'x'); end
    if (opts & Base.PCRE.DUPNAMES   ) != 0; print(io, 'J'); end
    if (opts & Base.PCRE.UNGREEDY ) != 0; print(io, 'U'); end
    ##if (opts & Base.PCRE.UCP      ) == 0; print(io, 'a'); end
end

export regex_escape
## https://github.com/JuliaLang/julia/pull/29643/commits/dfb865385edf19b681bc0936028af23b1f282b1d
"""
        regex_escape(s::AbstractString)
    regular expression metacharacters are escaped along with whitespace.
    # Examples
    ```jldoctest
    julia> regex_escape("Bang!")
    "Bang\\!"
    julia> regex_escape("  ( [ { . ? *")
    "\\ \\ \\(\\ \\[\\ \\{\\ \\.\\ \\?\\ \\*"
    julia> regex_escape("/^[a-z0-9_-]{3,16}\$/")
    "/\\^\\[a\\-z0\\-9_\\-\\]\\{3,16\\}\\\$/"
    ```
    """
function regex_escape(s)
    res = replace(string(s), r"([()[\]{}?*+\-|^\$\\.&~#\s=!<>|:])" => s"\\\1")
    replace(res, "\0" => "\\0")
end
export regex_string
regex_string(x::AbstractString) = regex_escape(x)
regex_string_(x::AbstractString) = regex_escape(x)
regex_string(::TextParse.Numeric{<:Integer}) = "-?[[:digit:]]+"

struct WithOptions{S}
    x::S
    flags::UInt32
end
Base.getindex(x::WithOptions,i) =
    WithOptions(x.x[i],x.flags)
Base.lastindex(x::WithOptions) =
    lastindex(x.x)
Base.firstindex(x::WithOptions) =
    firstindex(x.x)
Base.prevind(x::WithOptions,i::Integer,n::Integer) =
    prevind(x.x,i,n)
Base.nextind(x::WithOptions,i::Integer,n::Integer) =
    nextind(x.x,i,n)
Base.prevind(x::WithOptions,i::Integer) =
    prevind(x.x,i)
Base.nextind(x::WithOptions,i::Integer) =
    nextind(x.x,i)
Base.ncodeunits(x::WithOptions) =
    ncodeunits(x.x)
function ==(x::Char,y::WithOptions{Char})
    if !iszero(y.flags & Base.PCRE.CASELESS)
        lowercase(x)==lowercase(y.x) || uppercase(x)==uppercase(y.x)
    else
        x == y.x
    end
end
Base.iterate(x::WithOptions{<:AbstractString},a...) =
    let n = iterate(x.x,a...)
        n===nothing ? nothing : convert(Char,WithOptions(n[1],x.flags)),n[2]
    end

import Base: Regex
Base.Regex(x::WithOptions) =
    let sio = IOBuffer()
        printopts(sio, x.flags)
        Regex(x.x, String(take!(sio)))
    end

# Base.isless(x::Char, y::WithOptions{Char}) =
#     isless(x,convert(Char,y))

# Base.isless(x::WithOptions{Char}, y::Char) =
#     isless(convert(Char,x),y)

import Base: convert
function Base.convert(::Type{Char},y::WithOptions{Char})
    if !iszero(y.flags & Base.PCRE.CASELESS)
        lowercase(y.x)
    else
        y.x
    end
end
Base.show(io::IO,x::WithOptions) =
    print(io,x.x)

with_options(flags::UInt32,x::WithOptions) =
    flags == 0 ? with_options(UInt32(0),x.x) : WithOptions(x.x,flags)

with_options(flags::UInt32,x) =
    flags == 0 ? x : WithOptions(x,flags)

function Base.in(x::WithOptions,set)
    if !iszero(x.flags & Base.PCRE.CASELESS)
        lowercase(x.x) in set || uppercase(x.x) in set
    else
        x.x in set
    end
end

export set_options, with_options, on_options, instance
struct ParserOptions{P,T} <: WrappedParser{T}
    parser::P
    set_flags::UInt32
    unset_flags::UInt32
    ParserOptions(parser,set::UInt32,unset::UInt32) =
        new{typeof(parser),result_type(parser)}(parser,set,unset)
end
map_parser(f::Function,x::ParserOptions,a...) =
    ParserOptions(
        map_parser(f,x.parser,a...),
        x.set_flags,x.unset_flags)

set_options(set::UInt32,unset::UInt32,parser) =
    ParserOptions(parser,set,unset)

set_options(set::UInt32,parser) =
    set_options(set,UInt32(0),parser)

function printnode(io::IO,x::ParserOptions)
    print(io,"(?")
    printopts(io,x.set_flags)
    if x.unset_flags!=0
        print(io,"-")
        printopts(io,x.unset_flags)
    end
    print(io,")")
    printnode(io,x.parser)
end

@inline function _iterate(parser::ParserOptions, sequence::WithOptions, till, i, state)
    _iterate(parser.parser, WithOptions(sequence,parser.set_flags | (sequence.flags & ~parser.unset_flags)), till, i, state)
end

@inline function _iterate(parser::ParserOptions, sequence, till, i, state)
    _iterate(parser.parser, WithOptions(sequence,parser.set_flags), till, i, state)
end


export on_options
struct OnOptions{P,T} <: WrappedParser{T}
    parser::P
    flags::UInt32
    OnOptions(parser,flags::UInt32) =
        new{typeof(parser),result_type(parser)}(parser,flags)
end
map_parser(f::Function,x::OnOptions,a...) =
    OnOptions(
        map_parser(f,x.parser,a...),
        x.flags)

on_options(flags::UInt32,p) =
    OnOptions(parser(p),flags)

@inline _iterate(parser::OnOptions, sequence, till, i, state) =
    nothing

@inline function _iterate(parser::OnOptions, sequence::WithOptions, till, i, state)
    if parser.flags & sequence.flags == parser.flags 
        _iterate(parser.parser,
                 with_options(UInt32(~parser.flags & sequence.flags),sequence), till, i, state)
    else
        nothing
    end
end

export JoinSubstring
struct JoinSubstring{P} <: WrappedParser{SubString}
    parser::P
end
map_parser(f::Function,x::JoinSubstring,a...) =
    JoinSubstring(
        map_parser(f,x.parser,a...))

Base.get(x::JoinSubstring, sequence, till, after, i, state) =
    SubString(sequence, i, prevind(sequence,after))

import Base: SubString
Base.SubString(x::WithOptions,a...) =
    with_options(x.flags,SubString(x.x,a...))

"""
Capture a parser result, optionally with a name.
`index` field is recursively set when calling 'indexed_captures` on the parser.
"""
struct Capture{T,P} <: WrappedParser{T}
    parser::P
    name::Union{Nothing,Symbol}
    index::Int
    Capture(name::Union{Nothing,Symbol},x,index=-1) =
        new{result_type(x),typeof(x)}(x,name==Symbol("") ? nothing : name,index)
end
regex_string(x::Capture) =
    let name = (x.name===nothing ? "" : "?<$(x.name)>")
        "($name$(regex_string(x.parser)))"
    end

function printnode(io::IO,x::Capture)
    print(io,"capture ", x.name===nothing ? x.index : x.name,"=")
    printnode(io,x.parser)
end

function map_parser(f::Function,x::Capture,a...)
    capture(x.name,map_parser(f,x.parser,a...),x.index)
end

function map_parser(f::typeof(indexed_captures),x::Capture,context)
    index=nextind(context,x)
    capture(x.name,map_parser(indexed_captures,x.parser,context),index)
end

capture(x,index=-1) =
    Capture(nothing,x,index)

capture(name::Union{Nothing,Symbol},x,index=-1) =
    Capture(name,parser(x),index)

Base.get(x::Capture, sequence, till, after, i, state) =
    get(x.parser, sequence, till, after, i, state)

@inline function _iterate(parser::Capture, sequence::AbstractString, till, i, state)
    ##@warn "Not in Capturing sequence, not capturing" parser.parser
    _iterate(parser.parser, sequence, till, i, state)
end
      
struct Captures{S}
    captures::Vector{S}
    names::Dict{Symbol,Int}
    Captures{S}() where S =
        new{S}(S[],Dict{Symbol,Int}())
end
function Base.nextind(context::Captures,x::Capture)
    push!(context.captures,x)
    if x.name!==nothing
        haskey(context.names,x.name) && error("PCRE compilation error: two named subpatterns have the same name (PCRE2_DUPNAMES not set) at offset ?? $(x.name)")
        context.names[x.name]=length(context.captures)
    end
    length(context.captures)
end



struct ParserWithCaptures{T,P} <: WrappedParser{T}
    parser::P
    captures::Vector{ParserTypes}
    names::Dict{Symbol,Int}
    ParserWithCaptures(parser,cs::Captures) =
        new{result_type(parser),typeof(parser)}(parser,cs.captures,cs.names)
    ParserWithCaptures(parser,captures,names) =
        new{result_type(parser),typeof(parser)}(parser,captures,names)
end

set_options(set::UInt32,unset::UInt32,parser::ParserWithCaptures) =
    ParserWithCaptures(set_options(set,unset,parser.parser),
                       ParserTypes[ set_options(set,unset,p) for p in parser.captures],
                       parser.names)


indexed_captures(x::ParserWithCaptures) = x

indexed_captures(x) =
    let cs = Captures{ParserTypes}()
        ParserWithCaptures(map_parser(indexed_captures,x,cs),cs)
    end

"""
WithCaptures ensapsulates a sequence to be parsed, `Captures` parser subroutines with names,
and parsed captures.
"""
struct WithCaptures{S,T}
    match::S
    subroutines::Vector{ParserTypes}
    captures::Vector{Union{Nothing,S}}
    names::Dict{Symbol,Int}
    state::T
    WithCaptures(x,cs::Union{Captures,ParserWithCaptures}) =
        let S=typeof(x)
            new{S,Nothing}(x,cs.captures,
                           Union{Nothing,S}[ nothing for c in cs.captures ],cs.names,
                           nothing)
        end
    WithCaptures(x,cs::WithCaptures,state) =
        let S=promote_type(typeof(x),typeof(cs.match))
            new{S,typeof(state)}(x,cs.subroutines,cs.captures,cs.names,state)
        end
end

Base.lastindex(x::WithCaptures) =
    lastindex(x.match)
Base.prevind(x::WithCaptures,i::Integer,n::Integer) =
    prevind(x.match,i,n)
Base.nextind(x::WithCaptures,i::Integer,n::Integer) =
    nextind(x.match,i,n)
Base.prevind(x::WithCaptures,i::Integer) =
    prevind(x.match,i)
Base.nextind(x::WithCaptures,i::Integer) =
    nextind(x.match,i)
Base.getindex(x::WithCaptures,i...) =
    getindex(x.match,i...)

function Base.show(io::IO,x::WithCaptures)
    print(io,"ParseMatch(\"",x.match,"\"")
    indnames=Dict( ( k.second=>k.first for k in pairs(x.names) )... )
    for i in 1:length(x.captures)
        print(io, ", ",get(indnames,i,i),"=")
        if x.captures[i] === nothing
            print(io,"nothing")
        else
            print(io,"\"",x.captures[i],"\"")
        end
    end
    print(io,")")
end

@inline function _iterate(parser::Capture, sequence::WithCaptures, till, i, state)
    r = _iterate(parser.parser, sequence, till, i, state)
    if r !== nothing ## set only if found (e.g. if repeated capture capture last)
        sequence.captures[parser.index] = sequence[ start_index(sequence, r[1], parser.parser, r[2]):r[1]-1 ]
    end
    r
end




export BackReference
"""
Parser matching previously captured sequence, optionally with a name.
`index` field is recursively set when calling 'indexed_captures` on the parser.
"""
struct BackReference <: TextParse.AbstractToken{AbstractString}
    name::Union{Nothing,Symbol}
    index::Int
    BackReference(index::Integer) =
        new(nothing,index)
    BackReference(name::Union{Nothing,Symbol},index::Integer) =
        new(name,index)
    BackReference(name::AbstractString) =
        new(Symbol(name),-1)
end

printnode(io::IO,x::BackReference) = print(io,"\\g{$(x.index)}")
capture_index(name,delta,index,context) =
    if index<0 || delta!=Symbol("")
        if haskey(context.names,name)
            context.names[name]
        else
            length(context.captures)+index+1
        end
    else
        index
    end

function map_parser(::typeof(indexed_captures),x::BackReference,context)
    BackReference(x.name,
                  capture_index(x.name,Symbol(""),x.index,context))
end

@inline function prevind(str,i::Int,parser::BackReference,x)
    prevind(str,i,length(str.captures[parser.index]))
end

@inline function nextind(str,i::Int,parser::BackReference,x)
    nextind(str,i,length(str.captures[parser.index]))
end


@inline function _iterate(p::BackReference, sequence::WithCaptures, till, i, state)
    _iterate(parser(sequence.captures[p.index]), sequence, till, i, state)
end

export SubRoutine
"""
Parser matching preceding capture, optionally with a name.
`index` field is recursively set when calling 'indexed_captures` on the parser.
"""
struct SubRoutine{T} <: TextParse.AbstractToken{T}
    name::Union{Nothing,Symbol}
    delta::Symbol
    index::Int
    SubRoutine{T}(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) where T =
        new{T}(name,delta,index)
    SubRoutine(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) =
        new{Any}(name,delta,index)
end

function map_parser(::typeof(indexed_captures),x::SubRoutine,context)
    index = capture_index(x.name,x.delta,x.index,context)
    if index>length(context.captures)
        SubRoutine{Any}(x.name,Symbol(""),index)
        ## for forward reference
    else
        SubRoutine{result_type(context.captures[index])}(
            x.name,Symbol(""),index)
    end
end


@inline function prevind(sequence,i::Int,parser::SubRoutine,x)
    prevind(sequence,i,sequence.subroutines[parser.index].parser,x)
end

@inline function nextind(sequence,i::Int,parser::SubRoutine,x)
    nextind(sequence,i,sequence.subroutines[parser.index].parser,x)
end


@inline function _iterate(parser::SubRoutine, sequence::WithCaptures, till, i, state)
    _iterate(sequence.subroutines[parser.index].parser, sequence, till, i, state)
end

Base.match(parser::ParserTypes,sequence::AbstractString) =
    match(indexed_captures(parser),sequence)

function Base.match(parser::ParserWithCaptures,sequence::AbstractString)
    s = WithCaptures(sequence,parser)
    start,till=1, lastindex(s)
    i = nothing
    while i===nothing && start <= till
        i = _iterate(parser,s,till,start,nothing)
        i === nothing && (start = nextind(sequence,start))
    end
    i === nothing ? nothing : WithCaptures(SubString(sequence,start,prevind(s,i[1])),s,i)
end

