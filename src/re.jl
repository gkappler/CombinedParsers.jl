
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

struct WithOptions{S}
    x::S
    flags::UInt32
end
Base.getindex(x::WithOptions,i) =
    WithOptions(x.x[i],x.flags)
function ==(x::Char,y::WithOptions{Char})
    if !iszero(y.flags & Base.PCRE.CASELESS)
        lowercase(x)==lowercase(y.x)
    else
        x == y.x
    end
end

function Base.in(x::WithOptions,set)
    error()
end

export with_options, instance
struct ParserOptions{P,T} <: TextParse.AbstractToken{T}
    parser::P
    flags::UInt32
    ParserOptions(parser,flags::UInt32) =
        new{typeof(parser),result_type(parser)}(parser,flags)
end
indexed_captures(x::ParserOptions,context) =
    ParserOptions(
        indexed_captures(x.parser,context),
        x.flags)

with_options(parser,flags::UInt32) =
    ParserOptions(parser,flags)

@inline function _iterate(parser::ParserOptions, sequence, i, till, state)
    _iterate(parser.parser, WithOptions(sequence,parser.flags), i, till, state)
end

Base.get(x::ParserOptions, a...) =
    get(x.parser,a...)

                     
struct Captures{S}
    captures::Vector{S}
    names::Dict{Symbol,Int}
    Captures{S}() where S =
        new{S}(S[],Dict{Symbol,Int}())
end
function Base.nextind(context::Captures,x::Capture)
    push!(context.captures,x)
    if x.name!==nothing
        haskey(context.names,x.name) && error("PCRE compilation error: two named subpatterns have the same name (PCRE2_DUPNAMES not set) at offset ??")
        context.names[x.name]=length(context.captures)
    end
    length(context.captures)
end

"""
WithCaptures ensapsulates a sequence to be parsed, `Captures` parser subroutines with names,
and parsed captures.
"""
struct WithCaptures{S,T}
    sequence::S
    subroutines::Vector{ParserTypes}
    captures::Vector{Union{Nothing,S}}
    names::Dict{Symbol,Int}
    state::T
    WithCaptures(x,cs::Captures) =
        let S=typeof(x)
            new{S,Nothing}(x,cs.captures,
                           Union{Nothing,S}[ nothing for c in cs.captures ],cs.names,
                           nothing)
        end
    WithCaptures(x,cs::WithCaptures,state) =
        let S=promote_type(typeof(x),typeof(cs.sequence))
            new{S,typeof(state)}(x,cs.subroutines,cs.captures,cs.names,state)
        end
end

Base.lastindex(x::WithCaptures) =
    lastindex(x.sequence)
Base.prevind(x::WithCaptures,a...) =
    prevind(x.sequence,a...)
Base.nextind(x::WithCaptures,a...) =
    nextind(x.sequence,a...)
Base.getindex(x::WithCaptures,i...) =
    getindex(x.sequence,i...)

function Base.show(io::IO,x::WithCaptures)
    print(io,"ParserMatch(\"",x.sequence,"\"")
    indnames=Dict( ( k.second=>k.first for k in pairs(x.names) )... )
    for i in 1:length(x.captures)
        print(io, ",",get(indnames,i,i),"=","\"",x.captures[i],"\"")
    end
    print(io,")")
end

"""
Capture a parser result, optionally with a name.
`index` field is recursively set when calling 'indexed_captures` on the parser.
"""
struct Capture{T,P} <: TextParse.AbstractToken{T}
    parser::P
    name::Union{Nothing,Symbol}
    index::Int
    Capture(name::Union{Nothing,Symbol},x,index=-1) =
        new{result_type(x),typeof(x)}(x,name==Symbol("") ? nothing : name,index)
end

function indexed_captures(x::Capture,context)
    index=nextind(context,x)
    capture(x.name,indexed_captures(x.parser,context),index)
end

capture(x,index=-1) =
    Capture(nothing,x,index)

capture(name::Union{Nothing,Symbol},x,index=-1) =
    Capture(name,x,index)

Base.get(x::Capture, sequence, after, i, till, state) =
    get(x.parser, sequence, after, i, till, state)

@inline function _iterate(parser::Capture, sequence::AbstractString, i, till, state)
    @warn "Not in Capturing sequence, not capturing" parser.parser
    _iterate(parser.parser, sequence, i, till, state)
end

@inline function _iterate(parser::Capture, sequence::WithCaptures, i, till, state)
    r = _iterate(parser.parser, sequence, i, till, state)
    r === nothing && return nothing
    sequence.captures[parser.index] = sequence[i:r[1]-1]
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


capture_index(name,delta,index,context) =
    if index<0 || delta!=Symbol("")
        if haskey(context.names,name)
            context.names[name]
        else
            length(context.captures)+index##+1
        end
    else
        index
    end

function indexed_captures(x::BackReference,context)
    BackReference(x.name,
                   capture_index(x.name,Symbol(""),x.index,context))
end


@inline function _iterate(parser::BackReference, sequence::WithCaptures, i, till, state)
    _iterate(sequence.captures[parser.index], sequence, i, till, state)
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

function indexed_captures(x::SubRoutine,context)
    index = capture_index(x.name,x.delta,x.index,context)
    if index>length(context.captures)
        SubRoutine{Any}(x.name,Symbol(""),index)
        ## for forward reference
    else
        SubRoutine{result_type(context.captures[index])}(
            x.name,Symbol(""),index)
    end
end



@inline function _iterate(parser::SubRoutine, sequence::WithCaptures, i, till, state)
    _iterate(sequence.subroutines[parser.index].parser, sequence, i, till, state)
end



function Base.match(parser::ParserTypes,sequence::AbstractString)
    subroutines = Captures{ParserTypes}()
    p = indexed_captures(parser,subroutines)
    s = WithCaptures(sequence,subroutines)
    start,till=1, lastindex(s)
    i = nothing
    while i===nothing && start < till
        i = _iterate(p,s,start,till,nothing)
        i === nothing && (start = nextind(sequence,start))
    end
    i === nothing ? nothing : WithCaptures(sequence[start:prevind(s,i[1])],s,i)
end

