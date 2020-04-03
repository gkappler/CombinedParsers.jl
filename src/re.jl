
function printopts(io,opts)
    if (opts & Base.PCRE.CASELESS ) != 0; print(io, 'i'); end
    if (opts & Base.PCRE.MULTILINE) != 0; print(io, 'm'); end
    if (opts & Base.PCRE.DOTALL   ) != 0; print(io, 's'); end
    if (opts & Base.PCRE.EXTENDED_MORE ) != 0; print(io, "xx"); end
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
revert(x::WithOptions) =
    WithOptions(revert(x.x),x.flags)
reverse_index(x::WithOptions,a...) =
    reverse_index(x.x,a...)
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
regex_string_(x::WithOptions) = regex_string_(x.x)
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
    Regex(x.x, flagstring(x))

flagstring(x::WithOptions) =
    let sio = IOBuffer()
        printopts(sio, x.flags)
        String(take!(sio))
    end
    
Base.show(io::IO, x::WithOptions) =
    print(io,"\"",x.x,"\"",flagstring(x))

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

Base.convert(::Type{Union{Char,CharIn}},x::WithOptions{Char}) = x.x


with_options(set_flags::UInt32, unset_flags::UInt32,x) =
    with_options(set_flags & ~unset_flags,x)

with_options(flags::UInt32,x::WithOptions) =
    flags == 0 ? with_options(UInt32(0),x.x) : WithOptions(x.x,flags)

with_options(flags::UInt32,x) =
    flags == 0 ? x : WithOptions(x,flags)

with_options(set_flags, unset_flags,x::WithOptions) =
    with_options((set_flags | x.flags) & ~unset_flags,x.x)

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

parser(x::WithOptions{<:AbstractString}) =
    set_options(x.flags,ConstantParser{Base.ncodeunits(x),SubString}(x.x))

function printnode(io::IO,x::ParserOptions)
    print(io,"(?")
    printopts(io,x.set_flags)
    if x.unset_flags!=0
        print(io,"-")
        printopts(io,x.unset_flags)
    end
    print(io,")")
end

@inline function _iterate(parser::ParserOptions, sequence, till, i, state)
    _iterate(parser.parser, with_options(parser.set_flags,parser.unset_flags,sequence), till, i, state)
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
    Capture(x::Capture,index) =
        new{result_type(x),typeof(x.parser)}(x.parser,x.name,index)
end
regex_string(x::Capture) =
    let name = (x.name===nothing ? "" : "?<$(x.name)>")
        "($name$(regex_string(x.parser)))"
    end

function printnode(io::IO,x::Capture)
    print(io,"capture ", x.name===nothing ? x.index : x.name)
end

function map_parser(f::Function,x::Capture,a...)
    capture(x.name,map_parser(f,x.parser,a...),x.index)
end

function map_parser(f::typeof(indexed_captures),x::Capture,context)
    index=nextind(context,x)
    context.captures[index]=capture(x.name,map_parser(indexed_captures,x.parser,context),index)
end

capture(x,index=-1) =
    Capture(nothing,x,index)

capture(name::Union{Nothing,Symbol},x,index=-1) =
    Capture(name,parser(x),index)

Base.get(x::Capture, sequence, till, after, i, state) =
    get(x.parser, sequence, till, after, i, state)
      
struct Captures{S}
    captures::Vector{S}
    names::Dict{Symbol,Int}
    Captures{S}() where S =
        new{S}(S[],Dict{Symbol,Int}())
end
function Base.nextind(context::Captures,x::Capture)
    index = length(context.captures)+1
    push!(context.captures,Capture(x,index))
    if x.name!==nothing
        haskey(context.names,x.name) && error("PCRE compilation error: two named subpatterns have the same name (PCRE2_DUPNAMES not set) at offset ?? $(x.name)")
        context.names[x.name]=length(context.captures)
    end
    index
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
function printnode(io::IO,x::ParserWithCaptures)
    print(io,"regular expression combinator")
    length(x.captures)>0 && print(io,", capturing $(length(x.captures))")
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
    captures::Vector{Vector{UnitRange{Int}}}
    names::Dict{Symbol,Int}
    state::T
    WithCaptures(match,subroutines, captures, names, state) =
            new{typeof(match),typeof(state)}(match,subroutines, captures, names, state)
    
    WithCaptures(x,cs::Union{Captures,ParserWithCaptures}) =
        let S=typeof(x)
            new{S,Nothing}(x,cs.captures,
                           Vector{S}[ S[] for c in cs.captures ],cs.names,
                           nothing)
        end
    WithCaptures(x,cs::WithCaptures,state) =
        let S=typeof(x)
            new{S,typeof(state)}(x,cs.subroutines,cs.captures,cs.names,state)
        end
    WithCaptures(x,cs::WithCaptures) =
        let S=typeof(x)
            new{S,typeof(cs.state)}(x,cs.subroutines,cs.captures,cs.names,cs.state)
        end
    function WithCaptures(cs::WithCaptures,start::Integer,stop::Integer,state)
        m = (cs.match,start:stop)
        caps = [ [ (c.start-start+1):(c.stop-start+1) for c in caps
                   if c.stop<=stop # c.start>=start && 
                   ]
                 for caps in cs.captures ]
        new{typeof(m),typeof(state)}(m,cs.subroutines,caps,cs.names,state)
    end
end
copy_captures(x::WithCaptures) =
    WithCaptures(x.match,x.subroutines, [ copy(c) for c in x.captures ],x.names,x.state)
revert(x::WithCaptures) = WithCaptures(revert(x.match),x)
reverse_index(x::WithCaptures,a...) = reverse_index(x.match,a...)
with_options(flags::UInt32,x::WithCaptures) =
    WithCaptures(with_options(flags,x.match),x)
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

struct ParseMatch{C<:WithCaptures}
    m::C
    ParseMatch(x,cs::WithCaptures,state) =
        let wc=WithCaptures(x,cs,state)
            new{typeof(wc)}(wc)
        end
    function ParseMatch(cs::WithCaptures,start::Integer,stop::Integer,state)
        wc=WithCaptures(cs,start,stop,state)
        new{typeof(wc)}(wc)
    end
end

match_string(x::SubString,y::UnitRange{<:Integer}) =
    SubString(x.string,x.offset+y.start,x.offset+y.stop)

match_string(x::Tuple{<:AbstractString,UnitRange{<:Integer}},y::UnitRange{<:Integer}) =
    let rel = min(x[2].start,x[2].stop)-1
        SubString(x[1],rel+y.start,rel+y.stop)
    end

import Base: getproperty
function Base.getproperty(x::ParseMatch,key::Symbol)
    m = getfield(x,1)
    if key==:captures
        [ isempty(c) ? nothing : match_string(m.match,c[end])
          for c in m.captures ]
    elseif key==:match
        SubString(m.match[1],m.match[2])
    end
end

function Base.show(io::IO,m::ParseMatch)
    x=getfield(m,1)
    print(io,"ParseMatch(\"",m.match,"\"")
    indnames=Dict( ( k.second=>k.first for k in pairs(x.names) )... )
    for i in 1:length(x.captures)
        print(io, ", ",get(indnames,i,i),"=")
        if isempty(x.captures[i])
            print(io,"nothing")
        else
            print(io,"\"",match_string(x.match,x.captures[i][end]),"\"")
        end
    end
    print(io,")")
end


==(pc_m::ParseMatch,pcre_m::RegexMatch) =
    pcre_m==pc_m

"""
==(pcre_m::RegexMatch,pc_m::ParseMatch)

equal iif match's value and offset, and captures are equal.
"""
function ==(pcre_m::RegexMatch,pc_m::ParseMatch)
    pcre_m.match==pc_m.match &&
        pcre_m.match.offset==pc_m.match.offset &&
        pcre_m.match.ncodeunits==pc_m.match.ncodeunits &&
        pcre_m.captures==pc_m.captures
end

@inline function _iterate(parser::Capture, sequence, till, i, state)
    r = _iterate(parser.parser, sequence, till, i, state)
    if r !== nothing ## set only if found (e.g. if repeated capture capture last)
        before_i = _prevind(sequence,r[1],parser.parser,r[2])
        set_capture(sequence,parser.index,before_i,prevind(sequence,r[1]))
    else
        prune_captures(sequence, _prevind(sequence,i,parser.parser,state))
    end
    r
end

set_capture(sequence::AbstractString, index::Int, start, stop) =
    @warn "not setting capture $index=$(sequence[start:stop])"
set_capture(sequence::WithOptions, index::Int, start,stop) =
    set_capture(sequence.x,index,start,stop)
set_capture(sequence::WithCaptures, index::Int, start,stop) =
    push!(sequence.captures[index], start:stop)
set_capture(sequence::WithCaptures{<:Reverse}, index::Int, start,stop) =
    push!(sequence.captures[index],
          @show reverse_index(sequence.match,
                              prevind(sequence.match,stop)):reverse_index(sequence.match,
                                                                          prevind(sequence.match,start)))

function prune_captures(sequence,after_i)
end
function prune_captures(sequence::WithCaptures,after_i)
    ## @show after_i
    for i in 1:length(sequence.captures)
        cv = sequence.captures[i]
        while !isempty(cv) && ( cv[end].stop > after_i || cv[end].start > after_i )##-1
            ##@show after_i
            pop!(cv)
        end
    end
end



export BackReference
"""
Parser matching previously captured sequence, optionally with a name.
`index` field is recursively set when calling 'indexed_captures` on the parser.
"""
struct BackReference <: AbstractParser{AbstractString}
    name::Union{Nothing,Symbol}
    index::Int
    fallback::Function
    BackReference(f::Function,index::Integer) =
        new(nothing,index,f)
    BackReference(f::Function,name::Union{Nothing,Symbol},index::Integer) =
        new(name,index,f)
    BackReference(f::Function,name::AbstractString) =
        new(Symbol(name),-1,f)
end

regex_string(x::BackReference) =
    "\\g{"*
if x.name !== nothing
    string(x.name)
else
    string(x.index)
end *"}"

printnode(io::IO,x::BackReference) = print(io,regex_string(x))
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
    idx = capture_index(x.name,Symbol(""),x.index,context)
    if idx < 1 || idx>lastindex(context.captures)
        x.fallback()
    else
        BackReference(x.fallback,x.name, idx)
    end
end

function Base.get(x::BackReference, sequence, till, after, i, state)
    sequence[i:_prevind(sequence,i+state)]
end

@inline function prevind(str,i::Int,parser::BackReference,x)
    i-x
end

@inline function nextind(str,i::Int,parser::BackReference,x)
    i+x
end


@inline function _iterate(p::BackReference, sequence::WithCaptures, till, i, state)
    return nothing
end
@inline function _iterate(p::BackReference, sequence::WithCaptures, till, i, state::Nothing)
    isempty(sequence.captures[p.index]) && return nothing
    r = _iterate(parser(SubString(sequence.match,sequence.captures[p.index][end])), sequence, till, i, state)
    r === nothing && return nothing
    r[1], r[1]-i
end

export SubRoutine
"""
    Parser matching preceding capture, optionally with a name.
    `index` field is recursively set when calling 'indexed_captures` on the parser.
    """
struct SubRoutine{T} <: AbstractParser{T}
    name::Union{Nothing,Symbol}
    delta::Symbol
    index::Int
    SubRoutine{T}(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) where T =
        new{T}(name,delta,index)
    SubRoutine(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) =
        new{Any}(name,delta,index)
end
regex_string(x::SubRoutine) =
    "(?"*
if x.name !== nothing
    string(x.name)
else
    if x.delta == Symbol("")
        string(x.delta)
    else
        ""
    end*string(x.index)
end *")"
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
    _iterate(sequence.subroutines[parser.index].parser, copy_captures(sequence), till, i, state)
end

Base.match(parser::ParserTypes,sequence::AbstractString) =
    match(indexed_captures(parser),sequence)

function Base.match(parser::ParserWithCaptures,sequence::AbstractString)
    s = WithCaptures(sequence,parser)
    start,till=1, lastindex(s)
    i = nothing
    while i===nothing && start <= till+1
        i = _iterate(parser,s,till,start,nothing)
        i === nothing && (start = start <= till ? nextind(sequence,start) : start+1)
    end
    ##@show start
    i === nothing ? nothing : ParseMatch(s,start,prevind(s,i[1]),i[2])
end

"""
parse(p::ParserTypes, s::AbstractString)

parse `s` with parser `p`.
"""
function _iterate(p::ParserWithCaptures, sequence::AbstractString)
    s = WithCaptures(sequence,p)
    i = _iterate(p,s)
end


import Base: SubString, ==

Base.SubString(x::WithCaptures,a...) =
    SubString(x.match,a...)
Base.SubString(x::WithOptions,a...) =
    SubString(x.x,a...)
Base.SubString(x::Reverse,start,stop) =
    SubString(x.x, reverse_index(x,stop), reverse_index(x,start))
