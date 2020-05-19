"""
A regular expression parser transforming a PCRE string to a CombinedParser equivalent to the regular expression.
"""
module Regexp
using ..CombinedParsers
using TextParse
import TextParse: AbstractToken
import ..CombinedParsers: WrappedParser, ParserTypes, ConstantParser, LookAround, Either, SideeffectParser, MatchingNever
import ..CombinedParsers: parser, prune_captures, deepmap_parser, _iterate, print_constructor
import ..CombinedParsers: regex_prefix, regex_suffix, regex_inner, regex_string_, regex_string
import ..CombinedParsers: revert, reverse_index, state_type

import Base: prevind, nextind

indexed_captures_(x,context,reset_number) = x

include("pcre.jl")


import Base: SubString, ==

"""
SequenceWithCaptures ensapsulates a sequence to be parsed, and parsed captures.

See also [`ParserWithCaptures`](@ref)
"""
struct SequenceWithCaptures{S,T}
    match::S
    subroutines::Vector{ParserTypes}
    captures::Vector{Vector{UnitRange{Int}}}
    names::Dict{Symbol,Int}
    state::T
    SequenceWithCaptures(match,subroutines, captures, names, state) =
            new{typeof(match),typeof(state)}(match,subroutines, captures, names, state)
    SequenceWithCaptures(x,cs::SequenceWithCaptures,state) =
        let S=typeof(x)
            new{S,typeof(state)}(x,cs.subroutines,cs.captures,cs.names,state)
        end
    SequenceWithCaptures(x,cs::SequenceWithCaptures) =
        let S=typeof(x)
            new{S,typeof(cs.state)}(x,cs.subroutines,cs.captures,cs.names,cs.state)
        end
    function SequenceWithCaptures(cs::SequenceWithCaptures,start::Integer,stop::Integer,state)
        m = (cs.match,start:stop)
        caps = [ [ (c.start-start+1):(c.stop-start+1) for c in caps
                   if c.stop<=stop # c.start>=start && 
                   ]
                 for caps in cs.captures ]
        new{typeof(m),typeof(state)}(m,cs.subroutines,caps,cs.names,state)
    end
end
copy_captures(x::SequenceWithCaptures,state) =
    SequenceWithCaptures(x.match,x.subroutines, [ copy(c) for c in x.captures ],x.names,state)
revert(x::SequenceWithCaptures) = SequenceWithCaptures(revert(x.match),x)
reverse_index(x::SequenceWithCaptures,a...) = reverse_index(x.match,a...)
with_options(flags::UInt32,x::SequenceWithCaptures) =
    SequenceWithCaptures(with_options(flags,x.match),x)
Base.lastindex(x::SequenceWithCaptures) =
    lastindex(x.match)
Base.prevind(x::SequenceWithCaptures,i::Integer,n::Integer) =
    prevind(x.match,i,n)
Base.nextind(x::SequenceWithCaptures,i::Integer,n::Integer) =
    nextind(x.match,i,n)
Base.prevind(x::SequenceWithCaptures,i::Integer) =
    prevind(x.match,i)
Base.nextind(x::SequenceWithCaptures,i::Integer) =
    nextind(x.match,i)
Base.getindex(x::SequenceWithCaptures,i...) =
    getindex(x.match,i...)

Base.SubString(x::SequenceWithCaptures,a...) =
    SubString(x.match,a...)


export Capture
"""
Capture a parser result, optionally with a name.
`index` field is initialized when calling `ParserWithCaptures` on the parser.

[`ParserWithCaptures`](@ref)
"""
struct Capture{P,T} <: WrappedParser{P,T}
    parser::P
    name::Union{Nothing,Symbol}
    index::Int
    Capture(name::Union{Nothing,Symbol},x_,index=-1) =
        let x = parser(x_)
            new{typeof(x),result_type(x)}(x,name==Symbol("") ? nothing : name,index)
        end
    Capture(x::Capture,index) =
        new{typeof(x.parser),result_type(x)}(x.parser,x.name,index)
end
Capture(x,index=-1) =
    Capture(nothing,x,index)
function print_constructor(io::IO,x::Capture)
    print_constructor(io,x.parser)
    print(io, " |> Capture" )
end

regex_string(x::Capture) = regex_prefix_(x)*regex_string(x.parser)*")"
regex_prefix_(x::Capture) =
    let name = (x.name===nothing ? "" : "?<$(x.name)>")
        "($name"
    end
regex_prefix(x::Capture) =
    regex_prefix_(x::Capture)*regex_prefix(x.parser)
regex_suffix(x::Capture) = regex_suffix(x.parser)*")"

function deepmap_parser(f::Function,mem::AbstractDict,x::Capture,a...)
    get!(mem,x) do
        Capture(x.name,deepmap_parser(f,mem,x.parser,a...),x.index)
    end
end

function deepmap_parser(f::typeof(indexed_captures_),mem::AbstractDict,x::Capture,context,a...)
    get!(mem,x) do
        index=nextind(context,x)
        context.subroutines[index]=Capture(x.name,deepmap_parser(indexed_captures_,mem,x.parser,context,a...),index)
    end
end

Base.get(x::Capture, sequence, till, after, i, state) =
    get(x.parser, sequence, till, after, i, state)


@inline function _iterate(parser::Capture, sequence, till, i, state)
    before_i = state === nothing ? i : prevind(sequence,i,parser.parser,state)
    r = _iterate(parser.parser, sequence, till, i, state)
    if r !== nothing ## set only if found (e.g. if repeated capture capture last)
        set_capture(sequence,parser.index,before_i,prevind(sequence,r[1]))
    elseif state !== nothing
        prune_captures(sequence, before_i)
    end
    r
end

set_capture(sequence::AbstractString, index::Int, start, stop) = nothing
set_capture(sequence::WithOptions, index::Int, start,stop) =
    set_capture(sequence.x,index,start,stop)
set_capture(sequence::SequenceWithCaptures, index::Int, start,stop) =
    push!(sequence.captures[index], start:stop)
set_capture(sequence::SequenceWithCaptures{<:Reverse}, index::Int, start,stop) =
    push!(sequence.captures[index],
          reverse_index(sequence.match,
                        stop):reverse_index(sequence.match,
                                            start))

function prune_captures(sequence::SequenceWithCaptures,after_i)
    for i in 1:length(sequence.captures)
        cv = sequence.captures[i]
        while !isempty(cv) && ( cv[end].stop >= after_i)
            ##@show after_i
            pop!(cv)
        end
    end
end



export Backreference
"""
    Backreference(f::Function,index::Integer)

    Backreference(f::Function,name::Union{Nothing,Symbol},index::Integer)

    Backreference(f::Function,name::AbstractString)

Parser matching previously captured sequence, optionally with a name.
`index` field is recursively set when calling 'ParserWithCaptures` on the parser.
"""
struct Backreference <: AbstractParser{AbstractString}
    name::Union{Nothing,Symbol}
    index::Int
    fallback::Function
    Backreference(f::Function,index::Integer) =
        new(nothing,index,f)
    Backreference(f::Function,name::Union{Nothing,Symbol},index::Integer) =
        new(name,index,f)
    Backreference(f::Function,name::AbstractString) =
        new(Symbol(name),-1,f)
end

regex_string_(x::Function) = "#\$($x)"
                                                          
regex_string_(x::Backreference) =
   if x.name !== nothing
       string(x.name)
   else
       string(x.index)
   end
                                                          
regex_inner(x::Backreference) =
    "\\g{"*regex_string_(x) *"}"

capture_index(name,delta,index,context) =
    if haskey(context.names,name)
        context.names[name]
    elseif ( index<0 || delta!=Symbol("") )
        if name !== nothing
            index
        else
            length(context.subroutines)+index+1
        end
    else
        index
    end

function deepmap_parser(::typeof(log_names),mem::AbstractDict,x::Backreference,a...)
    get!(mem,x) do
        with_log("backreference $x",x)
    end
end

function deepmap_parser(::typeof(indexed_captures_),mem::AbstractDict,x::Backreference,context,a...)
    get!(mem,x) do
        idx = capture_index(x.name,Symbol(""),x.index,context)
        if idx < 1 || idx>lastindex(context.subroutines)
            x.fallback()
        else
            Backreference(x.fallback,x.name, idx)
        end
    end
end

function Base.get(x::Backreference, sequence, till, after, i, state)
    sequence[i:_prevind(sequence,i+state)]
end

@inline function prevind(str,i::Int,parser::Backreference,x)
    i-x
end

@inline function nextind(str,i::Int,parser::Backreference,x)
    i+x
end


state_type(::Type{<:Backreference}) = Int
@inline function _iterate(p::Backreference, sequence::SequenceWithCaptures, till, i, state)
    return nothing
end

@inline function _iterate(p::Backreference, sequence::SequenceWithCaptures, till, i, state::Nothing)
    isempty(sequence.captures[p.index]) && return nothing
    r = _iterate(
        parser(SubString(sequence.match, sequence.captures[p.index][end])),
        sequence, till, i, state)
    r === nothing && return nothing
    r[1], r[1]-i
end
_iterate_condition(cond::Backreference, sequence, till, i, state) =
    !isempty(sequence.captures[cond.index])




export Subroutine
"""
Parser matching preceding capture, optionally with a name.
`index` field is recursively set when calling 'ParserWithCaptures` on the parser.
"""
struct Subroutine{T} <: AbstractParser{T}
    name::Union{Nothing,Symbol}
    delta::Symbol
    index::Int
    Subroutine{T}(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) where T =
        new{T}(name,delta,index)
    Subroutine(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) =
        new{Any}(name,delta,index)
    Subroutine(index::Int) =
        new{Any}(nothing,Symbol(""),index)
    Subroutine(name::AbstractString) =
        new{Any}(Symbol(name),Symbol(""),-1)
    Subroutine() =
        new{Any}(nothing,Symbol(""),-1)
end
children(x::Subroutine) = tuple()
regex_prefix(x::Subroutine) = "(?" *
if x.name !== nothing
    "&$(x.name),$(x.index)"
else
    if x.delta == Symbol("")
        string(x.delta)
    else
        ""
    end*string(x.index)
end
regex_suffix(x::Subroutine) = ")"
regex_inner(x::Subroutine) = ""
deepmap_parser(::typeof(revert),mem::AbstractDict,x::Subroutine) = x

function _iterate_condition(cond::Subroutine, sequence, till, i, state)
    sequence.state === nothing && return false
    if cond.name === nothing && cond.index < 0
        true
    elseif cond.index>0
        cond.index == sequence.state.index
    else
        @show cond, sequence.state
        error()
    end
end

function deepmap_parser(::typeof(indexed_captures_),mem::AbstractDict,x::Subroutine,context,a...)
    get!(mem,x) do
        index = capture_index(x.name,x.delta,x.index, context)
        if index <= 0 || index>length(context.subroutines)
            Subroutine{Any}(x.name,Symbol(""),index)
        else
            Subroutine{result_type(context.subroutines[index])}(
                x.name,Symbol(""),index)
        end
    end
end

@inline function prevind(sequence,i::Int,parser::Subroutine,x)
    prevind(sequence,i,sequence.subroutines[index(parser,sequence)].parser,x)
end

@inline function nextind(sequence,i::Int,parser::Subroutine,x)
    nextind(sequence,i,sequence.subroutines[index(parser,sequence)].parser,x)
end

index(parser::Subroutine,sequence) = parser.index <= 0 ? sequence.names[parser.name] : parser.index

@inline function _iterate(parser::Subroutine, sequence::SequenceWithCaptures, till, i, state)
    _iterate(
        with_log("$(parser.name)",sequence.subroutines[index(parser,sequence)].parser),
        copy_captures(sequence,parser), till, i, state)
end
state_type(::Type{<:Subroutine}) = Any



export DupSubpatternNumbers
"""
Parser wrapper for `ParserWithCaptures`, setting reset_index=true in `deepmap_parser(::typeof(indexed_captures_),...)`.
"""
struct DupSubpatternNumbers{P,T} <: WrappedParser{P,T}
    parser::P
    DupSubpatternNumbers(parser) =
        new{typeof(parser),result_type(parser)}(parser)
end

function deepmap_parser(f::typeof(indexed_captures_),mem::AbstractDict,x::DupSubpatternNumbers,context,reset_index)
    get!(mem,x) do
        DupSubpatternNumbers(deepmap_parser(indexed_captures_,mem,x.parser,context,true))
    end
end

function deepmap_parser(::typeof(indexed_captures_),mem::AbstractDict,x::Either,context,reset_index)
    if reset_index
        idx = lastindex(context.subroutines)
        branches = Any[]
        for p in reverse(x.options) ## keep first for subroutines
            while lastindex(context.subroutines)>idx
                pop!(context.subroutines)
            end
            push!(branches,deepmap_parser(indexed_captures_,mem,p,context,false))
        end
        Either{result_type(x)}(tuple( branches... ))
    else
        Either{result_type(x)}(
            tuple( (deepmap_parser(indexed_captures_,mem,p,context,false) for p in x.options )...))
    end
end





export Conditional
"""
Conditional parser, `_iterate` cycles conditionally on `_iterate_condition` through matches in field `yes` and `no` respectively.
"""
struct Conditional{C,Y,N,T} <: AbstractParser{T}
    condition::C
    yes::Y
    no::N
    Conditional(condition,yes,no) =
        new{typeof(condition),typeof(yes),typeof(no),Union{result_type(yes),result_type(no)}}(condition,yes,no)
end

function regex_prefix(x::Conditional)
    "(?("*regex_string_(x.condition)*")"
end
function regex_suffix(x::Conditional)
    ")"
end
regex_inner(x::Conditional) =
    regex_string(x.yes)*(isa(x.no, Always) ? "" : ( "|" * regex_string(x.no)))


children(x::Conditional) = x.no isa Always ? tuple(x.yes) : tuple(x.yes,x.no)


function deepmap_parser(::typeof(log_names),mem::AbstractDict,x::Conditional,a...)
    get!(mem,x) do
        with_log("conditional $(regex_string(x))",x)
    end
end

function deepmap_parser(::typeof(indexed_captures_),mem::AbstractDict,x::Conditional,context,a...)
    get!(mem,x) do
        Conditional(deepmap_parser(indexed_captures_,mem,x.condition,context,a...),
                    deepmap_parser(indexed_captures_,mem,x.yes,context,a...),
                    deepmap_parser(indexed_captures_,mem,x.no,context,a...))
    end
end

@inline Base.get(parser::Conditional, sequence, till, after, i, state) =
    get(state.first == :yes ? parser.yes : parser.no, sequence, till, after, i, state.second)

_iterate_condition(cond, sequence, till, i, state) =
    _iterate(cond, sequence, till, i, state) !== nothing

state_type(::Type{Conditional{C,Y,N,T}}) where {C,Y,N,T} =
    Pair{Symbol,Union{state_type(Y),state_type(N)}}


@inline function prevind(str,i::Int,parser::Conditional,state)
    prevind(str,i,state.first == :yes ? parser.yes : parser.no, state.second)
end

@inline function nextind(str,i::Int,parser::Conditional,state)
    nextind(str,i,state.first == :yes ? parser.yes : parser.no, state.second)
end

@inline function _iterate(parser::Conditional, sequence, till, i, state::Nothing)
    c = _iterate_condition(parser.condition, sequence, till, i, state)
    f = c ? :yes : :no
    cparse = f == :yes ? parser.yes : parser.no
    s = _iterate(cparse,
                 sequence, till, i, state)
    s === nothing && return nothing
    s[1], f => s[2]
end

@inline function _iterate(parser::Conditional, sequence, till, i, state::Pair)
    _iterate(state.first == :yes ? parser.yes : parser.no, sequence, till, i, state.second)
end



"""
Top level parser supporting regular expression features
captures, backreferences and subroutines.
Collects subroutines in field `subroutines::Vector` and 
indices of named capture groups in field `names::Dict`.

!!! note
    implicitly called in `match`
See also [`Backreference`](@ref), [`Capture`](@ref), [`Subroutine`](@ref)
"""
struct ParserWithCaptures{P,T} <: WrappedParser{P,T}
    parser::P
    subroutines::Vector{ParserTypes} ## todo: rename subroutines
    names::Dict{Symbol,Int}
    ParserWithCaptures(parser,captures,names) =
        new{typeof(parser),result_type(parser)}(parser,captures,names)
end
ParserWithCaptures(x) =
    let cs = ParserWithCaptures(x,ParserTypes[],Dict{Symbol,Int}())
        pass1 = ParserWithCaptures(deepmap_parser(indexed_captures_,NoDict(),x,cs,false),cs.subroutines,cs.names)
        ParserWithCaptures(deepmap_parser(indexed_captures_,NoDict(),pass1.parser,pass1,false),pass1.subroutines,pass1.names)
    end

function Base.match(parser::ParserTypes,sequence::AbstractString; kw...)
    @warn "For better performance create `ParserWithCaptures(parser)` before calling `match`."
    match(ParserWithCaptures(parser),sequence; kw...)
end

"""
    Base.match(parser::ParserTypes,sequence::AbstractString; log=false)

Plug-in replacement for match(::Regex,sequence).
"""
function Base.match(parser::ParserWithCaptures,sequence::AbstractString; log=false)
    log && ( parser=log_names(parser) )
    s = SequenceWithCaptures(sequence,parser)
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
    _iterate(p::ParserWithCaptures, sequence::AbstractString)


"""
function _iterate(p::ParserWithCaptures, sequence::AbstractString)
    s = SequenceWithCaptures(sequence,p)
    i = _iterate(p,s)
end

##Base.getindex(x::ParserWithCaptures, i) = ParserWithCaptures(getindex(i,x)

SequenceWithCaptures(x,cs::ParserWithCaptures) =
    let S=typeof(x)
        SequenceWithCaptures(x,cs.subroutines,
                     Vector{S}[ S[] for c in cs.subroutines ],cs.names,
                     nothing)
    end

function print_constructor(io::IO, x::ParserWithCaptures)
    print_constructor(io,x.parser)
    print(io, " |> regular expression combinator",
          ( length(x.subroutines)>0 ? " with $(length(x.subroutines)) capturing groups" : "" ) )
end

set_options(set::UInt32,unset::UInt32,parser::ParserWithCaptures) =
    ParserWithCaptures(set_options(set,unset,parser.parser),
                       ParserTypes[ set_options(set,unset,p) for p in parser.subroutines],
                       parser.names)

function deepmap_parser(f::Function,mem::AbstractDict,x::ParserWithCaptures,a...)
    ParserWithCaptures(deepmap_parser(f,mem,x.parser,a...),
                       [ deepmap_parser(f,mem,p,a...) for p in x.subroutines ],
                       x.names)
end

function Base.nextind(context::ParserWithCaptures,x::Capture)
    if x.name!==nothing
        if haskey(context.names,x.name)
            return context.names[x.name]
        end
    end
    if x.index<0
        index = length(context.subroutines)+1
        push!(context.subroutines,Capture(x,index))
        x.name !== nothing && (context.names[x.name]=length(context.subroutines))
        index
    else
        x.index
    end
end

ParserWithCaptures(x::ParserWithCaptures) = x

"For use in ParserWithCaptures to enforce different indices for identical captures."
struct NoDict{K,V} <: AbstractDict{K,V} end
NoDict() = NoDict{Any,Any}()

import Base: get!
Base.get!(f::Function,d::NoDict,k) = f()




"""
Wrapper type for [`SequenceWithCaptures`](@ref), providing
`getindex` and `getproperty` behavior like `RegexMatch`.

```jldoctest
julia> m = match(re"(?<a>so)+ (or)", "soso or")
ParseMatch("soso or", a="so", 2="or")

julia> m[:a]
"so"

julia> m[2]
"or"

julia> m.match, m.captures
("soso or", SubString{String}["so", "or"])
```
"""
struct ParseMatch{C<:SequenceWithCaptures}
    m::C
    ParseMatch(x,cs::SequenceWithCaptures,state) =
        let wc=SequenceWithCaptures(x,cs,state)
            new{typeof(wc)}(wc)
        end
    function ParseMatch(cs::SequenceWithCaptures,start::Integer,stop::Integer,state)
        wc=SequenceWithCaptures(cs,start,stop,state)
        new{typeof(wc)}(wc)
    end
end

function Base.getindex(x::ParseMatch,i::Integer)
    m = getfield(x,1)
    c = m.captures[i]
    isempty(c) ? nothing : match_string(m.match,c[end])
end

function Base.getindex(m::ParseMatch,i::Symbol)
    x=getfield(m,1)
    getindex(m,x.names[i])
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
    print(io,"ParseMatch(\"",escape_string(m.match),"\"")
    indnames=Dict( ( k.second=>k.first for k in pairs(x.names) )... )
    for i in 1:length(x.captures)
        print(io, ", ",get(indnames,i,i),"=")
        if isempty(x.captures[i])
            print(io,"nothing")
        else
            print(io,"\"",match_string(x.match, x.captures[i][end]),"\"")
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


include("re-parser.jl")




end
