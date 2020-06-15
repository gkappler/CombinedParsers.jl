"""
A regular expression parser transforming a PCRE string to a CombinedParser equivalent to the regular expression.
"""
module Regexp
using ..CombinedParsers
using TextParse
import TextParse: AbstractToken
using AutoHashEquals

import ..CombinedParsers: LeafParser, WrappedParser, ParserTypes, ConstantParser, LookAround, Either, SideeffectParser, MatchingNever
import ..CombinedParsers: parser, prune_captures, deepmap_parser, _iterate, print_constructor
import ..CombinedParsers: regex_prefix, regex_suffix, regex_inner, regex_string_, regex_string, log_names_
import ..CombinedParsers: revert, reverse_index, state_type, start_index, tuple_pos, tuple_state

import Base: prevind, nextind

indexed_captures_(x,a...) = x

include("pcre.jl")


import Base: SubString, ==

"""
SequenceWithCaptures ensapsulates a sequence to be parsed, and parsed captures.

See also [`ParserWithCaptures`](@ref)
"""
@auto_hash_equals struct SequenceWithCaptures{S,T}
    match::S
    subroutines::Vector{ParserTypes}
    captures::Vector{Vector{UnitRange{Int}}}
    names::Dict{Symbol,Vector{Int}}
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
        caps = deepcopy(cs.captures)
        new{typeof(m),typeof(state)}(m,cs.subroutines,caps,cs.names,state)
    end
end
copy_captures(x::SequenceWithCaptures,state) =
    SequenceWithCaptures(x.match,x.subroutines, [ copy(c) for c in x.captures ],x.names,state)
revert(x::SequenceWithCaptures) = SequenceWithCaptures(revert(x.match),x)
reverse_index(x::SequenceWithCaptures,a...) = reverse_index(x.match,a...)
with_options(flags::UInt32,x::SequenceWithCaptures) =
    SequenceWithCaptures(with_options(flags,x.match),x)
@inline Base.lastindex(x::SequenceWithCaptures) =
    lastindex(x.match)
@inline Base.prevind(x::SequenceWithCaptures,i::Integer,n::Integer) =
    prevind(x.match,i,n)
@inline Base.nextind(x::SequenceWithCaptures,i::Integer,n::Integer) =
    nextind(x.match,i,n)
@inline Base.prevind(x::SequenceWithCaptures,i::Integer) =
    prevind(x.match,i)
@inline Base.nextind(x::SequenceWithCaptures,i::Integer) =
    nextind(x.match,i)
@inline Base.@propagate_inbounds Base.getindex(x::SequenceWithCaptures,i...) =
    getindex(x.match,i...)
@inline Base.@propagate_inbounds Base.iterate(x::SequenceWithCaptures,i...) =
    iterate(x.match,i...)
function Base.show(io::IO, x::SequenceWithCaptures)
    print(io, "SequenceWithCaptures ")
    show(io,x.match)
end
Base.SubString(x::SequenceWithCaptures,a...) =
    SubString(x.match,a...)


export Capture
"""
Capture a parser result, optionally with a name.
`index` field is initialized when calling `ParserWithCaptures` on the parser.

[`ParserWithCaptures`](@ref)
"""
@auto_hash_equals struct Capture{P,T} <: WrappedParser{P,T}
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
    print(io, " |> Capture ", x.index )
end

regex_string(x::Capture) = regex_prefix_(x)*regex_string(x.parser)*")"
regex_prefix_(x::Capture) =
    let name = (x.name===nothing ? "" : "?<$(x.name)>")
        "($name"
    end
regex_prefix(x::Capture) =
    regex_prefix_(x::Capture)*regex_prefix(x.parser)
regex_suffix(x::Capture) = regex_suffix(x.parser)*")"

function deepmap_parser(f::Function,mem::AbstractDict,x::Capture,a...;kw...)
    get!(mem,x) do
        Capture(x.name,deepmap_parser(f,mem,x.parser,a...;kw...),x.index)
    end
end



Base.get(x::Capture, sequence, till, after, i, state) =
    get(x.parser, sequence, till, after, i, state)

@inline function _iterate(parser::Capture, sequence, till, posi, next_i, state)
    r = _iterate(parser.parser, sequence, till, posi, next_i, state)
    if r !== nothing ## set only if found (e.g. if repeated capture capture last)
        set_capture(sequence,parser.index,posi,prevind(sequence,tuple_pos(r)))
    elseif state !== nothing
        prune_captures(sequence, posi)
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
    @inbounds for i in 1:length(sequence.captures)
        @inbounds cv = sequence.captures[i]
        while !isempty(cv) && (@inbounds  cv[end].stop >= after_i)
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
@auto_hash_equals struct Backreference <: LeafParser{AbstractString}
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
    if ( index<0 || delta!=Symbol("") )
        if name !== nothing
            index
        else
            length(context.subroutines)+index+1
        end
    else
        index
    end

function deepmap_parser(::typeof(log_names_),mem::AbstractDict,x::Backreference,a...;kw...)
    get!(mem,x) do
        with_log(regex_string(x),x;kw...)
    end
end

function deepmap_parser(::typeof(indexed_captures_),mem::AbstractDict,x::Backreference,context,a...)
    get!(mem,x) do
        idx = capture_index(x.name,Symbol(""),x.index,context)
        if idx < 1 || idx>lastindex(context.subroutines)
            x.name === nothing ? x.fallback() : x
        else
            Backreference(x.fallback,x.name, idx)
        end
    end
end

function Base.get(x::Backreference, sequence, till, after, i, state)
    sequence[i:prevind(sequence,i+state)]
end

@inline function prevind(str,i::Int,parser::Backreference,x)
    i-x
end

@inline function nextind(str,i::Int,parser::Backreference,x)
    i+x
end


state_type(::Type{<:Backreference}) = Int
@inline function _iterate(p::Backreference, sequence::SequenceWithCaptures, till, posi, next_i, state)
    return nothing
end

function resolve_index(p::Backreference, sequence::SequenceWithCaptures)
    index = p.index
    if index < 0 && p.name !== nothing
        for i in sequence.names[p.name]
            if !isempty(sequence.captures[i])
                return i
            end
        end        
    end
    ( index<0 || isempty(sequence.captures[index]) ) ? -1 : index
end

@inline function _iterate(p::Backreference, sequence::SequenceWithCaptures, till, posi, next_i, state::Nothing)
    index = resolve_index(p, sequence)
    index<0 && return nothing
    sequence.captures[index]
    r = _iterate(
        SubString(sequence.match, sequence.captures[index][end]),
        sequence, till, posi, next_i, state)
    r === nothing && return nothing
    tuple_pos(r), tuple_pos(r)-next_i
end

_iterate_condition(p::Backreference, sequence, till, posi, next_i, state) =
    resolve_index(p, sequence)>0


export Subroutine
"""
Parser matching preceding capture, optionally with a name.
`index` field is recursively set when calling `ParserWithCaptures` on the parser.
"""
@auto_hash_equals struct Subroutine{T} <: LeafParser{T}
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
function regex_prefix(x::Subroutine)
    "(?" *
        if x.name !== nothing
            "&$(x.name),$(x.index)"
        else
            if x.delta == Symbol("")
                string(x.delta)
            else
                ""
            end*string(x.index)
        end
end
regex_suffix(x::Subroutine) = ")"
regex_inner(x::Subroutine) = ""
deepmap_parser(::typeof(revert),mem::AbstractDict,x::Subroutine) = x

function _iterate_condition(cond::Subroutine, sequence, till, posi, next_i, state)
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


"""
    index(parser::Subroutine,sequence)

Index of a subroutine.
["If you make a subroutine call to a non-unique named subpattern, the one that corresponds to the first occurrence of the name is used."](https://www.pcre.org/original/doc/html/pcrepattern.html#SEC16)
(what about "In the absence of duplicate numbers (see the previous section) this is the one with the lowest number."?)
"""
index(parser::Subroutine,sequence) =
    parser.index <= 0 ? first(sequence.names[parser.name]) : parser.index

@inline function _iterate(parser::Subroutine, sequence::SequenceWithCaptures, till, posi, next_i, state)
    _iterate(
        with_log("$(parser.name)", sequence.subroutines[index(parser,sequence)].parser),
        copy_captures(sequence,parser), till, posi, next_i, state)
end
state_type(::Type{<:Subroutine}) = Any



export DupSubpatternNumbers
"""
Parser wrapper for `ParserWithCaptures`, setting reset_index=true in `deepmap_parser(::typeof(indexed_captures_),...)`.

```jldoctest
julia> p = re"(?|(a)|(b))\\1"
🗄 Sequence |> regular expression combinator with 1 capturing groups
├─ |🗄... Either |> DupSubpatternNumbers
│  ├─ (a)  |> Capture 1
│  └─ (b)  |> Capture 1
└─ \\g{1} Backreference
::Tuple{Char,AbstractString}

julia> match(p, "aa")
ParseMatch("aa", 1="a")

julia> match(p, "bb")
ParseMatch("bb", 1="b")

```

See also [pcre doc](https://www.pcre.org/original/doc/html/pcrepattern.html#dupsubpatternnumber)
"""
@auto_hash_equals struct DupSubpatternNumbers{P,T} <: WrappedParser{P,T}
    parser::P
    DupSubpatternNumbers(parser) =
        new{typeof(parser),result_type(parser)}(parser)
end

deepmap_parser(f::Function,mem::AbstractDict,x::DupSubpatternNumbers, a...;kw...) =
    get!(mem,x) do
        DupSubpatternNumbers(deepmap_parser(f,mem,x.parser,a...;kw...))
    end

function deepmap_parser(f::typeof(indexed_captures_),mem::AbstractDict,x::DupSubpatternNumbers,context,reset_index)
    get!(mem,x) do
        DupSubpatternNumbers(deepmap_parser(
            indexed_captures_,mem,
            x.parser,context,
            true))
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
            push!(branches,deepmap_parser(
                indexed_captures_,
                mem,
                p,context,false))
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
@auto_hash_equals struct Conditional{C,Y,N,T} <: CombinedParser{T}
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


function deepmap_parser(f::Function,mem::AbstractDict,x::Conditional,a...;kw...)
    get!(mem,x) do
        Conditional(deepmap_parser(f,mem,x.condition,a...;kw...),
                    deepmap_parser(f,mem,x.yes,a...;kw...),
                    deepmap_parser(f,mem,x.no,a...;kw...))
    end
end

@inline Base.get(parser::Conditional, sequence, till, after, i, state) =
    get(state.first == :yes ? parser.yes : parser.no, sequence, till, after, i, state.second)

_iterate_condition(cond, sequence, till, posi, next_i, state) =
    _iterate(cond, sequence, till, posi, next_i, state) !== nothing

state_type(::Type{Conditional{C,Y,N,T}}) where {C,Y,N,T} =
    Pair{Symbol,Union{state_type(Y),state_type(N)}}


@inline function prevind(str,i::Int,parser::Conditional,state)
    prevind(str,i,state.first == :yes ? parser.yes : parser.no, state.second)
end

@inline function nextind(str,i::Int,parser::Conditional,state)
    nextind(str,i,state.first == :yes ? parser.yes : parser.no, state.second)
end

@inline function _iterate(parser::Conditional, sequence, till, posi, next_i, state::Nothing)
    c = _iterate_condition(parser.condition, sequence, till, posi, next_i, state)
    f = c ? :yes : :no
    cparse = f == :yes ? parser.yes : parser.no
    s = _iterate(cparse,
                 sequence, till, posi, next_i, state)
    s === nothing && return nothing
    tuple_pos(s), f => tuple_state(s)
end

@inline function _iterate(parser::Conditional, sequence, till, posi, next_i, state::Pair)
    _iterate(state.first == :yes ? parser.yes : parser.no, sequence, till, posi, next_i, state.second)
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
@auto_hash_equals struct ParserWithCaptures{P,T} <: WrappedParser{P,T}
    parser::P
    subroutines::Vector{ParserTypes} ## todo: rename subroutines
    names::Dict{Symbol,Vector{Int}}
    ParserWithCaptures(parser,captures,names) =
        new{typeof(parser),result_type(parser)}(parser,captures,names)
end
"""
    ParserWithCaptures(x)

Return `ParserWithCaptures` if captures are used, `x` otherwise.
"""
ParserWithCaptures(x) =
    let cs = ParserWithCaptures(x,ParserTypes[],Dict{Symbol,Int}())
        pass1 = ParserWithCaptures(deepmap_parser(indexed_captures_,NoDict(),x,cs,false),cs.subroutines,cs.names)
        r = ParserWithCaptures(deepmap_parser(indexed_captures_,NoDict(),pass1.parser,pass1,false),pass1.subroutines,pass1.names)
        isempty(r.subroutines) ? r.parser : r
    end


"""
    deepmap_parser(f::typeof(indexed_captures_),mem::AbstractDict,x::Capture,context,a...)

Map the capture my setting `index` to  `nextind(context,x)`.

Registers result in `context.subroutines` if no previous subroutine with the same index exists
(see also [`DupSubpatternNumbers`](@ref)).
"""
function deepmap_parser(f::typeof(indexed_captures_),mem::AbstractDict,x::Capture,context,a...)
    get!(mem,x) do
        index,reset=subroutine_index_reset(context,x)
        r = Capture(
            x.name,
            deepmap_parser(indexed_captures_,mem,x.parser,context,a...),
            index
        )
        reset && ( context.subroutines[index] = r )
        r
    end
end

"""
https://www.pcre.org/original/doc/html/pcrepattern.html#SEC16
"""
function subroutine_index_reset(context::ParserWithCaptures,x::Capture)
    if x.index<0
        index = length(context.subroutines)+1
        push!(context.subroutines,Capture(x,index))
        if x.name !== nothing
            push!(get!(context.names,x.name) do
                  Int[]
                  end,
                  index)
        end
        index, true
    else
        x.index, false
    end
end

import ..CombinedParsers: JoinSubstring, Transformation
JoinSubstring(x::ParserWithCaptures) =
    ParserWithCaptures(JoinSubstring(x.parser),x.subroutines,x.names)
Transformation{T}(t,x::ParserWithCaptures) where T =
    ParserWithCaptures(Transformation{T}(t,x.parser),x.subroutines,x.names)
 
function Base.match(parser::ParserTypes,sequence; log=nothing)
    p = log === nothing ? parser : log_names(parser,log)
    start,till=1, lastindex(sequence)
    i = nothing
    while i===nothing && start <= till+1
        i = _iterate(p,sequence,till,start,start,nothing)
        i === nothing && (start = start <= till ? nextind(sequence,start) : start+1)
    end
    if i === nothing
        nothing
    else
        ParseMatch(parser,sequence,start,i...)
    end
end

"""
    Base.match(parser::ParserTypes,sequence::AbstractString; log=false)

Plug-in replacement for match(::Regex,sequence).
"""
function Base.match(parser::ParserWithCaptures,sequence::AbstractString; kw...)
    match(parser, SequenceWithCaptures(sequence,parser);kw...)
end

"""
    _iterate(p::ParserWithCaptures, sequence::AbstractString)


"""
function _iterate(p::ParserWithCaptures, sequence::AbstractString)
    sequence = SequenceWithCaptures(sequence,p)
    _iterate(p, sequence, lastindex(sequence), 1, 1, nothing)
end

import Base: empty!
Base.empty!(sequence::SequenceWithCaptures) =
    for c in sequence.captures
        Base.empty!(c)
    end

function _iterate(p::ParserWithCaptures, sequence::SequenceWithCaptures)
    Base.empty!(sequence)
    _iterate(p, sequence, lastindex(sequence), 1, 1, nothing)
end


##Base.getindex(x::ParserWithCaptures, i) = ParserWithCaptures(getindex(i,x)

function SequenceWithCaptures(x,cs::ParserWithCaptures)
    SequenceWithCaptures(
        x,cs.subroutines,
        Vector{String}[ String[] for c in cs.subroutines ],
        cs.names,
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

function deepmap_parser(f::Function,mem::AbstractDict,x::ParserWithCaptures,a...;kw...)
    ParserWithCaptures(deepmap_parser(f,mem,x.parser,a...;kw...),
                       [ deepmap_parser(f,mem,p,a...;kw...) for p in x.subroutines ],
                       x.names)
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
@auto_hash_equals struct ParseMatch{P,S,State}
    parser::P
    sequence::S
    start::Int
    stop::Int
    state::State
    function ParseMatch(parser::CombinedParser,s,start::Integer,stop::Integer,state)
        new{typeof(parser),typeof(s),typeof(state)}(parser,s,start,stop,state)
    end
end


"""
    Base.get(x::ParseMatch{<:MatchTuple})

Get the result of a match result.

```jldoctest
julia> m = match(re"(?<a>so)+ (or)", "soso or")
ParseMatch("soso or", a="so", 2="or")

julia> get(m)
"so"

julia> m[2]
"or"

julia> m.match, m.captures
("soso or", SubString{String}["so", "or"])

```
"""
function Base.get(x::ParseMatch)
    get(getfield(x,1), getfield(x,2), getfield(x,3), getfield(x,4), getfield(x,3),
        getfield(x,5))
end

function Base.getproperty(x::ParseMatch{<:Any,<:AbstractString,<:Any},key::Symbol)
    if key==:captures
        AbstractString[ ]
    elseif key==:match
        SubString(getfield(x,2),getfield(x,3),prevind(getfield(x,2),getfield(x,4)))
    end
end

function Base.show(io::IO,m::ParseMatch{<:Any,<:AbstractString,<:Any})
    print(io,"ParseMatch(\"",escape_string(m.match),"\"")
    print(io,")")
end


function Base.getindex(x::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},i::Integer)
    m = getfield(x,2)
    c = m.captures[i]
    isempty(c) ? nothing : match_string(m.match,c[end])
end

function Base.getindex(m::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},i::Symbol)
    x = getfield(m,2)
    for j in x.names[i]
        c=getindex(m,j)
        c !== nothing && return c
    end
end

match_string(x::SubString,y::UnitRange{<:Integer}) =
    SubString(x.string,x.offset+y.start,x.offset+y.stop)

match_string(x::Tuple{<:AbstractString,UnitRange{<:Integer}},y::UnitRange{<:Integer}) =
    let rel = min(x[2].start,x[2].stop)-1
        SubString(x[1],y.start,y.stop)
    end

match_string(x::AbstractString,y::UnitRange{<:Integer}) =
    SubString(x,y.start,y.stop)

import Base: getproperty
function Base.getproperty(x::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},key::Symbol)
    m = getfield(x,2)
    start = getfield(x,3)
    stop = getfield(x,4)
    if key==:captures
        [ isempty(c) ? nothing : match_string(m.match,c[end])
          for c in m.captures ]
    elseif key==:match
        SubString(m.match,start,prevind(m,stop))
    end
end

function Base.show(io::IO,m::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any})
    x = getfield(m,2)
    print(io,"ParseMatch(\"",escape_string(m.match),"\"")
    indnames=Dict( ( i=>k.first for k in pairs(x.names) for i in k.second )... )
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
