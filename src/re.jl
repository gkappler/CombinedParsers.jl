"""
A regular expression parser transforming a PCRE string to a CombinedParser equivalent to the regular expression.
"""
module Regexp
using ..CombinedParsers
using TextParse
import TextParse: AbstractToken
using AutoHashEquals

using ReversedStrings
import ReversedStrings: reversed, reverse_index

import ..CombinedParsers: LeafParser, WrappedParser, ParserTypes, ConstantParser, LookAround, Either, SideeffectParser, MatchingNever
import ..CombinedParsers: parser, prune_captures, deepmap_parser, _iterate, print_constructor
import ..CombinedParsers: regex_prefix, regex_suffix, regex_inner, regex_string_, regex_string, log_names_
import ..CombinedParsers: state_type, start_index, tuple_pos, tuple_state
import ..CombinedParsers: _prevind, _nextind
indexed_captures_(x,a...) = x

import Base: SubString, ==

include("pcre.jl")

"""
SequenceWithCaptures ensapsulates a sequence to be parsed, and parsed captures.

This struct will allow for captures a sequence-level state.
For next version, a match-level state passed as _iterate argument is considered.

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
import Base: empty!
Base.empty!(sequence::SequenceWithCaptures) =
    for c in sequence.captures
        Base.empty!(c)
    end
copy_captures(x::SequenceWithCaptures,state) =
    SequenceWithCaptures(x.match,x.subroutines, [ copy(c) for c in x.captures ],x.names,state)
reversed(x::SequenceWithCaptures) = SequenceWithCaptures(reversed(x.match),x)
reverse_index(x::SequenceWithCaptures,a...) = reverse_index(x.match,a...)
with_options(flags::UInt32,x::SequenceWithCaptures) =
    SequenceWithCaptures(with_options(flags,x.match),x)
@inline Base.lastindex(x::SequenceWithCaptures) =
    lastindex(x.match)
@inline Base.@propagate_inbounds Base.prevind(x::SequenceWithCaptures,i::Integer,n::Integer) =
    prevind(x.match,i,n)
@inline Base.@propagate_inbounds Base.nextind(x::SequenceWithCaptures,i::Integer,n::Integer) =
    nextind(x.match,i,n)
@inline Base.@propagate_inbounds Base.prevind(x::SequenceWithCaptures,i::Integer) =
    prevind(x.match,i)
@inline Base.@propagate_inbounds Base.nextind(x::SequenceWithCaptures,i::Integer) =
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

with_options(set_flags::UInt32, unset_flags::UInt32,x::SequenceWithCaptures) =
    SequenceWithCaptures(
        with_options(set_flags, unset_flags,x.match),
        x
    )

"""
    ==(pcre_m::RegexMatch,pc_m::ParseMatch)

equal iif values of `.match`, `.offset`, `.ncodeunits` and `.captures` are equal.
"""
function ==(pcre_m::RegexMatch,pc_m::ParseMatch)
    pcre_m.match==pc_m.match &&
        pcre_m.match.offset==pc_m.match.offset &&
        pcre_m.match.ncodeunits==pc_m.match.ncodeunits &&
        pcre_m.captures==pc_m.captures
end
==(pc_m::ParseMatch,pcre_m::RegexMatch) =
    pcre_m==pc_m

import Base: getproperty
"""
    Base.getproperty(m::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},key::Symbol)

enable `m.captures` and `m.match`.

See API of `RegexMatch`.
"""
function Base.getproperty(m::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},key::Symbol)
    x = getfield(m,1).sequence
    if key==:captures
        [ isempty(c) ? nothing : match_string(x.match,c[end])
          for c in x.captures ]
    elseif key==:match
        SubString(x.match,m.start,
                  prevind(x.match,m.stop))
    else
        CombinedParsers._getproperty(m,key)
    end
end

"""
    Base.getindex(x::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},i::Union{Integer,Symbol})

Gets capture `i` as SubString.

See API of `RegexMatch`.
"""
function Base.getindex(x::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},i::Integer)
    m = getfield(x,1).sequence
    c = m.captures[i]
    isempty(c) ? nothing : match_string(m.match,c[end])
end

function Base.getindex(m::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},i::Symbol)
    x = getfield(m,1).sequence
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

function Base.show(io::IO,m::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any})
    x = getfield(m,1).sequence
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


export Capture
"""
Capture a parser result, optionally with a name.
`index` field is initialized when calling `ParserWithCaptures` on the parser.

[`ParserWithCaptures`](@ref)
"""
@auto_hash_equals struct Capture{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    name::Union{Nothing,Symbol}
    index::Int
    Capture(name::Union{Nothing,Symbol},x_,index=-1) =
        let x = parser(x_)
            new{typeof(x),state_type(x),result_type(x)}(x,name==Symbol("") ? nothing : name,index)
        end
    Capture(x::Capture,index) =
        new{typeof(x.parser),state_type(x),result_type(x)}(x.parser,x.name,index)
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
    push!((@inbounds sequence.captures[index]), start:stop)
set_capture(sequence::SequenceWithCaptures{<:ReversedString}, index::Int, start,stop) =
    push!((@inbounds sequence.captures[index]),
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
@auto_hash_equals struct Backreference <: LeafParser{Int,AbstractString}
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


function Base.get(x::Backreference, sequence, till, after, i, state)
    sequence[i:prevind(sequence,i+state)]
end

@inline function prevind(str,i::Int,parser::Backreference,x)
    i-x
end

@inline function nextind(str,i::Int,parser::Backreference,x)
    i+x
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

capture_substring(p::ParserOptions{<:Backreference}, sequence::SequenceWithCaptures) =
    with_options(p.set_flags, p.unset_flags,capture_substring(p.parser, sequence))

function capture_substring(p::Backreference, sequence::SequenceWithCaptures)
    index = resolve_index(p, sequence)
    index<0 && return nothing
    SubString(sequence.match, sequence.captures[index][end])
end

@inline function _iterate(
    p::Union{Backreference,ParserOptions{<:Backreference}},
    sequence::SequenceWithCaptures, till,
    posi, next_i, state::Nothing)
    r = _iterate(
        capture_substring(p, sequence),
        sequence, till, posi, next_i, state)
    r === nothing && return nothing
    tuple_pos(r), tuple_pos(r)-next_i
end

@inline function _iterate(
    p::Union{Backreference,ParserOptions{<:Backreference}},
    sequence::SequenceWithCaptures, till,
    posi, next_i, state)
    return nothing
end


@inline function _iterate(
    p::Nothing,
    sequence, till,
    posi, next_i, state)
    return nothing
end


_iterate_condition(p::Backreference, sequence, till, posi, next_i, state) =
    resolve_index(p, sequence)>0


export Subroutine
"""
Parser matching preceding capture, optionally with a name.
`index` field is recursively set when calling `ParserWithCaptures` on the parser.
"""
@auto_hash_equals struct Subroutine{S,T} <: LeafParser{S,T}
    name::Union{Nothing,Symbol}
    delta::Symbol
    index::Int
    Subroutine{S,T}(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) where {S,T} =
        new{S,T}(name,delta,index)
    Subroutine(name::Union{Nothing,Symbol},delta::Symbol,index::Integer) =
        new{Any,Any}(name,delta,index)
    Subroutine(index::Int) =
        new{Any,Any}(nothing,Symbol(""),index)
    Subroutine(name::AbstractString) =
        new{Any,Any}(Symbol(name),Symbol(""),-1)
    Subroutine() =
        new{Any,Any}(nothing,Symbol(""),-1)
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
deepmap_parser(::typeof(reversed),mem::AbstractDict,x::Subroutine) = x

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
        sequence.subroutines[index(parser,sequence)].parser,
        copy_captures(sequence,parser), till, posi, next_i, state)
end



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
@auto_hash_equals struct DupSubpatternNumbers{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    DupSubpatternNumbers(parser) =
        new{typeof(parser),state_type(parser),result_type(parser)}(parser)
end

deepmap_parser(f::Function,mem::AbstractDict,x::DupSubpatternNumbers, a...;kw...) =
    get!(mem,x) do
        DupSubpatternNumbers(deepmap_parser(f,mem,x.parser,a...;kw...))
    end



export Conditional
"""
Conditional parser, `_iterate` cycles conditionally on `_iterate_condition` through matches in field `yes` and `no` respectively.
"""
@auto_hash_equals struct Conditional{C,Y,N,S,T} <: CombinedParser{S,T}
    condition::C
    yes::Y
    no::N
    Conditional(condition,yes,no) =
        new{typeof(condition),typeof(yes),typeof(no),
            Pair{Symbol,Union{state_type(yes),state_type(no)}},
            Union{result_type(yes),result_type(no)}}(condition,yes,no)
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

include("indexed_captures.jl")
 




include("re-parser.jl")




end
