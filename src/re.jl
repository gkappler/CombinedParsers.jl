"""
A regular expression parser transforming a PCRE string to a CombinedParser equivalent to the regular expression.
"""
module Regexp
using ..CombinedParsers
import TextParse
import TextParse: AbstractToken
using AutoHashEquals

using LazyStrings
import LazyStrings: reversed, reverse_index

import ..CombinedParsers: LeafParser, WrappedParser, CombinedParser, ConstantParser, Either, SideeffectParser
import ..CombinedParsers: parser, prune_captures, deepmap_parser, _deepmap_parser, print_constructor
import ..CombinedParsers: _iterate, _iterate_constant
import ..CombinedParsers: regex_prefix, regex_suffix, regex_inner, _regex_string, regex_string, _log_names
import ..CombinedParsers: state_type, leftof, tuple_pos, tuple_state
import ..CombinedParsers: _prevind, _nextind, _leftof, _rightof
_indexed_captures(x,a...) = x

import Base: SubString, ==

include("pcre.jl")

"""
SequenceWithCaptures ensapsulates a sequence to be parsed, and parsed captures.

This struct will allow for captures a sequence-level state.
For next version, a match-level state passed as _iterate argument is considered.

See also [`ParserWithCaptures`](@ref)
"""
@auto_hash_equals struct SequenceWithCaptures{S,T} <: StringWrapper
    x::S
    subroutines::Vector{CombinedParser}
    captures::Vector{Vector{UnitRange{Int}}}
    names::Dict{Symbol,Vector{Int}}
    state::T
    function SequenceWithCaptures(x,subroutines, captures, names, state)
        new{typeof(x),typeof(state)}(x,subroutines, captures, names, state)
    end
    SequenceWithCaptures(x,cs::SequenceWithCaptures,state) =
        let S=typeof(x)
            new{S,typeof(state)}(x,cs.subroutines,cs.captures,cs.names,state)
        end
    SequenceWithCaptures(x,cs::SequenceWithCaptures) =
        let S=typeof(x)
            new{S,typeof(cs.state)}(x,cs.subroutines,cs.captures,cs.names,cs.state)
        end
end
import Base: empty!
Base.empty!(sequence::SequenceWithCaptures) =
    for c in sequence.captures
        Base.empty!(c)
    end
copy_captures(x::SequenceWithCaptures,state) =
    SequenceWithCaptures(x.x,x.subroutines, [ copy(c) for c in x.captures ],x.names,state)
reversed(x::SequenceWithCaptures) = SequenceWithCaptures(reversed(x.x),x)
reverse_index(x::SequenceWithCaptures,a...) = reverse_index(x.x,a...)
with_options(flags::UInt32,x::SequenceWithCaptures) =
    SequenceWithCaptures(with_options(flags,x.x),x)
function Base.show(io::IO, x::SequenceWithCaptures)
    print(io, "SequenceWithCaptures ")
    show(io,x.x)
end

with_options(set_flags::UInt32, unset_flags::UInt32,x::SequenceWithCaptures) =
    SequenceWithCaptures(
        with_options(set_flags, unset_flags,x.x),
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


match_string(x::SubString,y::UnitRange{<:Integer}) =
    SubString(x.string,x.offset+y.start,x.offset+y.stop)

match_string(x::Tuple{<:AbstractString,UnitRange{<:Integer}},y::UnitRange{<:Integer}) =
    let rel = min(x[2].start,x[2].stop)-1
        SubString(x[1],y.start,y.stop)
    end

match_string(x::AbstractString,y::UnitRange{<:Integer}) =
    SubString(x,y.start,y.stop)

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

regex_string(x::Capture) = _regex_prefix(x)*regex_string(x.parser)*")"
_regex_prefix(x::Capture) =
    let name = (x.name===nothing ? "" : "?<$(x.name)>")
        "($name"
    end
regex_prefix(x::Capture) =
    _regex_prefix(x::Capture)*regex_prefix(x.parser)
regex_suffix(x::Capture) = regex_suffix(x.parser)*")"

function _deepmap_parser(f::Function,mem::AbstractDict,x::Capture,a...;kw...)
    Capture(x.name,deepmap_parser(f,mem,x.parser,a...;kw...),x.index)
end


Base.get(x::Capture, sequence, till, after, i, state) =
    get(x.parser, sequence, till, after, i, state)

@inline function _iterate(parser::Capture, sequence, till, posi, next_i, state)
    r = _iterate(parser.parser, sequence, till, posi, next_i, state)
    if r !== nothing ## set only if found (e.g. if repeated capture capture last)
        set_capture(sequence,parser.index,posi,_prevind(sequence,tuple_pos(r)))
    elseif state !== nothing
        prune_captures(sequence, posi)
    end
    r
end

set_capture(sequence::AbstractString, index::Int, start, stop) = nothing
set_capture(sequence::StringWithOptions, index::Int, start,stop) =
    set_capture(sequence.x,index,start,stop)
set_capture(sequence::SequenceWithCaptures, index::Int, start,stop) =
    push!((@inbounds sequence.captures[index]), start:stop)
set_capture(sequence::SequenceWithCaptures{<:ReversedString}, index::Int, start,stop) =
    push!((@inbounds sequence.captures[index]),
          reverse_index(sequence.x,
                        stop):reverse_index(sequence.x,
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

_regex_string(x::Backreference) =
   if x.name !== nothing
       string(x.name)
   else
       string(x.index)
   end
                                                          
regex_inner(x::Backreference) =
    "\\g{"*_regex_string(x) *"}"

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

function Base.get(x::Backreference, sequence, till, after, i, state)
    sequence[i:_prevind(sequence,i+state)]
end

@inline function _leftof(str,i,parser::Backreference,x)
    i-x
end

@inline function _rightof(str,i,parser::Backreference,x)
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
    SubString(sequence.x, sequence.captures[index][end])
end

@inline function _iterate(p::Union{Backreference,ParserOptions{<:Backreference}},
                          sequence::SequenceWithCaptures, till,
                          posi, next_i, state::Nothing)
    r = _iterate_constant(
        ConstantParser(capture_substring(p, sequence)),
        sequence, till, posi, next_i, state)
    r === nothing && return nothing
    tuple_pos(r), tuple_pos(r)-next_i
end

@inline function _iterate(p::Union{Backreference,ParserOptions{<:Backreference}},
                          sequence::SequenceWithCaptures, till,
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
@auto_hash_equals struct Subroutine{S,T} <: CombinedParser{S,T}
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

_deepmap_parser(::Function,mem::AbstractDict,x::Subroutine) = x


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


@inline function _leftof(sequence,i,parser::Subroutine,x)
    _leftof(sequence,i,sequence.subroutines[index(parser,sequence)].parser,x)
end

@inline function _rightof(sequence,i,parser::Subroutine,x)
    _rightof(sequence,i,sequence.subroutines[index(parser,sequence)].parser,x)
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
Parser wrapper for `ParserWithCaptures`, setting reset_index=true in `deepmap_parser(::typeof(_indexed_captures),...)`.

```jldoctest
julia> p = re"(?|(a)|(b))\\1"
🗄 Sequence |> regular expression combinator with 1 capturing groups
├─ |🗄 Either |> DupSubpatternNumbers
│  ├─ (a)  |> Capture 1
│  └─ (b)  |> Capture 1
└─ \\g{1} Backreference
::Tuple{Char, AbstractString}

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

_deepmap_parser(f::Function,mem::AbstractDict,x::DupSubpatternNumbers, a...;kw...) =
    DupSubpatternNumbers(deepmap_parser(f,mem,x.parser,a...;kw...))



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
    "(?("*_regex_string(x.condition)*")"
end
function regex_suffix(x::Conditional)
    ")"
end
regex_inner(x::Conditional) =
    regex_string(x.yes)*(isa(x.no, Always) ? "" : ( "|" * regex_string(x.no)))


children(x::Conditional) = x.no isa Always ? tuple(x.yes) : tuple(x.yes,x.no)

function _deepmap_parser(f::Function,mem::AbstractDict,x::Conditional,a...;kw...)
    Conditional(deepmap_parser(f,mem,x.condition,a...;kw...),
                deepmap_parser(f,mem,x.yes,a...;kw...),
                deepmap_parser(f,mem,x.no,a...;kw...))
end

@inline Base.get(parser::Conditional, sequence, till, after, i, state) =
    get(state.first == :yes ? parser.yes : parser.no, sequence, till, after, i, state.second)

_iterate_condition(cond::WrappedParser, sequence, till, posi, next_i, state) =
    _iterate_condition(cond.parser, sequence, till, posi, next_i, state)
_iterate_condition(cond, sequence, till, posi, next_i, state) =
    _iterate(cond, sequence, till, posi, next_i, state) !== nothing



@inline function _leftof(str,i,parser::Conditional,state)
    leftof(str,i,state.first == :yes ? parser.yes : parser.no, state.second)
end

@inline function _rightof(str,i,parser::Conditional,state)
    rightof(str,i,state.first == :yes ? parser.yes : parser.no, state.second)
end

@inline function _iterate(parser::Conditional, sequence, till, posi, next_i, state::Nothing)
    c = _iterate_condition(parser.condition, sequence, till, posi, next_i, state)
    cparse = c ? parser.yes : parser.no
    s = _iterate(cparse,
                 sequence, till, posi, next_i, state)
    s === nothing && return nothing
    tuple_pos(s), (c ? :yes : :no) => tuple_state(s)
end

@inline function _iterate(parser::Conditional, sequence, till, posi, next_i, state::Pair)
    _iterate(state.first == :yes ? parser.yes : parser.no, sequence, till, posi, next_i, state.second)
end

include("indexed_captures.jl")
 




include("re-parser.jl")




end
