export ParserWithCaptures, SequenceWithCaptures
"""
Top level parser supporting regular expression features
captures, backreferences and subroutines.
Collects subroutines in field `subroutines::Vector` and 
indices of named capture groups in field `names::Dict`.

!!! note
    implicitly called in [`match`](@ref)
See also [`Backreference`](@ref), [`Capture`](@ref), [`Subroutine`](@ref)
"""
@auto_hash_equals struct ParserWithCaptures{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    subroutines::Vector{CombinedParser} ## todo: rename subroutines
    names::Dict{Symbol,Vector{Int}}
    ParserWithCaptures(parser,captures,names) =
        new{typeof(parser),state_type(parser),result_type(parser)}(parser,captures,names)
end
function print_constructor(io::IO, x::ParserWithCaptures)
    print_constructor(io,x.parser)
    print(io, " |> regular expression combinator",
          ( length(x.subroutines)>0 ? " with $(length(x.subroutines)) capturing groups" : "" ) )
end
"""
    _iterate(p::ParserWithCaptures, sequence::SequenceWithCaptures,a...)

`Base.empty!(sequence)` before iteration.
(Why?)
"""
function _iterate(p::ParserWithCaptures, sequence::SequenceWithCaptures,a...)
    Base.empty!(sequence)
    _iterate(p.parser, sequence, a...)
end

"""
    ParserWithCaptures(x)

Return `ParserWithCaptures` if captures are used, `x` otherwise.
Two passes of `deepmap_parser(_indexed_captures,...)` are used 
(1. to assign `Capture` indices and 
 2. to use index number for `Backreference` and `Subroutine`).

See also [`_indexed_captures`](@ref)
"""
function ParserWithCaptures(x)
    cs = ParserWithCaptures(x,CombinedParser[],Dict{Symbol,Int}())
    pass1 = ParserWithCaptures(deepmap_parser(_indexed_captures,NoDict(),x,cs,false),cs.subroutines,cs.names)
    r = ParserWithCaptures(deepmap_parser(_indexed_captures,NoDict(),pass1.parser,pass1,false),pass1.subroutines,pass1.names)
    isempty(r.subroutines) ? r.parser : r
end

# _iterate(parser::ParserWithCaptures, sequence::AbstractString, till, next_i, after, state::Nothing) =
#     _iterate(parser, sequence, till, next_i, next_i, state)

SequenceWithCaptures(x,cs::CombinedParser) = x
function SequenceWithCaptures(x,cs::ParserWithCaptures)
    ## @show S=typeof(x)
    SequenceWithCaptures(
        x,cs.subroutines,
        Vector{String}[ String[] for c in cs.subroutines ],
        cs.names,
        nothing)
end



ParseMatchWithCaptures = ParseMatch{<:ParserWithCaptures,<:SequenceWithCaptures,<:Any}
function Base.show(io::IO,m::ParseMatchWithCaptures)
    x = getfield(m,1).sequence
    print(io,"ParseMatch(\"",
          m.state === nothing ? "no match" : escape_string(m.match),
          "\"")
    indnames=Dict( ( i=>k.first for k in pairs(x.names) for i in k.second )... )
    for i in 1:length(x.captures)
        print(io, ", ",get(indnames,i,i),"=")
        if isempty(x.captures[i])
            print(io,"nothing")
        else
            print(io,"\"",match_string(x.x, x.captures[i][end]),"\"")
        end
    end
    print(io,")")
end


import Base: getproperty
"""
    Base.getproperty(m::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},key::Symbol)

enable `m.captures` and `m.match`.

See API of `RegexMatch`.
"""
function Base.getproperty(m::ParseMatchWithCaptures,key::Symbol)
    x = getfield(m,1).sequence
    if key==:captures
        [ isempty(c) ? nothing : match_string(x.x,c[end])
          for c in x.captures ]
    elseif key==:match
        SubString(x.x,m.offset,
                  _prevind(x.x,m.after))
    else
        CombinedParsers._getproperty(m,key)
    end
end

"""
    Base.getindex(x::ParseMatch{<:Any,<:SequenceWithCaptures,<:Any},i::Union{Integer,Symbol})

Gets capture `i` as SubString.

See API of `RegexMatch`.
"""
function Base.getindex(x::ParseMatchWithCaptures,i::Integer)
    m = getfield(x,1).sequence
    c = m.captures[i]
    isempty(c) ? nothing : match_string(m.x,c[end])
end

function Base.getindex(m::ParseMatchWithCaptures,i::Symbol)
    x = getfield(m,1).sequence
    for j in x.names[i]
        c=getindex(m,j)
        c !== nothing && return c
    end
end


import ..CombinedParsers: MatchesIterator
"""
    MatchesIterator(parser::ParserWithCaptures, sequence, start=firstindex(sequence),stop=lastindex(sequence),till=lastindex(sequence))
    MatchesIterator(parser::ParserWithCaptures, sequence::SequenceWithCaptures, start=firstindex(sequence),stop=lastindex(sequence),till=lastindex(sequence))

If not `sequence isa SequenceWithCaptures` it is wrapped in such for `parser`.
"""
MatchesIterator(parser::ParserWithCaptures, sequence::SequenceWithCaptures, start::Int=firstindex(sequence),stop::Int=lastindex(sequence),till::Int=lastindex(sequence)) =
    MatchesIterator{typeof(parser),typeof(sequence)}(parser, sequence,start,stop,till)
MatchesIterator(parser::ParserWithCaptures, sequence, start::Int=firstindex(sequence),stop::Int=lastindex(sequence),till::Int=lastindex(sequence)) =
    MatchesIterator(parser, SequenceWithCaptures(sequence,parser),start,stop,till)

set_options(set::UInt32,unset::UInt32,parser::ParserWithCaptures) =
    ParserWithCaptures(set_options(set,unset,parser.parser),
                       CombinedParser[ set_options(set,unset,p) for p in parser.subroutines],
                       parser.names)

function _deepmap_parser(f::Function,mem::AbstractDict,x::ParserWithCaptures,a...;kw...)
    ParserWithCaptures(deepmap_parser(f,mem,x.parser,a...;kw...),
                       [ deepmap_parser(f,mem,p,a...;kw...) for p in x.subroutines ],
                       x.names)
end

ParserWithCaptures(x::ParserWithCaptures) = x

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

import ..CombinedParsers: MatchedSubSequence, Transformation
MatchedSubSequence(x::ParserWithCaptures) =
    ParserWithCaptures(MatchedSubSequence(x.parser),x.subroutines,x.names)
Transformation{T}(t,x::ParserWithCaptures) where T =
    ParserWithCaptures(Transformation{T}(t,x.parser),x.subroutines,x.names)


function _deepmap_parser(::typeof(_indexed_captures),mem::AbstractDict,x::Backreference,context,a...)
    idx = capture_index(x.name,Symbol(""),x.index,context)
    if idx < 1 || idx>lastindex(context.subroutines)
        x.name === nothing ? x.fallback() : x
    else
        Backreference(x.fallback,x.name, idx)
    end
end
function _deepmap_parser(::typeof(_indexed_captures),mem::AbstractDict,x::Subroutine,context,a...)
    index = capture_index(x.name,x.delta,x.index, context)
    if index <= 0 || index>length(context.subroutines)
        Subroutine{Any,Any}(x.name,Symbol(""),index)
    else
        sr = context.subroutines[index]
        Subroutine{state_type(sr),result_type(sr)}(
            x.name,Symbol(""),index)
    end
end

"""
    _deepmap_parser(f::typeof(_indexed_captures),mem::AbstractDict,x::DupSubpatternNumbers,context,reset_index)

set `reset_index===true'.
"""
function _deepmap_parser(f::typeof(_indexed_captures),mem::AbstractDict,x::DupSubpatternNumbers,context,reset_index)
    DupSubpatternNumbers(deepmap_parser(_indexed_captures,mem,x.parser,context,true))
end

"""
    _deepmap_parser(::typeof(_indexed_captures),mem::AbstractDict,x::Either,context,reset_index)

Method dispatch, resetting `lastindex(context.subroutines)` if `reset_index===true'.
"""
deepmap_parser(::typeof(_indexed_captures),mem::AbstractDict,x::Either{<:Tuple},context,reset_index) =
    _indexed_captures(mem,x,context,reset_index)
deepmap_parser(::typeof(_indexed_captures),mem::AbstractDict,x::Either{<:Vector},context,reset_index) =
    _indexed_captures(mem,x,context,reset_index)
function _indexed_captures(mem::AbstractDict,x::Either,context,reset_index)
    if reset_index
        idx = lastindex(context.subroutines)
        branches = Any[]
        for p in reverse(x.options) ## keep first for subroutines
            while lastindex(context.subroutines)>idx
                pop!(context.subroutines)
            end
            push!(branches,deepmap_parser(
                _indexed_captures,
                mem,
                p,context,false))
        end
        Either{result_type(x)}(tuple( branches... ))
    else
        Either{result_type(x)}(
            tuple( (deepmap_parser(_indexed_captures,mem,p,context,false) for p in x.options )...))
    end
end

"""
    _deepmap_parser(f::typeof(_indexed_captures),mem::AbstractDict,x::Capture,context,a...)

Map the capture my setting `index` to  `_nextind(context,x)`.

Registers result in `context.subroutines` if no previous subroutine with the same index exists
(see also [`DupSubpatternNumbers`](@ref)).
"""
function _deepmap_parser(f::typeof(_indexed_captures),mem::AbstractDict,x::Capture,context,a...)
    index,reset=subroutine_index_reset(context,x)
    r = Capture(
        x.name,
        deepmap_parser(_indexed_captures,mem,x.parser,context,a...),
        index
    )
    reset && ( context.subroutines[index] = r )
    r
end

##Base.getindex(x::ParserWithCaptures, i) = ParserWithCaptures(getindex(i,x)

"For use in ParserWithCaptures to enforce different indices for identical captures."
struct NoDict{K,V} <: AbstractDict{K,V} end
NoDict() = NoDict{Any,Any}()

import Base: get!
Base.get!(f::Function,d::NoDict,k) = f()
