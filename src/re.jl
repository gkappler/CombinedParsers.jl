include("pcre.jl")

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



import Base: SubString, ==

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
copy_captures(x::WithCaptures,state) =
    WithCaptures(x.match,x.subroutines, [ copy(c) for c in x.captures ],x.names,state)
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

Base.SubString(x::WithCaptures,a...) =
    SubString(x.match,a...)



"""
Capture a parser result, optionally with a name.
`index` field is recursively set when calling 'indexed_captures` on the parser.
"""
struct Capture{P,T} <: WrappedParser{P,T}
    parser::P
    name::Union{Nothing,Symbol}
    index::Int
    Capture(name::Union{Nothing,Symbol},x,index=-1) =
        new{typeof(x),result_type(x)}(x,name==Symbol("") ? nothing : name,index)
    Capture(x::Capture,index) =
        new{typeof(x.parser),result_type(x)}(x.parser,x.name,index)
end
capture(x,index=-1) =
    Capture(nothing,x,index)
capture(name::Union{Nothing,Symbol},x,index=-1) =
    Capture(name,parser(x),index)
function print_constructor(io::IO,x::Capture)
    print_constructor(io,x.parser)
    print(io, " |> capture" )
end

regex_prefix(x::Capture) =
    let name = (x.name===nothing ? "" : "?<$(x.name)>")
        "($name"
    end*regex_prefix(x.parser)
regex_suffix(x::Capture) = regex_suffix(x.parser)*")"

function map_parser(f::Function,mem::AbstractDict,x::Capture,a...)
    get!(mem,x) do
        capture(x.name,map_parser(f,mem,x.parser,a...),x.index)
    end
end

function map_parser(f::typeof(indexed_captures),mem::AbstractDict,x::Capture,context,a...)
    get!(mem,x) do
        index=nextind(context,x)
        context.subroutines[index]=capture(x.name,map_parser(indexed_captures,mem,x.parser,context,a...),index)
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

set_capture(sequence::AbstractString, index::Int, start, stop) =
    @warn "not setting capture $index=$(sequence[start:stop])"
set_capture(sequence::WithOptions, index::Int, start,stop) =
    set_capture(sequence.x,index,start,stop)
set_capture(sequence::WithCaptures, index::Int, start,stop) =
    push!(sequence.captures[index], start:stop)
set_capture(sequence::WithCaptures{<:Reverse}, index::Int, start,stop) =
    push!(sequence.captures[index],
          reverse_index(sequence.match,
                        stop):reverse_index(sequence.match,
                                            start))

function prune_captures(sequence,after_i)
end
function prune_captures(sequence::WithCaptures,after_i)
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
`index` field is recursively set when calling 'indexed_captures` on the parser.
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

function map_parser(::typeof(with_name),mem::AbstractDict,x::Backreference,a...)
    get!(mem,x) do
        with_log("backreference $x",x)
    end
end

function map_parser(::typeof(indexed_captures),mem::AbstractDict,x::Backreference,context,a...)
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
@inline function _iterate(p::Backreference, sequence::WithCaptures, till, i, state)
    return nothing
end

@inline function _iterate(p::Backreference, sequence::WithCaptures, till, i, state::Nothing)
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
`index` field is recursively set when calling 'indexed_captures` on the parser.
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
map_parser(::typeof(revert),mem::AbstractDict,x::Subroutine) = x

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

function map_parser(::typeof(indexed_captures),mem::AbstractDict,x::Subroutine,context,a...)
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

@inline function _iterate(parser::Subroutine, sequence::WithCaptures, till, i, state)
    _iterate(
        with_log("$(parser.name)",sequence.subroutines[index(parser,sequence)].parser),
        copy_captures(sequence,parser), till, i, state)
end
state_type(::Type{<:Subroutine}) = Any



export DupSubpatternNumbers
"""
Parser wrapper for `indexed_captures`, setting reset_index=true in map_parser(::typeof(indexed_captures),...).
"""
struct DupSubpatternNumbers{P,T} <: WrappedParser{P,T}
    parser::P
    DupSubpatternNumbers(parser) =
        new{typeof(parser),result_type(parser)}(parser)
end

function map_parser(f::typeof(indexed_captures),mem::AbstractDict,x::DupSubpatternNumbers,context,reset_index)
    get!(mem,x) do
        DupSubpatternNumbers(map_parser(indexed_captures,mem,x.parser,context,true))
    end
end

function map_parser(::typeof(indexed_captures),mem::AbstractDict,x::Either,context,reset_index)
    if reset_index
        idx = lastindex(context.subroutines)
        branches = Any[]
        for p in reverse(x.options) ## keep first for subroutines
            while lastindex(context.subroutines)>idx
                pop!(context.subroutines)
            end
            push!(branches,map_parser(indexed_captures,mem,p,context,false))
        end
        Either{result_type(x)}(tuple( branches... ))
    else
        Either{result_type(x)}(
            tuple( (map_parser(indexed_captures,mem,p,context,false) for p in x.options )...))
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
    "(?("*regex_string_(x.condition)*")"*regex_string(x.yes)*(isa(x.no, Always) ? "" : ( "|" * regex_string(x.no)))*")"
end
function regex_suffix(x::Conditional)
    ")"
end
function regex_string(x::Conditional)
    "(?("*regex_string_(x.condition)*")"*regex_string(x.yes)*(isa(x.no, Always) ? "" : ( "|" * regex_string(x.no)))*")"
end


printnode(io::IO,x::Conditional{<:Backreference}) = print(io,"(?(",regex_string_(x.condition),")")
printnode(io::IO,x::Conditional) = print(io,"(?",regex_string(x.condition),"")
children(x::Conditional) = x.no isa Always ? tuple(x.yes) : tuple(x.yes,x.no)


function map_parser(::typeof(with_name),mem::AbstractDict,x::Conditional,a...)
    get!(mem,x) do
        with_log("conditional $(regex_string(x))",x)
    end
end

function map_parser(::typeof(indexed_captures),mem::AbstractDict,x::Conditional,context,a...)
    get!(mem,x) do
        Conditional(map_parser(indexed_captures,mem,x.condition,context,a...),
                    map_parser(indexed_captures,mem,x.yes,context,a...),
                    map_parser(indexed_captures,mem,x.no,context,a...))
    end
end

regex_string(x::SideeffectParser) = regex_string(x.parser)


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
Regular expression top level parser with fields `subroutines::Vector`, and a `names::Dict`.
"""
struct ParserWithCaptures{P,T} <: WrappedParser{P,T}
    parser::P
    subroutines::Vector{ParserTypes} ## todo: rename subroutines
    names::Dict{Symbol,Int}
    ParserWithCaptures(parser) =
        new{typeof(parser),result_type(parser)}(parser,ParserTypes[],Dict{Symbol,Int}())
    ParserWithCaptures(parser,captures,names) =
        new{typeof(parser),result_type(parser)}(parser,captures,names)
end

WithCaptures(x,cs::ParserWithCaptures) =
    let S=typeof(x)
        WithCaptures(x,cs.subroutines,
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

function map_parser(f::Function,mem::AbstractDict,x::ParserWithCaptures,a...)
    ParserWithCaptures(map_parser(f,mem,x.parser,a...),
                       [ map_parser(f,mem,p,a...) for p in x.subroutines ],
                       x.names)
end

function Base.nextind(context::ParserWithCaptures,x::Capture)
    if x.name!==nothing
        if haskey(context.names,x.name)
            return @show context.names[x.name]
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

indexed_captures(x::ParserWithCaptures) = x

"For use in indexed_captures to enforce different indices for identical captures."
struct NoDict{K,V} <: AbstractDict{K,V} end
NoDict() = NoDict{Any,Any}()

import Base: get!
Base.get!(f::Function,d::NoDict,k) = f()

indexed_captures(x) =
    let cs = ParserWithCaptures(x)
        pass1 = ParserWithCaptures(map_parser(indexed_captures,NoDict(),x,cs,false),cs.subroutines,cs.names)
        ParserWithCaptures(map_parser(indexed_captures,NoDict(),pass1.parser,pass1,false),pass1.subroutines,pass1.names)
    end




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
