# TODO:
# - remove after from get (nextind with state and i)
# - (Feedback appreciated: Would is be more efficient change the `iterate_state` internal API for the first match to arity 4?)
# - Base.get(parser, sequence, till, after, i, state) to
#   Base.get(parser, sequence, i, after, till, state) 
"""
A package for combining parsers and transforming strings into julia types.

Compose parsers with the functional [parser combinator paradigm](https://en.wikipedia.org/wiki/Parser_combinator),
utilize Julia's type inferrence for transformations,
log conveniently for debugging, and let Julia compile your parser for good performance.
"""
module CombinedParsers
using Nullables
using AutoHashEquals
import Base: ==, hash
import Base: lowercase
import Base: cat, get

import TextParse
import TextParse: AbstractToken

include("ind.jl")

using LazyStrings
import LazyStrings: reversed, reverse_index

export CombinedParser
export result_type

"Julia types that provide CombinedParser methods result_type, state_type, iterate_state, get, nextind, prevind."
## Pair{<:Union{AbstractToken, AbstractString, Char, Regex, Pair},<:Any} }
export parser
import Base: convert

iostring(f::Function, a...; kw...) =
    let sio = IOBuffer()
        f(sio, a...; kw...)
        String(take!(sio))
    end
@nospecialize
"""
    CombinedParser

Abstract parser type for parsers returning matches transformed to `::T` and 
state::`S`.
"""
abstract type CombinedParser end

export MatchState
"""
State object for a match that is defined by the triple `parser, sequence, position`.

!!! note:
    Performance tip: [`Atomic`](@ref) is masking the state of its wrapped parser with `MatchState`.
    This simplifies the state
"""
struct MatchState end
Base.show(io::IO, ::MatchState) = print(io,"∘")

"""
State object representing ncodeunits explicitely with state of match for `leftof`, `rightof` to improve performance.
    `nc::Int` and `state::S`.

See also [`MatchState`](@ref), [`leftof`](@ref), [`rightof`](@ref).

!!! note:
    `nc` as type parameter faster but slow compilation.
"""
struct NCodeunitsState{S}
    nc::Int
    state::S
end

"""
    (x::CombinedParser)(str;kw...)


`parse(x,str;kw...)`

See also [`parse`](@ref).
"""
(x::CombinedParser)(str;kw...) = parse(x,str;kw...)
(x::CombinedParser)(prefix,str;kw...) = parse(map(IndexAt(2),Sequence(prefix,x)),str;kw...)
(x::CombinedParser)(f::Function,a...;kw...) = map(f,x,a...;kw...)

"""
    result_type(x::CombinedParser)

returns the result type of a parser. 

The result type is a CombinedParser type parameter.
Most of the time it is type-inferred within constructors
by [`infer_result_type`](@ref).
"""
result_type(x::CombinedParser, sequence; kw...) =
    error("implement result_type(::$(typeof(x)), sequence)!")

result_type(x::CombinedParser) =
    result_type(x, "")

"""
    WrappedParser{P}

Abstract type for parser wrappers, providing default methods."
"""
abstract type WrappedParser{P} <: CombinedParser end
result_type(p::WrappedParser, sequence; kw...)  = result_type(p.parser, sequence; kw...)

@inline state_type(::Type{<:WrappedParser{P}}) where P =
    state_type(P)

export FilterParser
"""
A parser succeeds ony if 
1. the wrapped `parser` succeeds 
2. and a predicate function `state_filter(sequence, till, posi, r...)` returns `true` the `after,state = r` tuple.
"""
struct FilterParser{P,F} <: WrappedParser{P}
    parser::P
    state_filter::F
    FilterParser(f::Function,parser_) =
        let p = parser(parser_)
            new{typeof(p),typeof(f)}(p,f)
        end
end
Base.filter(f::Function, x::Union{TextParse.AbstractToken,CombinedParser}) =
    FilterParser(f,parser(x))

export filter_result
filter_result(f::Function, x) =
    filter(parser(x)) do sequence,till,posi,after,state
        f(get(parser(x),sequence,till,posi,after,state))
    end
        


"""
    LeafParser <: CombinedParser

Abstract parser type for parsers that have no sub-parser (e.g. [`ConstantParser`](@ref)).
Used for dispatch in [`deepmap_parser`](@ref).
"""
abstract type LeafParser <: CombinedParser end



"""
    NIndexParser{N} <: LeafParser{MatchState}

Abstract type for stepping `N` indices with [`_leftof`](@ref) and [`_rightof`](@ref), 
accounting for `Base.ncodeunits` length of unicode chars.

See [`Bytes`](@ref) and [`ValueMatcher`](@ref).
"""
abstract type NIndexParser{N} <: LeafParser end
@inline state_type(::Type{<:NIndexParser}) =
    MatchState
@inline _leftof(str,i,parser::NIndexParser{0},state) = i
@inline _rightof(str,i,parser::NIndexParser{0},state) = i
@inline _leftof(str,i,parser::NIndexParser{L},state) where L =
    _prevind(str,i,L)
@inline _rightof(str,i,parser::NIndexParser{L},state) where L =
    _nextind(str,i,L)



export Bytes
"""
    Bytes{N} <: NIndexParser{N}

Fast parsing of a fixed number `N` of indices, 
`reinterpret(T,match)[1]` the parsed vector as `T`, if `isbitstype`, or `T(match)` constructor otherwise.

Provide `Base.get(parser::Bytes{N,T}, sequence, till, after, i, state) where {N,T}` for custom conversion.

!!! note

    Endianness can be achieved by just mapping `bswap`
    ```jldoctest
    julia> map(bswap, Bytes(2,UInt16))([0x16,0x11])
    0x1611

    julia> Bytes(2,UInt16)([0x16,0x11])
    0x1116
    ```
"""
struct Bytes{N} <: NIndexParser{N} end

"""
    Bytes(N::Integer, T::Type=Vector{UInt8})

If available before end of sequence, parse `N` bytes successfully with `result_type` `T`, fail otherwise.
"""
Bytes(N::Integer, T::Type=Vector{UInt8}) = map(T,Bytes{N}())
@deprecate Bytes{T}(N::Integer) where T Bytes(N,T)


include("parser.jl")
include("textparse.jl")
include("constant.jl")
include("valuematcher.jl")
include("assertions.jl")

@auto_hash_equals struct PartialMatchException{S,P} <: Exception
    index::Int
    str::S
    delta::Int
    pattern::P
    PartialMatchException(i,str::S,p::P) where {S<:AbstractString,P} =
        new{S,P}(i,str,200,p)
    PartialMatchException(i,str::S,p::P) where {S,P} =
        new{S,P}(i,str,6,p)
end
export context
context(x::PartialMatchException) =
    x.str[min(x.index,end):min(end, _nextind(x.str,x.index,x.delta))]
import Base: showerror
function Base.showerror(io::IO, x::PartialMatchException)
    println(io, "parsing stopped at postion $(x.index) in:")
    println(io, "$(x.str)")
    println(io, "."^(x.index-1),"^")
end

struct SideeffectParser{A,K,P} <: WrappedParser{P}
    parser::P
    args::A
    keywords::K
    effect::Function
    SideeffectParser(f::Function, p::CombinedParser,a...; kw...) =
        new{typeof(a),typeof(kw),typeof(p)}(p,a,kw,f)
end

"""
    with_effect(f::Function,p,a...)

Call `f(sequence,before_i,after_i,state,a...)` if `p` matches,
 `f(sequence,before_i,before_i,nothing,a...)` otherwise.
"""
with_effect(f::Function,p,a...; kw...) =
    SideeffectParser(f,p,a...; kw...)




using Dates
export NamedParser, with_name
"""
    NamedParser{P} <: WrappedParser{P}

if doc!="", printing will print the node with this label, and hides constructors.
Struct with
```julia
    name::Symbol
    parser::P
    doc::String
```
"""
@auto_hash_equals struct NamedParser{P} <: WrappedParser{P}
    name::Symbol
    parser::P
    doc::String ## rename -> label
    NamedParser(name::Symbol,p_;doc="") =
        let p=parser(p_)
            new{typeof(p)}(name,p,doc)
        end
end

"""
    with_name(name::Symbol,x; doc="")

A parser labelled with `name`.
Labels are useful in printing and logging.

See also: [`@with_names`](@ref), [`with_name`](@ref), [`log_names`](@ref)
"""
with_name(name::Symbol, x, doc="") = 
    NamedParser(name,parser(x); doc=doc)

with_name(name::AbstractString,x, doc="") =
    name=="" && doc=="" ? x : NamedParser(Symbol(name),parser(x); doc=doc)

export @with_names
with_names(x) = x
function with_names(node::Expr)
    if node.head == :(=) && length(node.args) == 2 && isa(node.args[1], Symbol)
        node.args[2] = Expr(:call, :with_name, QuoteNode(node.args[1]), node.args[2])
    end
    if node.head != :call 
        node.args = map(with_names, node.args)
    end
    node
end


"""
    @with_names

Sets names of parsers within begin/end block to match the variables they are asigned to.

so, for example
```jldoctest
julia> @with_names foo = AnyChar()
. AnyValue |> with_name(:foo)
::Char

julia> parse(log_names(foo),"ab")
   match foo@1-2: ab
                  ^
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

See also [`log_names`](@ref) and [`@syntax`](@ref).
"""
macro with_names(block)
    esc(with_names(block))
end


export @syntax
"""
    @syntax name = expr
Convenience macro defining a CombinedParser `name=expr` and custom parsing macro `@name_str`.

```@meta
DocTestFilters = r"map\\(.+\\)"
```

```jldoctest
julia> @syntax a = AnyChar();

julia> a"char"
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

```

    @syntax for name in either; expr; end
Parser `expr` is [`pushfirst!`](@ref) to `either`.
If `either` is undefined, it will be created.
If `either == :text || either == Symbol(:)` the parser will be added to `CombinedParser_globals` variable in your module.

```jldoctest
julia> @syntax street_address = Either(Any[]);

julia> @syntax for german_street_address in street_address
            Sequence(!!Repeat(AnyChar()),
                     " ",
                     TextParse.Numeric(Int)) do v
                (street = v[1], no=v[3])
            end
       end
🗄 Sequence |> map(#50) |> with_name(:german_street_address)
├─ .* AnyValue |> Repeat |> ! |> map(intern)
├─ \\
└─ <Int64>
::NamedTuple{(:street, :no), Tuple{String, Int64}}

julia> german_street_address"Some Avenue 42"
(street = "Some Avenue", no = 42)


julia> @syntax for us_street_address in street_address
            Sequence(TextParse.Numeric(Int),
                     " ",
                     !!Repeat(AnyChar())) do v
                (street = v[3], no=v[1])
            end
       end
🗄 Sequence |> map(#52) |> with_name(:us_street_address)
├─ <Int64>
├─ \\  
└─ .* AnyValue |> Repeat |> ! |> map(intern)
::NamedTuple{(:street, :no), Tuple{String, Int64}}

julia> street_address"50 Oakland Ave"
(street = "Oakland Ave", no = 50)

julia> street_address"Oakland Ave 50"
(street = "Oakland Ave", no = 50)
```
"""
macro syntax(block)
    R = if block.head == :for
        name, within = block.args[1].args
        within_expr = if within === :texts || within === Symbol(":") ## parser is global
            quote
                if isdefined($__module__,:CombinedParser_globals)
                    CombinedParser_globals
                else
                    global CombinedParser_globals
                    CombinedParser_globals = Repeat(Either{Any}())
                end
            end
        elseif __module__.eval( :(isdefined($__module__,$(QuoteNode(within))) && $within isa CombinedParser ))
            :($within)
        else ## new Either
            quote
                @with_names $within = Either{Any}()
            end
        end
        body = block.args[2]
        if body.head==:block
            expr = Any[]
            for e in body.args
                if e isa LineNumberNode
                    push!(expr,e)
                elseif e isa Symbol
                    push!(expr,e)
                elseif e.head==Symbol("=") && e.args[1]==:examples
                    @warn "examples currently ignored" 
                else
                    push!(expr,with_names(Expr(Symbol("="), name, e)))
                end
            end
            quote
                $within_expr
                $(expr...)
                pushfirst!($within, $name);
                $name
            end
        else
            dump(block)
            error()
        end
    elseif block.head==Symbol("=")
        name = block.args[1]
        quote
            $(with_names(block))
        end
    elseif block.head == :block
        with_names(block)
    else
        dump(block)
        error()
    end
    esc(R)
end


import Base: in


export Repeat_stop, Repeat_until
"""
    Repeat_stop(p,stop)
    Repeat_stop(p,stop; min=0, max=Repeat_max)

Repeat `p` until `stop` (`NegativeLookahead`), not matching `stop`.
Sets cursor **before** `stop`. Tries `min:max` times
Returns results of `p`.

!!! note
Can be wrapped with Lazy.

```jldoctest
julia> p = Repeat_stop(AnyChar(),'b') * AnyChar()
🗄 Sequence
├─ 🗄* Sequence[2] |> Repeat
│  ├─ (?!b) NegativeLookahead
│  └─ . AnyValue
└─ . AnyValue
::Tuple{Vector{Char}, Char}

julia> parse(p,"acbX")
(['a', 'c'], 'b')
```

See also [`NegativeLookahead`](@ref)
"""
Repeat_stop(p,stop; min=0, max=Repeat_max) =
    Repeat(map(IndexAt(2),Sequence(NegativeLookahead(parser(stop)),parser(p)));min=min,max=max)

@deprecate rep_stop(a...;kw...) Repeat_stop(a...;kw...)

"""
    Repeat_until(p,until, with_until=false; wrap=identity, min=0, max=Repeat_max)

Repeat `p` until `stop` (with [`Repeat_stop`](@ref)).
and set point **after** `stop`.

Return a `Vector{result_type(p)}` if `wrap_until==false`, otherwise a `Tuple{Vector{result_type(p)},result_type(until)}`.

To transform the `Repeat_stop(p)` parser head, provide a function(::Vector{result_type(p)}) in `wrap` keyword argument, e.g.
```jldoctest
julia> p = Repeat_until(AnyChar(),'b') * AnyChar()
🗄 Sequence
├─ 🗄 Sequence[1]
│  ├─ (?>🗄*) Sequence[2] |> Repeat |> Atomic
│  │  ├─ (?!b) NegativeLookahead
│  │  └─ . AnyValue
│  └─ b
└─ . AnyValue
::Tuple{Vector{Char}, Char}

julia> parse(p,"acbX")
(['a', 'c'], 'X')

julia> parse(Repeat_until(AnyChar(),'b';wrap=MatchedSubSequence),"acbX")
"ac"
```

See also [`NegativeLookahead`](@ref)
"""
Repeat_until(p,until, with_until=false;wrap=identity,min=0,max=Repeat_max) =
    if with_until
        Sequence(map(wrap,Atomic(Repeat_stop(p,until;min=min,max=max))), until)
    else
        map(IndexAt(1),Sequence(map(wrap,Atomic(Repeat_stop(p,until;min=min,max=max))), until))
    end

@deprecate rep_until(p,until) Repeat_until(p,until)



export FlatMap,after
"""
    FlatMap{P,Q<:Function} <: CombinedParser

Like Scala's [fastparse FlatMap](https://www.lihaoyi.com/fastparse/#FlatMap).
See [`after`](@ref)
"""
@auto_hash_equals struct FlatMap{P,Q<:Function} <: CombinedParser
    left::P
    right::Q
    function FlatMap(right::Q, left::P) where {P<:CombinedParser, Q<:Function}
        new{P,Q}(left, right)
    end
end
flatmap_state(old,ls,rp,rs) = tuple_pos(rs), (ls,rp,tuple_state(rs))
left_state(state::Tuple) = state[1]
right_parser(state::Tuple) = state[2]
right_state(state::Tuple) = state[3]

@inline state_type(::Type{<:FlatMap}) =
    Tuple{<:Any,<:Any,<:Any}

result_type(x::FlatMap, sequence; kw...) =  Any #result_type(x.left, sequence; kw...)


@deprecate FlatMap(right::Function, left, T::Type=Any) map(T,FlatMap(right,parser(left)))
@deprecate FlatMap(right::Function, T::Type, left) map(T,FlatMap(right,parser(left)))
@deprecate FlatMap{T}(right::Function, left) where T map(T,FlatMap(right,parser(left)))

"""
    after(right::Function,left::AbstractToken)
    after(right::Function,left::AbstractToken,T::Type)

Like Scala's fastparse [`FlatMap`](@ref)

```jldoctest
julia> saying(v) = v == "same" ? v : "different";

julia> p = after(saying, String, "same"|"but")
🗄 FlatMap
├─ |🗄 Either
│  ├─ same 
│  └─ but 
└─ saying
::String

julia> p("samesame")
"same"

julia> p("butdifferent")
"different"

```
"""
after(a...) = FlatMap(a...)



@inline _rightof(str,i,parser::FlatMap,x::Tuple) =
    let li = rightof(str,i,parser.left,tuple_pos(x))
        rightof(str,li,x[2],x[3])
    end

@inline _leftof(str,i,parser::FlatMap,x::Tuple) =
    let li = leftof(str,i,x[2],x[3])
        leftof(str,li,parser.left,tuple_pos(x))
    end

    

export ParserPair
"""
    Sequence{P,S,T}

of `parts::P`, [`sequence_state_type`](@ref)==S and [`sequence_result_type`](@ref).
"""
@auto_hash_equals struct ParserPair{P,Q} <: CombinedParser
    first::P
    second::Q
    ParserPair(first::CombinedParser, second::CombinedParser) =
        new{typeof(first),typeof(second)}(p)
end


merge_tuples(v) = tuple(v[1]..., v[2]...)

export Sequence, mSequence
"""
    Sequence{P}

of `parts::P`. [`sequence_state_type`](@ref)`==S` with [`sequence_result_type`](@ref)`==T` are computed from `parts`.

    Sequence(parts::CombinedParser...; tuplestate=true)

of `parts`, [`sequence_state_type`](@ref)`(p; tuplestate=tuplestate)` with [`sequence_result_type`](@ref).

Sequences can alternatively created with [`*`](@ref)
```jldoctest
julia> german_street_address = !Repeat(AnyChar()) * ' ' * TextParse.Numeric(Int)
🗄 Sequence
├─ .* AnyValue |> Repeat |> !
├─ \\
└─ <Int64>
::Tuple{SubString{String}, Char, Int64}

julia> german_street_address("Some Avenue 42")
("Some Avenue", ' ', 42)
```
Indexing (transformation) can be defined with
```jldoctest
julia> e1 = Sequence(!Repeat(AnyChar()), ' ',TextParse.Numeric(Int))[1]
🗄 Sequence[1]
├─ .* AnyValue |> Repeat |> !
├─ \\
└─ <Int64>
::SubString{String}

julia> e1("Some Avenue 42")
"Some Avenue"
```


!!! note
    State is managed as [`sequence_state_type`](@ref)`(parts; tuplestate)`.
    Overwrite to optimize state types special cases.



    Sequence(parts...; kw...)


Parts that are not `::CombinedParser` are converted with [`parser`](@ref).
```jldoctest
julia> german_street_address = Sequence(!Repeat(AnyChar()), ' ', TextParse.Numeric(Int))
🗄 Sequence
├─ .* AnyValue |> Repeat |> !
├─ \\
└─ <Int64>
::Tuple{SubString{String}, Char, Int64}

julia> german_street_address("Some Avenue 42")
("Some Avenue", ' ', 42)
```

!!! note
    Returns a NamedTuple [`Base.map`](@ref) transformation if any part was `Pair{Symbol}`.

    ```jldoctest
    julia> german_street_address =  Sequence(:street => !Repeat(AnyChar()), " ", :no => TextParse.Numeric(Int))
    🗄 Sequence |> map(ntuple)
    ├─ .* AnyValue |> Repeat |> ! |> with_name(:street)
    ├─ \\
    └─  <Int64> |> with_name(:no)
    ::NamedTuple{(:street, :no), Tuple{SubString{String}, Int64}}

    julia> german_street_address("Some Avenue 42")
    (street = "Some Avenue", no = 42)
    ``` 
"""
@auto_hash_equals struct Sequence{P} <: CombinedParser
    parts::P
    Sequence(parts::Tuple, stype::Type=Any) = new{stype}(parts)
    Sequence(parts::Vector, stype::Type=Vector{CombinedParser}) = new{stype}(parts)
end
@nospecialize
Sequence(;kw...) =
    isempty(kw) ? Always() : Sequence(kw...)
function Sequence(p...)
    parts = CombinedParser[ parser(p_) for p_ in p ] # tuple( parser.(p)... )
    s = Sequence(parts)
    names = Pair{Symbol,Int}[ t.first=>i
                              for (i,t) in enumerate(p)
                                  if t isa Pair{Symbol} ]
    if isempty(names)
        return s
    else
        function ntuple(v)
            (; (k.first => v[k.second] for k in names )... )
        end
        map(ntuple, s)
    end
end
#Sequence(p::Vector; kw...) = Sequence(p...; kw...)

@deprecate Sequence(transform::Function, T::Type, a...; kw...)   map(transform, T, Sequence(a...; kw...))

@deprecate Sequence(transform::Function, a...; kw...) map(transform, Sequence(a...; kw...))

@deprecate Sequence(transform::Integer,tokens...; kw...) Sequence(Val{transform}(),parser.(tokens)...; kw...)

mSequence(transform::Function, T::Type, a...; kw...) =
    map(transform, T, Sequence(a...; kw...))

mSequence(transform::Function, a...; kw...) =
    map(transform, Sequence(a...; kw...))


mSequence(transform::Integer,tokens...; kw...) =
    map(IndexAt(transform),Sequence(tokens...; kw...))


function _sSequence(x, r::Vector{CombinedParser} = CombinedParser[])
    if x isa Sequence
        _sSequence(e,r)
    else
        for e in x
            !isa(e, Always) && push!(r,parser(e))
        end
    end
    r
end

export sSequence
"""
    sSequence(x...)

Simplifying `Sequence`, flatten `Sequence`s, remove `Always` assertions.

```jldoctest
julia> Sequence('a',CharIn("AB")*'b')
🗄 Sequence
├─ a
└─ 🗄 Sequence
   ├─ [AB] ValueIn
   └─ b
::Tuple{Char, Tuple{Char, Char}}


julia> sSequence('a',CharIn("AB")*'b')
🗄 Sequence
├─ a
├─ [AB] ValueIn
└─ b
::Tuple{Char, Char, Char}
```
See also [`Sequence`](@ref)

!!! note
    This function will be removed and replaced with a keyword argument
"""
function sSequence(x...)
    p = _sSequence(x)
    length(p) == 1 ? p[1] : Sequence(p...)
end
@specialize

result_type(p::Sequence, sequence; kw...)  =
    sequence_result_type(p.parts, sequence; kw...)

"""
    sequence_result_type(parts, sequence)

`Tuple` type, internally used for `Sequence` result_type.
"""
sequence_result_type(parts, sequence; kw...) =
    Tuple{ (result_type(p, sequence; kw...) for p in parts)... }

isliteralsequence(c::ConstantParser) = true
isliteralsequence(c) = false
function isliteralsequence(c::Sequence)
    (&)(isliteralsequence.(c.parts)...)
end

"""
    state_type(pts::Type; tuplestate=true)

- `MatchState` if all `fieldtypes` are `MatchState`, 
- otherwise if `tuplestate`, a tuple type with the `state_type` of `parts`,
- or `Vector{Any}` if `!tuplestate`.

!!! note
    Todo: NCodeunitsState instead of MatchState might increase performance.
"""
function state_type(::Type{<:Sequence{pts}}) where {pts <: Tuple}
    if isempty(fieldtypes(pts)) || all(t->state_type(t)<:MatchState, fieldtypes(pts))
        MatchState
    else
        Tuple{(state_type(p) for p in fieldtypes(pts))...}
    end
end
state_type(::Type{<:Sequence{<:AbstractVector{P}}}) where P =
    Vector{Any}



Base.lastindex(x::Sequence) = lastindex(x.parts)

@inline function _leftof(str,i,parser::Sequence,x::MatchState)
    for p in length(parser.parts):-1:1
        i=leftof(str,i,parser.parts[p],x)
    end
    i
end

@inline function _leftof(str,i,parser::Sequence,x)
    for j in lastindex(x):-1:1
        (p,e) = parser.parts[j],x[j]
        i=leftof(str,i,p,e)
    end
    i
end

@inline function _rightof(str,i,parser::Sequence,x::MatchState)
    for p in parser.parts
        i=rightof(str,i,p,MatchState())
    end
    i
end

@inline function _rightof(str,i,parser::Sequence,x)
    for (p,e) in zip(parser.parts,x)
        i=rightof(str,i,p,e)
    end
    i
end

function prune_captures(sequence,after_i)
end


Base.getindex(A::MatchState, i::Int) = MatchState()
Base.setindex!(A::MatchState, ::MatchState, i::Int) = nothing
Base.setindex!(A::MatchState, v, i::Int) = error("MatchState elements can only be ::MatchState")


sequence_state(statettype::Type{MatchState}, states) = MatchState()
sequence_state(statettype::Type{<:Tuple}, states) = tuple( (s for s in states...) )
sequence_state(statettype::Type, states) = states

sequence_state(statettype::Type{MatchState}) = MatchState()
sequence_state(statettype::Type{<:Tuple}) = tuple( )
sequence_state(statettype::Type) = Any[]



export Lazy
"""
    Lazy(x::Repeat)
    Lazy(x::Optional)

Lazy `x` repetition matching (instead of default greedy).

```jldoctest
julia> german_street_address = !Lazy(Repeat(AnyChar())) * Repeat1(' ') * TextParse.Numeric(Int)
🗄 Sequence
├─ .*? AnyValue |> Repeat |> Lazy |> !
├─ \\ +  |> Repeat
└─ <Int64>
::Tuple{SubString{String}, Vector{Char}, Int64}

julia> german_street_address("Konrad Adenauer Allee    42")
("Konrad Adenauer Allee", [' ', ' ', ' ', ' '], 42)
```

!!! note 
    PCRE `@re_str`
    ```jldoctest
    julia> re"a+?"
    a+?  |> Repeat |> Lazy
    ::Vector{Char}

    julia> re"a??"
    a?? |missing |> Lazy
    ::Union{Missing, Char}
    ```
"""
@auto_hash_equals struct Lazy{P} <: WrappedParser{P}
    parser::P
    Lazy(p_) =
        let p = parser(p_)
            new{typeof(p)}(p)
        end
end

const Repeat_max = 10^6
export Repeat1, Repeat
"""
    Repeat(minmax::UnitRange, x...)
    Repeat(x...; min=0,max=Repeat_max)
    Repeat(min::Integer, x...)
    Repeat(min::Integer,max::Integer, x...)

Parser repeating pattern `x` `min:max` times.

```jldoctest
julia> Repeat(2,2,'a')
a{2}  |> Repeat
::Vector{Char}


julia> Repeat(3,'a')
a{3,}  |> Repeat
::Vector{Char}

```
"""
@auto_hash_equals struct Repeat{P} <: WrappedParser{P}
    range::UnitRange{Int}
    parser::P
    Repeat(range::UnitRange{Int},p::CombinedParser)  =
        new{typeof(p)}(range,p)
    # Repeat(p::P) where {P<:CombinedParser} =
    #     new{P,repeat_state_type(state_type(p)),Vector{result_type(P)}}(0:Repeat_max,p)
end
Repeat(range::UnitRange{Int},p...)                  = Repeat(range,sSequence(p...))
Repeat(min::Integer,max::Integer,p...)              = Repeat((min:max),p...)
Repeat(p...;min::Integer=0,max::Integer=Repeat_max) = Repeat((min:max),p...)
Repeat(min::Integer,p...)                           = Repeat((min:Repeat_max),p...)

result_type(p::Repeat, sequence; kw...)  =
    Vector{result_type(p.parser, sequence; kw...)}

@inline state_type(t::Type{Repeat{P}}) where P =
    repeat_state_type(state_type(P))


@inline repeat_state_type(::Type{MatchState}) = Int
@inline repeat_state_type(T::Type) = Vector{T}

"""
    Repeat(f::Function,a...)

Abbreviation for [`Base.map`](@ref)`(f,Repeat(a...))`.
"""
mRepeat(f::Union{Function,Type},a...;kw...)           = map(f,Repeat(a...;kw...))

"""
    Repeat1(x)

Parser repeating pattern `x` one time or more.
"""
Repeat1(x...; max=Repeat_max)                        = Repeat(1:Repeat_max,x...)

"""
    Repeat1(f::Function,a...)

Abbreviation for [`Base.map`](@ref)`(f,Repeat1(a...))`.
"""
mRepeat1(f::Function,a...; kw...)                     = map(f,Repeat1(a...; kw...))

@deprecate Repeat(minmax::Tuple{<:Integer,<:Integer},x,y::Vararg) Repeat(minmax...,x,y...)

@deprecate mRepeat(transform::Function, T::Type, a...) map(transform, T, Repeat(a...))

@deprecate mRepeat(transform::Function, minmax::Tuple{<:Integer,<:Integer}, a...) map(transform, Repeat(minmax..., a...))

@deprecate rep(a...;kw...) Repeat(a...;kw...)

import Base.join

"""
    Base.join(x::Repeat,delim, infix=:skip)

Parser matching repeated `x.parser` separated by `delim`.
```jldoctest
julia> parse(join(Repeat(AnyChar()),','),"a,b,c")
3-element Vector{Char}:
 'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
 'b': ASCII/Unicode U+0062 (category Ll: Letter, lowercase)
 'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)
```
```jldoctest
julia> parse(join(Repeat(AnyChar()),',';infix=:prefix),"a,b,c")
('a', [(',', 'b'), (',', 'c')])

julia> parse(join(Repeat(AnyChar()),',';infix=:suffix),"a,b,c")
([('a', ','), ('b', ',')], 'c')
```
"""
function Base.join(x::Repeat, delim_; infix=:skip)
    delim = parser(delim_)
    if infix==:prefix
        Sequence(x.parser, Repeat( Sequence( delim, x.parser ) ))
    elseif infix==:suffix
        Sequence(Repeat( Sequence( x.parser, delim ) ), x.parser)
    elseif infix==:skip
        ## todo: the get function could be optimized
        ##@show x.range
        map(x.parser * Repeat(
            max(0,x.range.start-1),
            x.range.stop == Repeat_max ? Repeat_max : x.range.stop-1,
            mSequence(2, delim,x.parser ))) do (f,r)
                pushfirst!(r,f)
                r::result_type(x)
            end
    else
        error("unknown delim=$delim, infix=$infix")
    end
end

"""
    Base.join(x::CombinedParser,delim; kw...)

Shorthand for `join(Repeat(x),delim; kw...)`.
"""
Base.join(x::Union{TextParse.AbstractToken,CombinedParser},delim; kw...) =
    join(Repeat(parser(x)),delim;kw...)

"""
    Base.join(f::Function, x::CombinedParser, delim; kw...)

Shorthand for [`Base.map`](@ref)`(f,join(x,delim; kw...))`.
"""
Base.join(f::Function,p::CombinedParser,delim_; kw...) =
    map(f,join(p,delim_; kw...))

@inline function _leftof(str,i,parser::Repeat,x::Int)
    for e in 1:x
        i=leftof(str,i,parser.parser,MatchState())
    end
    i
end

@inline function _rightof(str,i,parser::Repeat,x::Int)
    for e in 1:x
        i=rightof(str,i,parser.parser,MatchState())
    end
    i
end

@inline function _rightof(str,i,parser::Repeat,x::Vector)
    for e in x
        i=rightof(str,i,parser.parser,e)
    end
    i
end

@inline function _leftof(str,i,parser::Repeat,x::Vector)
    for j in lastindex(x):-1:1
        @inbounds i=leftof(str,i,parser.parser,x[j])
    end
    i
end


@inline emptystate(::Type{Int}) = 0
@inline emptystate(::Type{Vector{T}}) where T = T[]

@inline state_length(parser,state::Int) = state
@inline state_length(parser::Repeat,x::Vector) = length(x)

@inline pushstate!(state::Int,parser,substate::MatchState) =  state + 1
@inline pushstate!(state::Vector,parser,substate) =  push!(state,substate)

@inline poplast!(outer_state,inner_parser) = poplast!(outer_state)
@inline poplast!(outer_state::Int) =
    if iszero(outer_state)
        nothing, 0
    else
        MatchState(), outer_state - 1
    end
@inline function poplast!(outer_state::Vector)
    l=pop!(outer_state)
    l,outer_state
end

## kernel function (function barrier)
@inline function fill_rep_j_state(x::Tuple{Int,<:Any},state_,tparser)
    pos, state = x
    pos, pushstate!(state_,tparser, state)
end

@inline function fill_rep(t::Repeat, sequence, till::Int, i::Int,state) 
    j_::Int = -1
    j::Int = i
    state_ = state
    tp = t.parser
    while state_length(t,state_) < t.range.stop && ( x = iterate_state(t.parser,sequence, till, j, j,nothing) )!==nothing
        ## @info "rep fill..." x state_
        ## e.g. match(re"(?:a|(?=b)|.)*\z","abc")
        j_=j
        ##j, state_ = fill_rep_j_state(x,state_,tp)
        j, state_ = fill_rep_j_state(x, state_,tp)
        state_length(t,state_)>t.range.start && j_==j && break
    end
    j,state_,state_length(t,state_) < t.range.start
end

## used by Repeat
function push_rep(t,sequence, till, posi, x::Nothing, state_)
    posi, state_, !iszero(state_length(t,state_))
end

## used by Repeat
function push_rep(t,sequence, till, posi, x::Tuple{Int,S}, state_) where S
    p,s = x
    if posi==p
        posi, state_, true
    else
        fill_rep(t,sequence,till,p,pushstate!(state_,t.parser,s))
    end
end


## used by Lazy{Repeat}
@inline function pushstate!_fill_rep(t_, sequence, till, state_, x)
    t = t_.parser
    state_=pushstate!(state_,t.parser,tuple_state(x))
    fill_rep(t_,sequence,till,tuple_pos(x),state_)
end







"""
    defaultvalue(T::Type)

Default value if [`Optional`](@ref)<:`CombinedParser` is skipped.
- `T<:AbstractString`: `""`
- `T<:Vector{E}`: `E[]`
- `T<:CombinedParser`: `Always()`
- otherwise `missing`

!!! note
    [`get`](@ref) will return a [`CombinedParsers._copy`](@ref) of `defaultvalue`.
"""
defaultvalue(::Type{ConstantParser{C}}) where C =
    defaultvalue(C)
defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type) = missing
defaultvalue(V::CombinedParser) = defaultvalue(result_type(V, ""))


export Optional
"""
State type for skipped optional. (Missing was breaking julia).
"""
struct NoMatch end
Base.show(io::IO, ::NoMatch) = print(io,"n/a")
"""
    Optional(parser;default=defaultvalue(result_type(parser)))
    
Parser that always succeeds. 
If parser succeeds, return result of `parser` with curser behind match.
If parser does not succeed, return `default` with curser unchanged.

```jldoctest
julia> match(r"a?","b")
RegexMatch("")

julia> parse(Optional("a", default=42),"b")
42
```
"""
@auto_hash_equals struct Optional{P,T} <: WrappedParser{P}
    parser::P
    default::T
    function Optional(p::CombinedParser; default=defaultvalue(p))
        new{typeof(p),typeof(default)}(p, default)
    end
end


state_type(p::Type{<:Optional{P}}) where P =
    Union{NoMatch,state_type(P)}

function result_type(p::Optional, sequence; kw...)
    #error()
    D, T = typeof(p.default), result_type(p.parser, sequence; kw...)
    T_ = promote_type(T,D)
    T_ === Any ? Union{T,D} : T_
end

Optional(x...;kw...) =
    Optional(sSequence(x...); kw...)


Optional(T::Type, x_; transform, kw...) =
    Optional(transform, T, x; kw...)

function mOptional(transform::Function, T::Type, x;
                  default=defaultvalue(T))
    map(transform,T,Optional(x; default=default))
end



@inline _leftof(str,i,parser::Optional,x::NoMatch) = i
@inline _rightof(str,i,parser::Optional,x::NoMatch) = i









export Either, mEither
export Delayed
using Tries

@nospecialize
"""
    Either{T}(p...) where {T} = map(T, Either(p...))

Parser that tries matching the provided parsers in order, accepting the first match, and fails if all parsers fail.

This parser has no `==` and `hash` methods because it can recurse.


```jldoctest
julia> match(r"a|bc","bc")
RegexMatch("bc")

julia> parse(Either("a","bc"),"bc")
"bc"

julia> parse("a" | "bc","bc")
"bc"

```

    Either(p...; simplify=false)

Create a immutable `Either{either_state_type(p),either_result_type(p)}(::Tuple)` improved for performance.
Arguments `p...` are wrapped in [`parser`](@ref),
type parameters are computed with [`either_state_type`](@ref) and [`either_result_type`](@ref).

If `simplify`, flattens nested `Either`s and remove `Never` parsers, if only a single option remains return that option.
If you want to simplify a constructed parser, [`CombinedParsers.strip_either1`](@ref) provides more options.

```jldoctest
julia> Either('a', simplify=true)
re"a"

julia> Either('a',CharIn("AB")|"bc")
|🗄 Either
├─ a
└─ |🗄 Either
   ├─ [AB] ValueIn
   └─ bc
::Union{Char, SubString{String}}


julia> Either('a',CharIn("AB")|"bc", simplify=true)
|🗄 Either
├─ a
├─ [AB] ValueIn
└─ bc
::Union{Char, SubString{String}}
```



    Either(p::Vector; simplify=false)

Create a mutable `Either{Any,Any}(::Vector{Any})` for creating recursive parsers.
Arguments `p...` are wrapped in [`parser`](@ref),
type parameters are computed with [`either_state_type`](@ref) and [`either_result_type`](@ref).

See also [`@syntax`](@ref).
!!! note
    state type and result type are `Any` which might cost performance.


    mEither(transform::Function, x::Vararg)

abbreviation for [`Base.map`]`(transform, Either(x...))`.



    Either{T}(p...; simplify=false, convert=false)

Create a mutable `map(T,Either(Any[p...]))` for creating recursive parsers.
Options can be added with [`push!`](@ref) and [`pushfirst!`](@ref).

If `convert` for any option `x` in `p` that has `!(result_type(x) <: T)`, adds [`Base.map`](@ref)`(T,x)` instead.
(Provide a `convert` method!)

See also [`@syntax`](@ref).
!!! note
    state type is `Any` which might cost performance.
"""
struct Either{Ps} <: CombinedParser
    options::Ps

    function Either(p_::Vector; simplify=false)
        p = either_options(p_; simplify=simplify)
        simplify && length(os)==1 && return first(os)
        new{typeof(p)}(p)
    end
    function Either(p::AbstractTrie)
        new{typeof(p)}(p)
    end
    function Either(p_...; simplify=false)
        os = either_options(p_; simplify=simplify)
        simplify && length(os)==1 && return first(os)
        p = tuple(os...)
        new{typeof(p)}(p)
    end

    function Either{T}(p_...; simplify=false) where {T}
        p = either_options(p_; simplify=simplify)
        simplify && length(os)==1 && return first(os)
        # for (i,x) in enumerate(p)
        #     if !(result_type(x) <: T)
        #         convert || error("transforming results with convert($T,::$(result_type(x)))\n$x")
        #         p[i] = map(T,x)
        #     end 
        # end
        Any <: T ? new{typeof(p)} : map(T,new{typeof(p)}(p))
    end

end
function mEither(transform::Function, x...; kw...)
    map(transform, Either(x...); kw...)
end

@deprecate Either(p::Tuple; kw...) Either(p...; kw...) 
@deprecate Either{T}(x::Vector; kw...) where T Either{T}(x...; kw...)
@deprecate Either{T}(x::Tuple; kw...) where T Either{T}(x...; kw...)
@deprecate sEither(x...) Either(x...; simplify=true)
@specialize


result_type(x::Either, sequence; kw...)  =
    either_result_type(x.options, sequence; kw...)


either_state_type(ts::Type{Vector{CombinedParser}}) =
    Tuple{Int,Any}
either_state_type(ts::Type{<:Tuple}) =
    Tuple{Int,promote_type(state_type.(fieldtypes(ts))...)}
@inline with_state!(x::Nothing,k::Int,s) = (k,s)

@inline state_type(::Type{Either{P}}) where {P} =
    either_state_type(P)

"""
    Delayed(T::Type) = 

[`Either`](@ref)`{T}()`.
"""
Delayed(T::Type) = map(T, Either())


"""
    Base.getindex(x::Either, property::Symbol)

Return parser option with name `property` if found nested in `WrappedParser`s.
Errors otherwise.

Useful with [`substitute`](@ref) and [`CombinedParsers.BNF.bnf`](@ref).
"""
function Base.getindex(x::Either, property::Symbol)
    for p in x.options
        while p isa WrappedParser
            p isa NamedParser && p.name==property && return p
            p = p.parser
        end
    end
    error("no NamedParser $property found")
end


function promote_type_union(Ts...)
    T = promote_type(Ts...)
    Any <: T ? Union{Ts...} : T
end

"return tuple(state_type,result_type)"
function either_result_type(ts::Tuple, sequence; kw...)
    promote_type_union(result_type.(ts, sequence; kw...)...)
end
either_result_type(ts::Vector, sequence; kw...) = ## possibly recursive!
    Any


function either_options(x, result = CombinedParser[]; simplify = true)
    for e in parser.(x)
        if e isa Either
            if simplify
                either_options(e.options, result; simplify = simplify)
            else
                push!(result, e)
            end
        elseif e isa Never
        elseif e isa CombinedParser
            push!(result, e)
        else
            error()
        end
    end
    result
end

"""
    Base.push!(x::Either, option)

Push `option` to `x.options` as parser tried next if `x` fails.

Recursive parsers can be built with `push!` to `Either`.

See also [`pushfirst!`](@ref) and [`@syntax`](@ref).
"""
function Base.push!(x::Either{<:Vector}, y_)
    y = parser(y_)
#    promote_type(result_type(y),result_type(x)) <: result_type(x) || error("$(result_type(y)) <: $(result_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
    # promote_type(state_type(y),state_type(x)) <: state_type(x) || error("$(state_type(y)) <: $(state_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
    push!(x.options,y)
    y
end

"""
    Base.pushfirst!(x::Either, option)

Push `option` to `x.options` as parser tried first, and trying `x` if `option` fails.

Recursive parsers can be built with `pushfirst!` to `Either`.

See also [`push!`](@ref) and [`@syntax`](@ref).
"""
function Base.pushfirst!(x::Either{<:Vector}, y_)
    y = parser(y_)
    #promote_type(result_type(y),result_type(x)) <: result_type(x) || error("$(result_type(y)) <: $(result_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
    #promote_type(state_type(y),state_type(x)) <: state_type(x) || error("$(state_type(y)) <: $(state_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
    pushfirst!(x.options,y)
    x
end


"""
    Base.push!(x::WrappedParser{<:Either}, option)

Push `option` to `x.options` of repeated inner parser.
"""
function Base.push!(x::WrappedParser, y)
    push!(x.parser,y)
    x
end
"""
    Base.pushfirst!(x::WrappedParser{<:Either}, option)

Push `option` as first `x.options` of repeated inner parser.
"""
function Base.pushfirst!(x::WrappedParser, y)
    pushfirst!(x.parser,y)
    x
end



mutable struct MutablePair{K,V}
    first::K
    second::V
    MutablePair{K,V}(f,s) where {K,V} =
        new{K,V}(f,s)
    MutablePair(f,s) =
        new{typeof(f),typeof(s)}(f,s)
end
Base.show(io::IO, x::MutablePair) =
    print(io, x.first, "=>", x.second)
@inline function with_state!(x::MutablePair,s)
    ##s isa Tuple{Int,Nothing} && error()
    x.second=s
    x
end

@inline function with_state!(x::MutablePair,k,s)
    ##s isa Tuple{Int,Nothing} && error()
    x.first=k
    x.second=s
    x
end
@inline function with_state!(x::Pair,s)
    Pair(x.first,s)
end
@inline function with_state!(x::Pair,k,s)
    Pair(k, s)
end

@inline function with_state!(x::Tuple,k,s)
    (k, s)
end

either_state_option(::Nothing) = 1
either_state_state(x::Nothing) = nothing
either_state_option(x::Tuple) = x[1]
either_state_state(x::Tuple) = x[2]
either_state_option(x::Union{Pair,MutablePair}) = x.first
either_state_state(x::Union{Pair,MutablePair}) = x.second

@inline function _leftof(str,i,parser::Either,x)
    ## @show i
    leftof(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end

@inline function _rightof(str,i,parser::Either,x)
    ## @show i
    rightof(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end
@inline function _rightof(str,i,parser::Either{P},x::Tuple{Int}) where {P}
    rightof(str,i,(@inbounds parser.options[either_state_option(x)]),either_state_state(x))
end
 

@generated function _leftof(str,i,parser::Either{pts},x::Union{Pair,MutablePair}) where {pts<:Tuple}
    fpts = fieldtypes(pts)
    parseoptions = [
        quote
        if j === $p
        return _leftof(str,i,parser.options[$p],s) # $(part[p]),s)
        end
        end
        for (p,t) in enumerate(fpts)
    ]
    R = quote
        j = x.first
        s = x.second
        $(parseoptions...)
        error("?")
    end
    R
end


include("trie.jl")


# AtomicState = NCodeunitsState{MatchState}

export Atomic
"""
    Atomic(x)

A parser matching `p`, and failing when required to backtrack
(behaving like an atomic group in regular expressions).
"""
@auto_hash_equals struct Atomic{P} <: WrappedParser{P}
    parser::P
    Atomic(p::CombinedParser) =
        new{typeof(p)}(p)
    Atomic{MatchState}(p::CombinedParser) =
        error("unsupported")
end
Atomic(p) = Atomic(parser(x))




@specialize
include("state.jl")
@nospecialize

include("transformation.jl")

include("caseless.jl")
include("deepmap.jl")

#@specialize


include("reverse.jl")


include("abstracttrees.jl")

include("tracing.jl")

include("match.jl")


include("get.jl")

include("operators.jl")

include("defaults.jl")

include("show.jl")

include("memoize.jl")


include("lazy.jl")
include("re.jl")
include("bnf.jl")

using PrecompileTools: @setup_workload, @compile_workload    # this is a small dependency

export @re_str
"""
    parse_options(options::AbstractString)

Return PCRE option mask parsed from `options`.

Parser for `flags` in [`@re_str`](@ref).

```jldoctest
julia> CombinedParsers.Regexp.pcre_options()
▽ * Sequence |> Repeat
├─ |▽  Either |> pcre_option
│  ├─ dupnames  |> DUPNAMES
│  ├─ xx  |> EXTENDED_MORE
│  ├─ i  |> CASELESS
│  ├─ m  |> MULTILINE
│  ├─ n  |> NO_AUTO_CAPTURE
│  ├─ U  |> UNGREEDY
│  ├─ J  |> DUPNAMES
│  ├─ s  |> DOTALL
│  ├─ x  |> EXTENDED
│  ├─ B  |> BINCODE
│  └─ I  |> INFO
└─ ,?  |> Optional
```
"""
macro re_str(x,flags)
    quote
        if true || !@isdefined(__pcre)
            @info "initializing"
            __pcre = CombinedParsers.Regexp.pcre_parser()
        end
        if true || !@isdefined(__pcre_options_parser)
            __pcre_options_parser = CombinedParsers.padded(CombinedParsers.Regexp.pcre_options())
        end
        options = tryparse(__pcre_options_parser,$flags)
        options === nothing && throw(UnsupportedError("options $options"))
        r=parse(__pcre,with_options(options...,$x); trace=true)
        r === nothing && error("invalid regex")
        r
    end |> esc
end


macro re_str(x)
    quote
        if true || !@isdefined(__pcre)
            __pcre = CombinedParsers.Regexp.pcre_parser()
        end

        r=parse(__pcre,$x; trace=true)
        r === nothing && error("invalid regex")
        r
    end |> esc
end


@setup_workload begin
    # Putting some things in `@setup_workload` instead of `@compile_workload` can reduce the size of the
    # precompile file and potentially make loading faster.
    @compile_workload begin
        
    end
end
end # module
