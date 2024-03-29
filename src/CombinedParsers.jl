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

using AbstractTrees
import AbstractTrees: children
import AbstractTrees: print_tree, printnode

export CombinedParser
export result_type

"Julia types that provide CombinedParser methods result_type, state_type, iterate_state, get, nextind, prevind."
## Pair{<:Union{AbstractToken, AbstractString, Char, Regex, Pair},<:Any} }
export parser
import Base: convert

"""
    CombinedParser{S,T} <: AbstractToken{T}

Abstract parser type for parsers returning matches transformed to `::T` and 
state::`S`.
"""
abstract type CombinedParser{S,T} <: AbstractToken{T} end

"""
    (x::CombinedParser)(str;kw...)


`parse(x,str;kw...)`

See also [`parse`](@ref).
"""
(x::CombinedParser)(str;kw...) = parse(x,str;kw...)
(x::CombinedParser)(prefix,str;kw...) = parse(Sequence(2,prefix,x),str;kw...)
(x::CombinedParser)(f::Function,a...;kw...) = map(f,x,a...;kw...)

"""
    result_type(x::CombinedParser)

returns the result type of a parser. 

The result type is a CombinedParser type parameter.
Most of the time it is type-inferred within constructors
by [`infer_result_type`](@ref).
"""
result_type(x::CombinedParser) = result_type(typeof(x))
result_type(::Type{<:CombinedParser{<:Any,T}}) where T = T

"""
    CombinedParsers.state_type(x::CombinedParser{S}) where S

Return `S`, the state type of `x`
"""
@inline state_type(::Type{<:CombinedParser{S}}) where {S} = S
@inline state_type(x::CombinedParser) = state_type(typeof(x))

include("state.jl")


export regex_string

"""
    regex_string(x::CombinedParser)

`regex_prefix(x)*regex_inner(x)*regex_suffix(x)`
"""
regex_string(x::CombinedParser) = regex_prefix(x)*regex_inner(x)*regex_suffix(x)
regex_prefix(x::CombinedParser) = ""
regex_suffix(x::CombinedParser) = ""
regex_inner(x::CombinedParser) = ""

if VERSION>=v"1.6"
    constructor_name(x) = typeof(x).name.name
else
    constructor_name(x) = typeof(x).name.name
end

"""
    print_constructor(io::IO,x)

Print constructor pipeline in parser tree node.
"""
print_constructor(io::IO,x) =
    if x isa CombinedParser
        print(io, constructor_name(x))
    else
    end


export iterate_state

"""
    iterate_state(parser, sequence[, till::Int=lastindex(sequence)[, posi::Int=firstindex(sequence)[, next_i=posi[, state=nothing]]]])

Return position `after` next match of `parser` in `sequence` at `posi`.
The next match is following current match `state` (first match iif `state==nothing`).

If no next match is found, return `nothing`.

!!! note
    `next_i` is the index in `sequence` after `parser` match at `posi` with `state`.
    
    - `leftof(sequence,next_i,parser,state)==posi`, the start of the `state`-matching subsequence.
    - `rightof(sequence,posi,parser,state)==next_i`, the position after the `state`-matching subsequence.
    - `sequence[leftof(sequence,next_i,parser,state):_prevind(sequence,next_i)]` is the matched subsequence.

!!! note 
    Writing a custom `iterate_state` implementations *must* return
    - `nothing` if no match is found
    - `Tuple{Int64,state_type(parser)}` with next position, match state if a match is found.

"""
@inline iterate_state(parser::CombinedParser, sequence, till=lastindex(sequence), posi=firstindex(sequence)) =
    iterate_state(parser, sequence,till,posi,posi,nothing)


"""
    WrappedParser{P,S,T}

Abstract type for parser wrappers, providing default methods."
"""
abstract type WrappedParser{P,S,T} <: CombinedParser{S,T} end
children(x::WrappedParser) = children(x.parser)
children_char = '\U1F5C4'
function print_constructor(io::IO,x::WrappedParser)
    print_constructor(io, x.parser)
    print(io, " |> ", constructor_name(x))
end
regex_prefix(x::WrappedParser) = regex_prefix(x.parser)
regex_suffix(x::WrappedParser) = regex_suffix(x.parser)
regex_inner(x::WrappedParser) = regex_inner(x.parser)

"""
    _leftof(str,i,parser::WrappedParser,x)

Convienience function for overriding [`leftof`](@ref) that guarantees that not `x isa Nothing` (returning `i`).
"""
@inline _leftof(str,i,parser::WrappedParser,x) = _leftof(str,i,parser.parser,x)

"""
    _rightof(str,i,parser::WrappedParser,x)

Convienience function for overriding [`rightof`](@ref) that guarantees that not `x isa Nothing` (returning `i`).
"""
@inline _rightof(str,i,parser::WrappedParser,x) = _rightof(str,i,parser.parser,x)

@inline _leftof(str,i,parser::WrappedParser,x::NCodeunitsState) = i-x.nc
@inline _rightof(str,i,parser::WrappedParser,x::NCodeunitsState) = i+x.nc

@inline iterate_state(parser::WrappedParser, sequence, till, posi, after, state) =
    iterate_state(parser.parser, sequence, till, posi, after, state)

export FilterParser
"""
A parser succeeds ony if 
1. the wrapped `parser` succeeds 
2. and a predicate function `state_filter(sequence, till, posi, r...)` returns `true` the `after,state = r` tuple.
"""
struct FilterParser{P,S,F,T} <: WrappedParser{P,S,T}
    parser::P
    state_filter::F
    FilterParser(f::Function,parser_) =
        let p = parser(parser_)
            new{typeof(p),state_type(p),typeof(f),result_type(p)}(p,f)
        end
end
Base.filter(f::Function, x::Union{TextParse.AbstractToken,CombinedParser}) =
    FilterParser(f,parser(x))

export filter_result
filter_result(f::Function, x) =
    filter(parser(x)) do sequence,till,posi,after,state
        f(get(parser(x),sequence,till,posi,after,state))
    end
        

@inline function iterate_state(parser::FilterParser, sequence, till, posi, next_i, state)
    r::Union{Nothing,Tuple{Int,state_type(parser.parser)}} = nothing
    while r === nothing
        r = iterate_state(parser.parser, sequence, till, posi, next_i, state)
        if r === nothing
            return nothing
        elseif !parser.state_filter(sequence, till, posi, r...)
            next_i,state=r
            r = nothing
        end
    end
    r
end


"""
    LeafParser{T} <: CombinedParser{T}

Abstract parser type for parsers that have no sub-parser (e.g. [`ConstantParser`](@ref)).
Used for dispatch in [`deepmap_parser`](@ref).
"""
abstract type LeafParser{S,T} <: CombinedParser{S,T} end

# for convenience
iterate_state(parser::LeafParser, sequence, till, posi, next_i, state::MatchState)  = nothing


"""
    NIndexParser{N,T} <: LeafParser{MatchState,T}

Abstract type for stepping `N` indices with [`_leftof`](@ref) and [`_rightof`](@ref), 
accounting for `Base.ncodeunits` length of unicode chars.

See [`Bytes`](@ref) and [`ValueMatcher`](@ref).
"""
abstract type NIndexParser{N,T} <: LeafParser{MatchState,T} end
@inline _leftof(str,i,parser::NIndexParser{0},state) = i
@inline _rightof(str,i,parser::NIndexParser{0},state) = i
@inline _leftof(str,i,parser::NIndexParser{L},state) where L =
    _prevind(str,i,L)
@inline _rightof(str,i,parser::NIndexParser{L},state) where L =
    _nextind(str,i,L)

@inline function iterate_state(parser::NIndexParser, sequence, till, posi, next_i, state::Nothing)
    posi > till && return nothing # prevents BoundsError
    ni = rightof(sequence,posi,parser,MatchState())
    if ni <= till+1
        (ni, MatchState())
    else
        nothing
    end
end


export Bytes
"""
    Bytes{N,T} <: NIndexParser{N,T}

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
struct Bytes{N,T} <: NIndexParser{N,T} end

"""
    Bytes(N::Integer, T::Type=Vector{UInt8})

If available before end of sequence, parse `N` bytes successfully with `result_type` `T`, fail otherwise.
"""
Bytes(N::Integer, T::Type=Vector{UInt8}) = Bytes{N,T}()

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
    ##println(io, x.pattern)
end

@auto_hash_equals struct SideeffectParser{P,S,T,A} <: WrappedParser{P,S,T}
    parser::P
    args::A
    effect::Function
    SideeffectParser(f::Function, p::CombinedParser,a...) =
        new{typeof(p),state_type(p),result_type(p),typeof(a)}(p,a,f)
end
children(x::SideeffectParser) = children(x.parser)
function print_constructor(io::IO,x::SideeffectParser)
    print_constructor(io,x.parser)
    c = if x.effect == log_effect
        "with_log(;nomatch=true)"
    elseif x.effect == log_effect_match
        "with_log"
    else
        "with_effect($(x.effect))"
    end
    print(io," |> $c")
end
regex_string(x::SideeffectParser) = regex_string(x.parser)

export with_log, with_effect
"""
    with_effect(f::Function,p,a...)

Call `f(sequence,before_i,after_i,state,a...)` if `p` matches,
 `f(sequence,before_i,before_i,nothing,a...)` otherwise.
"""
with_effect(f::Function,p,a...) =
    SideeffectParser(f,p,a...)

@inline function iterate_state(parser::SideeffectParser, sequence, till, posi, next_i, state)
    r = iterate_state(parser.parser, sequence, till, posi, next_i, state)
    if r!==nothing
        parser.effect(sequence,posi,r...,parser.args...)
    else
        parser.effect(sequence,posi,posi,nothing,parser.args...)
    end
    r
end


"""
    Base.escape_string(x::AbstractVector)

for printing a non-string sequence when parsing.
!!! note
    type piracy? module local `_escape_string`?
"""
Base.escape_string(x::AbstractVector) = "["*join(repr.(x),",")*"]"

"""
    with_log(s::AbstractString,p, delta=5;nomatch=false)

Log matching process of parser `p`, displaying `delta` characters left of and right of match.

If `nomatch==true`, also log when parser does not match.

See also: [`log_names`](@ref), [`with_effect`](@ref)
"""
with_log(s::AbstractString,p_, delta_char::Integer=5;nomatch=false) =
    let p = parser(p_), log=s
        SideeffectParser(nomatch ? log_effect : log_effect_match ,p, log, delta_char)
    end

function log_effect(s,start,after,state,log,delta)
    at = "@$(start)-$(after)"
    if state === nothing
        printstyled("no match ", color=:underline)
    else
        print("   ")
        printstyled("match";
                    bold=false,color=:underline)
        print(" ")
    end
    printstyled(log,color=:green, bold=false)
    print(at,": ")
    firsti = _prevind(s,start,delta)
    lasti = (_prevind(s,start))
    before, matched = if _prevind(s,start)<start
        escape_string(s[max(1,firsti):lasti]), escape_string(s[start:_prevind(s,after)])
    else
        "",""
    end
    if lastindex(matched)>100
        matched=matched[1:_nextind(matched,1,20)]*"[...]"*matched[_prevind(matched,end,20):end]
    end
    printstyled(before; bold=true)
    printstyled(matched; bold=true,color=:underline)
    li = after>lastindex(s) ? lastindex(s) : _nextind(s,after,delta)
    if state === nothing 
        printstyled(escape_string(s[after:min(end,li)]),
                    bold=true,color=:underline)
    elseif after<=lastindex(s)
        printstyled(escape_string(s[after:min(end,li)]),
                    color=:darkgray)
    end
    println()
    if !get(stdout,:color,false)
        print(" "^(11+length(at)+length(log)+length(before)),"^")
        if length(matched)>1
            print("_"^(length(matched)-2),"^")
        end
        println()
    end
end

function log_effect_match(s,start,after,state,log,delta)
    if state!==nothing && start!=after
        log_effect(s,start,after,state,log,delta)
    end
end




export NamedParser, with_name
"""
    NamedParser{P,S,T} <: WrappedParser{P,S,T}

if doc!="", printing will print the node with this label, and hides constructors.
Struct with
```julia
    name::Symbol
    parser::P
    doc::String
```
"""
@auto_hash_equals struct NamedParser{P,S,T} <: WrappedParser{P,S,T}
    name::Symbol
    parser::P
    doc::String ## rename -> label
    NamedParser(name::Symbol,p_,doc="") =
        let p=parser(p_)
            new{typeof(p),state_type(p),result_type(p)}(name,p,doc)
        end
end
function print_constructor(io::IO,x::NamedParser)
    if x.doc==""
        print_constructor(io,x.parser)
        print(io, " |> ")
    end
    print(io, "with_name(:")
    printstyled(io, x.name, bold=true,color=:red)
    print(io, ")")
end

children(x::NamedParser)     = x.doc=="" ? children(x.parser)     : tuple()
regex_prefix(x::NamedParser) = x.doc=="" ? regex_prefix(x.parser) : ""
regex_inner(x::NamedParser)  = x.doc=="" ? regex_inner(x.parser)  : x.doc
regex_suffix(x::NamedParser) = x.doc=="" ? regex_suffix(x.parser) : ""

"""
    with_name(name::Symbol,x; doc="")

A parser labelled with `name`.
Labels are useful in printing and logging.

See also: [`@with_names`](@ref), [`with_name`](@ref), [`log_names`](@ref)
"""
with_name(name::Symbol, x, doc="") = 
    NamedParser(name,parser(x),doc)

with_name(name::AbstractString,x, doc="") =
    name=="" && doc=="" ? x : NamedParser(Symbol(name),parser(x),doc)

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

export @seq
"""
    @seq(x...)

Create a sequence interleaved with whitespace (horizontal or vertical).
The result_type is omitting whitespace.
"""
macro seq(x...)
    r = if length(x)==1
        x
    else
        quote
            x_ = [$(x...)]
            sSequence( (i < lastindex(x_) ? (e*CombinedParsers.Regexp.whitespace_newline)[1] : e for (i,e) in enumerate(x_))...)
        end
    end
    esc(r)
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
                @syntax $within = Either{Any}()
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
                macro $(Symbol(string(name)*"_str"))(x)
                    $name(x)
                end
                $(expr...)
                pushfirst!($within_expr, $name)
                $name
            end
        else
            dump(block)
            error()
        end
    elseif block.head==Symbol("=")
        name = block.args[1]
        quote
            macro $(Symbol(string(name)*"_str"))(x)
                $name(x)
            end
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
    FlatMap{P,S,Q<:Function,T} <: CombinedParser{S,T}

Like Scala's [fastparse FlatMap](https://www.lihaoyi.com/fastparse/#FlatMap).
See [`after`](@ref)
"""
@auto_hash_equals struct FlatMap{P,S,Q<:Function,T} <: CombinedParser{S,T}
    left::P
    right::Q
    function FlatMap{T}(right::Q, left::P) where {T, P<:CombinedParser, Q<:Function}
        new{P,Tuple{<:Any,<:Any,<:Any},Q,T}(left, right)
    end
end
flatmap_state(old,ls,rp,rs) = tuple_pos(rs), (ls,rp,tuple_state(rs))
left_state(state::Tuple) = state[1]
right_parser(state::Tuple) = state[2]
right_state(state::Tuple) = state[3]


children(x::FlatMap) = ( x.left, x.right )
function print_constructor(io::IO,x::FlatMap)
    print(io, "FlatMap" )
end
FlatMap(right::Function, left, T::Type=Any) = FlatMap{T}(right,parser(left))
FlatMap(right::Function, T::Type, left) = FlatMap{T}(right,parser(left))

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

regex_inner(x::FlatMap)  = error("regex determined at runtime!")


@inline _rightof(str,i,parser::FlatMap,x::Tuple) =
    let li = rightof(str,i,parser.left,tuple_pos(x))
        rightof(str,li,x[2],x[3])
    end

@inline _leftof(str,i,parser::FlatMap,x::Tuple) =
    let li = leftof(str,i,x[2],x[3])
        leftof(str,li,parser.left,tuple_pos(x))
    end

    
function iterate_state(tokf::FlatMap, str, till, posi, next_i, state::Nothing)
    posi = next_i
    lr = iterate_state(tokf.left, str, till, posi, next_i, nothing)
    lr === nothing && return nothing
    next_i_ = tuple_pos(lr)
    rightp = parser(tokf.right(get(tokf.left, str, till, next_i_,next_i,tuple_state(lr))))
    rr = nothing
    while rr === nothing
        rr = iterate_state(rightp, str, till, next_i_, next_i_, nothing)
        if rr === nothing
            lr = iterate_state(tokf.left, str, till, posi, next_i_, tuple_state(lr))
            lr === nothing && return nothing
            next_i_ = tuple_pos(lr)
            rightp = parser(tokf.right(get(tokf.left, str, till, next_i_,posi,tuple_state(lr))))
        else
            return flatmap_state(nothing,tuple_state(lr), rightp, rr)
        end
    end
    nothing
end

function iterate_state(tokf::FlatMap, str, till, posi, next_i, state)
    lstate,rightp,rstate = left_state(state), right_parser(state), right_state(state)

    next_i_=next_i
    posi_ = leftof(str,next_i_,rightp,rstate)
    rr = nothing
    while rr === nothing
        rr = iterate_state(rightp, str, till, posi_, next_i_, rstate)
        if rr === nothing
            lr = iterate_state(tokf.left, str, till, posi, next_i_, lstate)
            lr === nothing && return nothing
            next_i_,lstate = lr
            rightp = parser(tokf.right(get(tokf.left, str, till, next_i_,posi,lstate)))
            rstate = nothing
        else
            return flatmap_state(state,lstate, rightp, rr)
        end
    end
end

export ParserPair
"""
    Sequence{P,S,T}

of `parts::P`, [`sequence_state_type`](@ref)==S and [`sequence_result_type`](@ref).
"""
@auto_hash_equals struct ParserPair{P,Q,S,T} <: CombinedParser{S,T}
    first::P
    second::Q
    ParserPair(first::CombinedParser, second::CombinedParser) =
        new{typeof(first),typeof(second),Tuple{first,second},sequence_result_type(typeof((first,second)))}(p)
end



export Sequence
"""
    Sequence{P,S,T}

of `parts::P`, [`sequence_state_type`](@ref)`==S` with [`sequence_result_type`](@ref)`==T`.

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
"""
@auto_hash_equals struct Sequence{P,S,T} <: CombinedParser{S,T}
    parts::P
    function Sequence(p::CombinedParser...; tuplestate=true)
        if VERSION>=v"1.6" && length(p)>4
            Sequence(Sequence(p[1:2]...; tuplestate=tuplestate),
                     Sequence(p[3:end]...; tuplestate=tuplestate);
                     tuplestate=tuplestate) do v
                         tuple(v[1]..., v[2]...)
                     end
        else
            new{typeof(p),sequence_state_type(p; tuplestate=tuplestate),sequence_result_type(typeof.(p)...)}(p)
        end
    end
end
print_constructor(io::IO,x::Sequence) = print(io,"Sequence")
children(x::Sequence) = isliteralsequence(x) ? tuple() : x.parts
regex_inner(x::Sequence)  = join([ regex_string(p) for p in x.parts])

isliteralsequence(c::ConstantParser) = true
isliteralsequence(c) = false
function isliteralsequence(c::Sequence)
    (&)(isliteralsequence.(c.parts)...)
end

sequence_state_type(x; kw...) = sequence_state_type(typeof(x); kw...)

"""
    sequence_state_type(pts::Type; tuplestate=true)

- `MatchState` if all `fieldtypes` are `MatchState`, 
- otherwise if `tuplestate`, a tuple type with the `state_type` of `parts`,
- or `Vector{Any}` if `!tuplestate`.

!!! note
    Todo: NCodeunitsState instead of MatchState might increase performance.
"""
function sequence_state_type(pts::Type; tuplestate=true)
    if isempty(fieldtypes(pts)) || all(t->state_type(t)<:MatchState, fieldtypes(pts))
        MatchState
    elseif tuplestate
        Tuple{(state_type(p) for p in fieldtypes(pts))...}
    else
        Vector{Any}
    end
end

"""
    sequence_result_type(::Type{T}) where {T<:Tuple}

`Tuple` type, internally used for `Sequence` result_type.
"""
sequence_result_type(T::Type{<:CombinedParser}...) =
    Tuple{ (result_type(t) for t in T)... }

@deprecate Sequence(p::Vector; kw...) Sequence(p...; kw...)

"""
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
function Sequence(p...; kw...)
    s = Sequence(( parser(x) for x = p )...; kw...)
    T = fieldtypes(result_type(s))
    names = ( t.first=>i
              for (i,t) in enumerate(p)
              if t isa Pair{Symbol} )
    isempty(names) && return s
    NT= NamedTuple{ tuple( (n.first for n in names)...),
                    Tuple{ (T[n.second] for n in names)... }}
    NTn = NamedTuple{ tuple( (n.first for n in names)...) }
    function ntuple(v)
        NT( tuple( (v[k.second] for k in names )... ))
    end
    map(ntuple, NT, s)
end



Base.lastindex(x::Sequence) = lastindex(x.parts)

"""
    Sequence(;kw...)

Sequence keyword argument constructors transform the parsing into a named tuple.
"""
Sequence(;tuplestate=true, kw...) =
    isempty(kw) ? Always() : Sequence(kw...; tuplestate=tuplestate)


Sequence(transform::Function, T::Type, a...; kw...) =
    map(transform, T, Sequence(a...; kw...))

Sequence(transform::Function, a...; kw...) =
    map(transform, Sequence(a...; kw...))


Sequence(transform::Integer,tokens...; kw...) =
    Sequence(Val{transform}(),parser.(tokens)...; kw...)

function Sequence(::Val{transform},tokens...; kw...) where {transform}
    s = Sequence(tokens...)
    map(v -> v[transform], fieldtype(result_type(s),transform), s; kw...)
    # map(IndexAt(transform), s)
end


Base.getindex(x::CombinedParser, i) = map(IndexAt(i),x)


_sSequence(x::Sequence) = _sSequence(x.parts...)
_sSequence(x::Always) = tuple()
_sSequence() = tuple()
_sSequence(x1) = tuple(parser(x1))
_sSequence(x1,x...) =
    Iterators.flatten(tuple( _sSequence(x1), collect(Iterators.flatten( ( _sSequence(e) for e in x ) ))))

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
    sSequence(_sSequence(parser.(x)...)...)
end

sSequence(x::CombinedParser) = x
function sSequence(x::CombinedParser...)
    Sequence(_sSequence(x...)...)
end


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

function iterate_state_(parser::Sequence, sequence, till, posi, next_i, states::Nothing)
    length(parser.parts) == 0 && return next_i, sequence_state(state_type(parser))
    sss = Vector{Any}(undef,length(parser.parts))
    sss[1] = nothing
    iterate_state(parser, sequence, till, posi, next_i, sss, 1)
end

function iterate_state_(parser::Sequence, sequence, till, posi, next_i, substate::Vector{Any}, p=length(substate))
    next_i_ = next_i
    part=parser.parts
    length(part) == 0 && return nothing
    pposi = [ 0 for _ in 1:(length(substate)+1)]
    pposi[1]=posi
    if p==length(substate)
        pposi[end]=next_i
    end
    while p<=length(substate)
        if iszero(pposi[p])
            pposi[p] = leftof(sequence, pposi[p+1], part[p], @inbounds substate[p])
        end
        if (@inbounds substate[p]) === nothing
            pposi[p+1] = pposi[p]
        end
        r = iterate_state(part[p], sequence, till, pposi[p], pposi[p+1], substate[p])

        if r === nothing
            prune_captures(sequence, pposi[p])
            @inbounds substate[p] = nothing
            pposi[p+1] = pposi[p]
            p == 1 && return nothing
            p -= 1
        else
            pposi[p+1] = tuple_pos(r)
            @inbounds substate[p] = tuple_state(r)
            if p < length(substate)
                @inbounds substate[p+1]=nothing
            end
            p += 1
        end
    end
    pposi[end], sequence_state(state_type(parser), substate)
end

# unambigously
@generated function iterate_state(parser::Sequence{pts,sts}, sequence, till, posi, next_i, states::MatchState) where {pts<:Tuple,sts}
    nothing
end

@generated function iterate_state(parser::Sequence{pts,sts}, sequence, till, posi, next_i, states)::Union{Nothing,Tuple{Int,sts}} where {pts<:Tuple,sts}
    fpts = fieldtypes(pts)
    spts = Type[ Union{Nothing,state_type(t)} for t in fpts ]
    n = length(fpts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    pposi = Symbol[ gensym(:pos) for p in 1:(n+1) ]
    substate,init = if states<:Nothing
        substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            $(substate[i])::$t = nothing
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    elseif states<:Vector
        substate = Expr[ Expr(Symbol("::"), Expr(:ref,:states,i), t) for (i,(p,t)) in enumerate(zip(fpts,spts)) ]
        ## substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            ## @inbounds $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    elseif states<:Tuple
        substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            @inbounds $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    else
        error("strange sequence state type")
    end

    ret_state = if state_type(parser) <: MatchState
        :(MatchState())
    elseif state_type(parser) <: Tuple
        :(tuple( $([ :(($(s))) for s in substate ]...) ) )
    elseif states <: Nothing
        :(Any[ $([ :(($(s))) for s in substate ]...) ] )
    elseif states <: Vector
        quote
            ## $( [ :(@inbounds states[$i]=$(substate[i])) for i in 1:n ]...)
            states
        end
    else
        error("invalid state_type")
    end
    parseparts = [
        quote
        @label $(subsearch[p])
        if iszero($(pposi[p]))
            $(pposi[p]) = leftof(sequence, $(pposi[p+1]), $(part[p]), @inbounds $(substate[p]))
        end
        if (@inbounds $(substate[p])) === nothing
            ## if sss[$p] === nothing
            $(pposi[p+1]) = $(pposi[p])
        end
        ## TODO: gc happening in next line?
        $(subresult[p]) = iterate_state($(part[p]), sequence, till, $(pposi[p]), $(pposi[p+1]), @inbounds $(substate[p]))
        if $(subresult[p]) === nothing
        prune_captures(sequence,$(pposi[p]))
        @inbounds $(substate[p]) = nothing
        $(pposi[p+1]) = $(pposi[p])
           @goto $(p == 1 ? :theend : subsearch[p-1])
        else
            $(pposi[p+1]) = tuple_pos($(subresult[p]))
            @inbounds $(substate[p]) = tuple_state($(subresult[p]))
        ##$(pposi[p+1]), $(substate[p]) = $(subresult[p])
            $(if p < length(fpts); (:(@inbounds $((substate[p+1]))=nothing)); end )
        end
        end
        for (p,t) in enumerate(fpts)
    ]
    R = quote
        $(init...)
        $(pposi[1]) = posi
        $(pposi[end]) = next_i
        states !== nothing && @goto $(subsearch[end])
        $(parseparts...)
        return $(pposi[end]), $ret_state
        @label theend
        return nothing
    end
    R
end


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
@auto_hash_equals struct Lazy{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    Lazy(p_) =
        let p = parser(p_)
            new{typeof(p),state_type(p),result_type(p)}(p)
        end
end


regex_inner(x::Lazy) = regex_inner(x.parser)
regex_suffix(x::Lazy) = regex_suffix(x.parser)*"?"

function print_constructor(io::IO, x::Lazy)
    print_constructor(io,x.parser)
    print(io, " |> Lazy" )
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
@auto_hash_equals struct Repeat{P,S,T} <: WrappedParser{P,S,T}
    range::UnitRange{Int}
    parser::P
    Repeat(range::UnitRange{Int},p::P) where {P<:CombinedParser} =
        new{P,repeat_state_type(state_type(p)),Vector{result_type(P)}}(range,p)
    # Repeat(p::P) where {P<:CombinedParser} =
    #     new{P,repeat_state_type(state_type(p)),Vector{result_type(P)}}(0:Repeat_max,p)
end
Repeat(range::UnitRange{Int},p...)                  = Repeat(range,sSequence(p...))
Repeat(min::Integer,max::Integer,p...)              = Repeat((min:max),p...)
Repeat(p...;min::Integer=0,max::Integer=Repeat_max) = Repeat((min:max),p...)
Repeat(min::Integer,p...)                           = Repeat((min:Repeat_max),p...)

@inline repeat_state_type(::Type{MatchState}) = Int
@inline repeat_state_type(T::Type) = Vector{T}

"""
    Repeat(f::Function,a...)

Abbreviation for [`Base.map`](@ref)`(f,Repeat(a...))`.
"""
Repeat(f::Union{Function,Type},a...;kw...)           = map(f,Repeat(a...;kw...))

"""
    Repeat1(x)

Parser repeating pattern `x` one time or more.
"""
Repeat1(x...; max=Repeat_max)                        = Repeat(1:Repeat_max,x...)

"""
    Repeat1(f::Function,a...)

Abbreviation for [`Base.map`](@ref)`(f,Repeat1(a...))`.
"""
Repeat1(f::Function,a...; kw...)                     = map(f,Repeat1(a...; kw...))

@deprecate Repeat(minmax::Tuple{<:Integer,<:Integer},x,y::Vararg) Repeat(minmax...,x,y...)

@deprecate Repeat(transform::Function, T::Type, a...) map(transform, T, Repeat(a...))

@deprecate Repeat(transform::Function, minmax::Tuple{<:Integer,<:Integer}, a...) map(transform, Repeat(minmax..., a...))

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
            Sequence(2, delim,x.parser ))) do (f,r)
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

function print_constructor(io::IO,x::Repeat)
    print_constructor(io,x.parser)
    print(io, " |> Repeat" )
end

regex_inner(x::Repeat) = regex_inner(x.parser)
regex_suffix(x::Repeat) = 
    regex_suffix(x.parser)*if x.range.start == 0
        if x.range.stop >= Repeat_max
            "*"
        else            
            "{,$(x.range.stop)}"
        end
    else
        if x.range.stop >= Repeat_max
            if x.range.start == 1
                "+"
            else
                "{$(x.range.start),}"
            end
        elseif x.range.start==x.range.stop
            "{$(x.range.start)}"
        else
            "{$(x.range.start),$(x.range.stop)}"
        end
    end


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
@inline state_length(parser,state::Int) = state
@inline pushstate!(state::Int,parser,substate::MatchState) =
    state + 1

@inline poplast!(outer_state::Int) =
    if iszero(outer_state)
        nothing, 0
    else
        MatchState(), outer_state - 1
    end

@inline poplast!(outer_state,inner_parser) =
    poplast!(outer_state)

@inline state_length(parser::Repeat,x::Vector) =
    length(x)
@inline emptystate(::Type{Vector{T}}) where T =
    T[]

@inline function pushstate!(state::Vector,parser,substate)
    push!(state,substate)
end

@inline function poplast!(outer_state::Vector)
    l=pop!(outer_state)
    l,outer_state
end

## kernel function (function barrier)
@inline function fill_rep_j_state(x::Tuple{Int,S},state_,tparser) where S
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
        j, state_ = fill_rep_j_state(x,state_,tp)
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

function iterate_state(t::Repeat, sequence, till, posi, next_i, state)
    next_i_::Int,outer_state::state_type(typeof(t)),goback::Bool = if state === nothing
        es = emptystate(state_type(typeof(t)))
        fill_rep(t,sequence,till,next_i, es)
    else
        if state_length(t,state)==0
            return nothing
            # https://www.pcre.org/original/doc/html/pcrepattern.html:
            # It is possible to construct infinite loops by following
            # a subpattern that can match no characters with a
            # quantifier that has no upper limit, for example:
            
            #   (a?)*
            
            # Earlier versions of Perl and PCRE used to give an error
            # at compile time for such patterns. However, because
            # there are cases where this can be useful, such patterns
            # are now accepted, but if any repetition of the
            # subpattern does in fact match no characters, the loop is
            # forcibly broken.
        #     return nothing
        end
        next_i, state, true
    end
    while goback
        if state_length(t,outer_state)==0
            return nothing
        end
        inner_state, outer_state=poplast!(outer_state,t.parser)
        posi = leftof(sequence,next_i_,t.parser,inner_state) ##state[end][1]
        prune_captures(sequence,posi)
        x = iterate_state(t.parser,sequence, till, posi, next_i_, inner_state)
        x === nothing && state_length(t,outer_state) in t.range && return posi, outer_state
        next_i_,outer_state,goback = push_rep(t,sequence, till, posi, x, outer_state)
    end
    if state_length(t,outer_state) in t.range
        next_i_, outer_state
    else
        nothing
    end
end

@inline function fill_rep(t_::Lazy{<:Repeat}, sequence, till::Int, j::Int, state_)
    t = t_.parser
    tp = t.parser
    while state_length(t,state_) < t.range.start && (x = iterate_state(t.parser,sequence, till,j, j,nothing))!==nothing 
        j_=j
        j, state_ = fill_rep_j_state(x,state_,tp)
        j_==j && break
    end
    j,state_,false
end
function iterate_state(t_::Lazy{<:Repeat}, sequence, till, posi, next_i, state)
    t = t_.parser
    next_i_::Int,state_::state_type(typeof(t)),goback::Bool = if state === nothing
        es = emptystate(state_type(typeof(t)))
        fill_rep(t_,sequence,till,next_i, es)
    else
        if state_length(t,state)<t.range.stop
            x = iterate_state(t.parser,sequence, till, next_i, next_i, nothing)
            if x!==nothing && ( tuple_pos(x)>next_i || state_length(t,state)==0)
                return fill_rep_j_state(x,state,t.parser) #tuple_pos(x),pushstate!(state,t.parser,tuple_state(x))
            end
        end
        next_i, state, true
    end

    while goback
        if state_length(t,state)==0
            return nothing
        end
        lstate, state_=poplast!(state,t.parser)
        posi = leftof(sequence,next_i_,t.parser,lstate) ##state[end][1]
        x = iterate_state(t.parser,sequence, till, posi, next_i_, lstate)
        if x === nothing
            next_i_ = posi
            prune_captures(sequence,next_i_)
            if state_length(t,state_)==0
                return nothing
            end
            state = state_
        else
            next_i_,state_ = pushstate!_fill_rep(t_, sequence, till, state_, x)
            if state_length(t,state_) in t.range
                goback = false
            end
        end
    end
    if state_length(t,state_) in t.range ## && state_length(t,state_)>0
        return next_i_, state_
    else
        nothing
    end
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
defaultvalue(::Type{<:AbstractString}) = ""
defaultvalue(V::Type{<:Vector}) = eltype(V)[]
defaultvalue(V::Type) = missing
defaultvalue(V::Type{<:CombinedParser}) = Always()


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
@auto_hash_equals struct Optional{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    default::T
    function Optional(p::CombinedParser;default=defaultvalue(result_type(p)))
        T = result_type(p)
        D = typeof(default)
        T_ = promote_type(T,D)
        T_ === Any && ( T_ = Union{T,D} )
        new{typeof(p),Union{NoMatch,state_type(p)},T_}(p, default)
    end
end

Optional(x...;kw...) =
    Optional(sSequence(x...); kw...)


Optional(T::Type, x_; transform, kw...) =
    Optional(transform, T, x; kw...)

function Optional(transform::Function, T::Type, x;
                  default=defaultvalue(T))
    map(transform,T,Optional(x; default=default))
end



children(x::Optional) = children(x.parser)
regex_inner(x::Optional) = regex_inner(x.parser)
regex_suffix(x::Optional) = regex_suffix(x.parser)*"?"

function print_constructor(io::IO, x::Optional)
    print_constructor(io,x.parser)
    printstyled(io, "|$(x.default)",color=:blue)
    #print(io, " |> Optional(default=$(x.default))")
end


@inline _leftof(str,i,parser::Optional,x::NoMatch) = i
@inline _rightof(str,i,parser::Optional,x::NoMatch) = i



function iterate_state(t::Optional, str, till, posi, next_i, state::MatchState)
    prune_captures(str,posi)
    posi, NoMatch()
end

iterate_state(t::Optional, str, till, posi, next_i, state::NoMatch) =
    nothing

function iterate_state(t::Optional, str, till, posi, next_i, state)
    posi = state === nothing ? next_i : leftof(str,next_i,t.parser,state) ##state[end][1]
    r = iterate_state(t.parser, str, till, posi, next_i, state)
    if r === nothing
        prune_captures(str,posi)
        return tuple(posi, NoMatch())
    else
        r
    end
end

iterate_state(t::Lazy{<:Optional}, str, till, posi, next_i, state::Nothing) =
    next_i, NoMatch()
iterate_state(t::Lazy{<:Optional}, str, till, posi, next_i, state::NoMatch) =
    iterate_state(t.parser.parser, str, till, posi, next_i, nothing)
iterate_state(t::Lazy{<:Optional}, str, till, posi, next_i, state) =
    iterate_state(t.parser.parser, str, till, posi, next_i, state)






export Either
export Delayed

"""
    Either{S,T}(p) where {S,T} = new{typeof(p),S,T}(p)

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
"""
struct Either{Ps,S,T} <: CombinedParser{S,T}
    options::Ps
    Either{S,T}(p) where {S,T} = new{typeof(p),S,T}(p)
end

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

"""
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
"""
function Either(p_...; simplify=false)
    os = either_options(p_...; simplify=simplify)
    simplify && length(os)==1 && return first(os)
    p = tuple(os...)
    Either{either_state_type(p),either_result_type(p)}(p)
end

"""
    Either(p::Vector; simplify=false)

Create a mutable `Either{Any,Any}(::Vector{Any})` for creating recursive parsers.
Arguments `p...` are wrapped in [`parser`](@ref),
type parameters are computed with [`either_state_type`](@ref) and [`either_result_type`](@ref).

See also [`@syntax`](@ref).
!!! note
    state type and result type are `Any` which might cost performance.
"""
function Either(p_::Vector; simplify=false)
    p = either_options(p_...; simplify=simplify)
    Either{Any,Any}(p)
end


"""
    Either{T}(p...; simplify=false, convert=false)

Create a mutable `Either{Any,T}(::Vector{Any})` for creating recursive parsers.
Options can be added with [`push!`](@ref) and [`pushfirst!`](@ref).

If `convert` for any option `x` in `p` that has `!(result_type(x) <: T)`, adds [`Base.map`](@ref)`(T,x)` instead.
(Provide a `convert` method!)

See also [`@syntax`](@ref).
!!! note
    state type is `Any` which might cost performance.
"""
function Either{T}(p_...; convert=false, simplify=false) where T
    p = either_options(p_...; simplify=simplify)
    for (i,x) in enumerate(p)
        if !(result_type(x) <: T)
            convert || error("transforming results with convert($T,::$(result_type(x)))\n$x")
            p[i] = map(T,x)
        end 
    end
    Either{Any,T}(p)
end

@deprecate Either{T}(x::Vector; kw...) where T Either{T}(x...; kw...)
@deprecate Either{T}(x::Tuple; kw...) where T Either(x...; kw...)
@deprecate Either(x::Tuple; kw...) Either(x...; kw...)

"""
    Delayed(T::Type) = 

[`Either`](@ref)`{T}()`.
"""
Delayed(T::Type) = Either{T}()

"""
    Either(transform::Function, x::Vararg)

abbreviation for [`Base.map`]`(transform, Either(x...))`.
"""
function Either(transform::Function, x...; kw...)
    map(transform, Either(x...;kw...))
end

_sEither(x::Either) = _sEither(x.options...)
_sEither(x::Never) = tuple()
_sEither() = tuple()
_sEither(x1) = tuple(parser(x1))
## todo: better aggregate in argument?
_sEither(x1,x...) = Iterators.flatten( Any[ _sEither(x1), ( _sEither(e) for e in x )... ] )

function either_options(x...; simplify = false)
    Any[if simplify
            _sEither(x...)
        else
            parser.(x)
        end...]
end
@deprecate sEither(x...) Either(x...; simplify=true)


either_state_type(ts::Type{Vector{Any}}) = Tuple{Int,Any}
either_state_type(ts::Type{<:CombinedParser}) = Tuple{Int,Any}
either_state_type(ts::Type{<:Vector}) = Tuple{Int,state_type(eltype(ts))}
either_state_type(ts::Type{<:Tuple}) = Tuple{Int,promote_type(state_type.(fieldtypes(ts))...)}
either_state_type(ts::Type...) = Tuple{Int,promote_type(state_type.(ts))}
either_state_type(x) = either_state_type(typeof(x))
@inline with_state!(x::Nothing,k::Int,s) = (k,s)
function promote_type_union(Ts...)
    T = promote_type(Ts...)
    Any <: T ? Union{Ts...} : T
end

"return tuple(state_type,result_type)"
function either_result_type(ts)
    promote_type_union(result_type.(ts)...)
end

children(x::Either) = x.options
regex_string(x::Either) = join(regex_string.(x.options),"|")
regex_prefix(x::Either) = "|"
regex_inner(x::Either) = join([ regex_string(p) for p in x.options],"|")
regex_suffix(x::Either) = ""
print_constructor(io::IO,x::Either) = print(io,"Either")


"""
    Base.push!(x::Either, option)

Push `option` to `x.options` as parser tried next if `x` fails.

Recursive parsers can be built with `push!` to `Either`.

See also [`pushfirst!`](@ref) and [`@syntax`](@ref).
"""
function Base.push!(x::Either{<:Vector,<:Any}, y_)
    y = parser(y_)
    promote_type(result_type(y),result_type(x)) <: result_type(x) || error("$(result_type(y)) <: $(result_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
    promote_type(state_type(y),state_type(x)) <: state_type(x) || error("$(state_type(y)) <: $(state_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
    push!(x.options,y)
    y
end

"""
    Base.pushfirst!(x::Either, option)

Push `option` to `x.options` as parser tried first, and trying `x` if `option` fails.

Recursive parsers can be built with `pushfirst!` to `Either`.

See also [`push!`](@ref) and [`@syntax`](@ref).
"""
function Base.pushfirst!(x::Either{<:Vector,<:Any}, y_)
    y = parser(y_)
    promote_type(result_type(y),result_type(x)) <: result_type(x) || error("$(result_type(y)) <: $(result_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
    promote_type(state_type(y),state_type(x)) <: state_type(x) || error("$(state_type(y)) <: $(state_type(x)). Fix with `push!(x|$(typeof(y)),y)`.\n$y")
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
@inline function _rightof(str,i,parser::Either{P,T},x::Tuple{Int,T}) where {P,T}
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


@inline function iterate_state_paired(first,state,sstate::Nothing)
    nothing
end

@inline function iterate_state_paired(first, state, sstate::Tuple)
    iterate_state_paired(first, state, sstate...)
end

@inline function iterate_state_paired(first, state, next_i_::Int, nstate_)
    next_i_, with_state!(state,first,nstate_)
end

function iterate_state_paired(first, t, str, till, posi, next_i, state)
    iterate_state_paired(first, state, iterate_state(t, str, till, posi, next_i, either_state_state(state)))
end

function iterate_state(t::Either{<:Vector}, str, till, posi, next_i, state::Nothing)
    r = nothing
    for (j,o) in enumerate(t.options)
        r = iterate_state_paired(j,o,str,till,posi, next_i,nothing)
        r!== nothing && return r
    end
    nothing
end

function iterate_state(t::Either{<:Vector}, str, till, posi, next_i, state)
    @inbounds opt = t.options[either_state_option(state)]
    fromindex = either_state_option(state)+1
    posi = leftof(str,next_i,opt,either_state_state(state)) ##state[end][1]
    r = iterate_state_paired(either_state_option(state),opt,str,till,posi, next_i,state)
    r !== nothing && return r
    prune_captures(str,posi)
    ##sstate = nothing
    for j in fromindex:length(t.options)
        @inbounds r2 = iterate_state_paired(j,t.options[j],str,till,posi,posi,nothing)
        r2 !== nothing && return r2
    end
    nothing
end


function iterate_state(parser::Either{<:Tuple}, sequence, till, posi, next_i, state)
    either_first(parser,posi,next_i,state) do index, option, ni, sstate
        iterate_state_paired(index, option, sequence, till, posi, ni, sstate)
    end
end



@generated function either_first(f::Function, parser::Either{pts}, posi, next_i, state) where {pts<:Tuple}
    fpts = fieldtypes(pts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    push!(subsearch, gensym(:subsearch))
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    init = Expr(:(=), Expr(:tuple, part...),:(parser.options))
    parseoptions = [
        quote
        @label $(subsearch[p])
        j > $p && @goto $(subsearch[p+1])
        $(subresult[p]) = f($p,$(part[p]), next_i_, sstate)
        $(subresult[p]) !== nothing && return $(subresult[p])# iterate_state_paired($p,state, $(subresult[p]))
        next_i_ = posi
        sstate = nothing
        end
        for (p,t) in enumerate(fpts)
    ]
    init_before = 
        quote
            j = either_state_option(state)
            sstate = state
        end
    R = quote
        next_i_::Int = next_i
        $(init)
        $(init_before)
        $(parseoptions...)
        @label $(subsearch[end])
        return nothing
    end
    R
end

include("trie.jl")


AtomicState = NCodeunitsState{MatchState}

export Atomic
"""
    Atomic(x)

A parser matching `p`, and failing when required to backtrack
(behaving like an atomic group in regular expressions).
"""
@auto_hash_equals struct Atomic{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    Atomic(p::CombinedParser) =
        new{typeof(p),state_type(p),result_type(p)}(p)
    Atomic{MatchState}(p::CombinedParser) =
        new{typeof(p),AtomicState,result_type(p)}(p)
end
Atomic(p) = Atomic(parser(x))

regex_prefix(x::Atomic) = "(?>"*regex_prefix(x.parser)
regex_suffix(x::Atomic) = regex_suffix(x.parser)*")"
function Base.get(parser::Atomic, sequence, till, after, i, state::AtomicState)
    a, s = iterate_state(parser.parser, sequence, till, i, i, nothing)
    get(parser.parser, sequence, till, after, i, s)
end

@inline iterate_state(parser::Atomic, sequence, till, posi, next_i, state::Nothing) =
    iterate_state(parser.parser, sequence, till, posi, next_i, state)
@inline iterate_state(parser::Atomic{<:Any,AtomicState}, sequence, till, posi, next_i, state::Nothing) =
    AtomicState(iterate_state(parser.parser, sequence, till, posi, next_i, state))
@inline iterate_state(parser::Atomic, sequence, till, posi, next_i, state) =
    nothing


function print_constructor(io::IO,x::Atomic)
    print_constructor(io,x.parser)
    print(io, " |> Atomic" )
end


include("match.jl")


include("caseless.jl")

include("reverse.jl")

include("get.jl")
include("transformation.jl")

include("operators.jl")

include("deepmap.jl")

function _log_names(x::CombinedParser,message::Function,a...;kw...)
    log = message(x,a...; kw...)
    if log!==nothing
        with_log("$(log)",x)
    else
        x
    end
end

export log_parser, log_names

"""
    log_names(x,names=true; exclude=nothing)

Rebuild parser replacing `NamedParser` instances with `with_log` parsers.
Log all `NamedParser` instanses if `names==true` or `name in names` and not `name in exclude`.

See also: [`with_log`](@ref), [`log_parser`](@ref), [`deepmap_parser`](@ref)
"""
function log_names(x, names=true; exclude=nothing)
    message = if names === true
        if exclude === nothing
            x -> x isa NamedParser ? x.name : nothing
        else
            x -> ( x isa NamedParser && !in(x.name,exclude) ) ? x.name : nothing
        end
    elseif names isa Type
        return log_parser(names, x)
    else
        x -> ( x isa NamedParser && in(x.name,names) ) ? x.name : nothing
    end
    log_parser(message, x)
end

iostring(f::Function, a...; kw...) =
    let sio = IOBuffer()
        f(sio, a...; kw...)
        String(take!(sio))
    end

function lognode(message)
    p -> 
        if p isa message
            iostring(printnode, p)
        else
            nothing
        end
end

"""
    log_parser(message::Type, x::CombinedParser, a...; kw...)
    log_parser(message::Function, x::CombinedParser, a...; kw...)

Transform parser including logging statements for sub-parsers 
of type `message` or 
for which calling `message` does not return `nothing`.
"""
function log_parser(message::Type, x::CombinedParser, a...; kw...)
    log_parser(lognode(message), x, a...; kw...)
end

function log_parser(message::Function, x::CombinedParser, a...; kw...)
    deepmap_parser(_log_names,Dict(),x,message, a...;kw...)
end

export optimize
optimize(x) = deepmap_parser(_optimize,x)
_optimize(x,a...) = x
_deepmap_parser(::typeof(_optimize),dict::AbstractDict,x::SideeffectParser) = x.parser

include("defaults.jl")
include("re.jl")


include("show.jl")

include("memoize.jl")


include("lazy.jl")

include("bnf.jl")
end # module
