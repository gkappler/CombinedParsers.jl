############################################################
## get methods
## - reorganise result_type here?

function Base.get(parser::WrappedParser, sequence, till, after, i, state)
    get(parser.parser, sequence, till, after, i, state)
end


"""
    Base.get(parser::Bytes{N,T}, sequence::Vector{UInt8})

!!! note

    Endianness can be achieved by just mapping `bswap`
    ```jldoctest
    julia> map(bswap, Bytes(2,UInt16))([0x16,0x11])
    0x1611

    julia> Bytes(2,UInt16)([0x16,0x11])
    0x1116
    ```
"""
function Base.get(parser::Bytes{N,T}, sequence::Vector{UInt8}, till,
                  after, i, state) where {N,T}
    if isbitstype(T)
        reinterpret(T,sequence[i:after-1])[1]
    else
        T(sequence[i:after-1])
    end
end

function Base.get(parser::AbstractTokenParser,
                  sequence, till,
                  after, i, state)
    state.state
end



"""
    Base.get(parser::CombinedParser{Nothing}, sequence, till, after, i, state)

Default method for parser types returning nothing
"""
Base.get(::CombinedParser{<:Nothing},
         sequence, till, after,
         i, state) =
             nothing


export JoinSubstring
"""
    JoinSubstring(x)
    (!)(x::CombinedParser)

Parser Transformation getting the matched SubString.
"""
@auto_hash_equals struct JoinSubstring{P,S} <: WrappedParser{P,S,SubString}
    parser::P
    JoinSubstring(x) =
        new{typeof(x),state_type(x)}(x)
end
Base.map(f::Type{<:JoinSubstring}, p::CombinedParser) = JoinSubstring(p)
reversed(x::JoinSubstring) = JoinSubstring(reversed(x.parser))
function print_constructor(io::IO,x::JoinSubstring)
    print_constructor(io,x.parser)
    printstyled(io," |> !", color=:blue)
end

deepmap_parser(f::Function,mem::AbstractDict,x::JoinSubstring,a...;kw...) =
    get!(mem,x) do
        JoinSubstring(
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end

export map_match
map_match(f::Function,p_) =
    map(f, JoinSubstring(parser(p_)))

function Base.get(x::Union{JoinSubstring,ConstantParser{<:AbstractString}}, sequence, till, after, i, state)
    li = _prevind(sequence,after)
    li<i ? "" : @inbounds SubString(sequence,i,li)
end

function Base.get(parser::Union{NIndexParser{1}, ConstantParser{Char}}, sequence, till, after, i, state)
    @inbounds sequence[i]
end

function Base.get(parser::ConstantParser, sequence, till, after, i, state)
    parser.parser
end

# Base.get(parser::ConstantParser, sequence, till, after, i, state) = parser.parser



function Base.get(parser::JoinSubstring{<:NamedParser}, sequence, till, after, i, state)
    parser.parser.name => get(parser.parser.parser, sequence, till, after, i, state)
    
end

function Base.get(parser::FlatMap, sequence, till, after, i, state)
    li = rightof(sequence,i,parser.left,tuple_pos(state))
    get(right_parser(state),
        sequence, till,
        after, li,
        right_state(state))
end

function Base.get(parser::Either,
                  sequence, till,
                  after, i, state)
    get(parser.options[either_state_option(state)],
        sequence, till,
        after, i, either_state_state(state))
end

function Base.get(parser::Either{<:Trie},
                  sequence, till,
                  after, i, state::NCodeunitsState)
    get(state.state)
end

_copy(x::Vector) = copy(x)
_copy(x::AbstractDict) = copy(x)
_copy(x::AbstractSet) = copy(x)
_copy(x) = x

Base.get(parser::Optional, sequence, till, after, i, state::NoMatch) = 
    _copy(parser.default)

Base.get(parser::Optional, sequence, till, after, i, state) = 
    get(parser.parser,sequence, till, after, i, state)

function Base.get(parser::Repeat,
                  sequence, till,
                  after, i, state::Vector)
    r = Vector{result_type(parser.parser)}(undef,length(state))
    i_=i
    for (p,s) in enumerate(state)
        after_ = rightof(sequence,i_,parser.parser,s)
        @inbounds r[p] = get(parser.parser, sequence, till, after_, i_, s)
        i_=after_
    end
    r
end

function Base.get(parser::Repeat,
                  sequence, till,
                  after, i, state::Int)
    r = Vector{result_type(parser.parser)}(undef,state)
    i_=i
    s=MatchState()
    for p in 1:state
        after_ = rightof(sequence,i_,parser.parser,s)
        @inbounds r[p] = get(parser.parser,sequence, till, after_, i_, s)
        i_=after_
    end
    r
end


# Base.get(parser::Sequence, sequence, till, after, i, state::MatchState) =
#     get(parser, sequence, till, after, i, ( MatchState() for i in 1:length(parser.parts)) )
# function Base.get(parser::Sequence, sequence, till::Int, after::Int, i::Int, state)
#     r = Vector{Any}(undef,length(parser.parts))
#     i_::Int = i
#     for (p,s) in enumerate(state)
#         after_ = rightof(sequence,i_,parser.parts[p],s)
#         r[p] = get(parser.parts[p],sequence, till, after_, i_, s)
#         i_=after_
#     end
#     1
#     tuple(r...)
# end
@generated function get(parser::Sequence{pts,sts}, sequence, till::Int, after::Int, posi::Int, states) where {pts,sts}
    fpts = fieldtypes(pts)
    spts = Type[ Union{Nothing,state_type(t)} for t in fpts ]
    n = length(fpts)
    subresult = Symbol[ gensym(:r) for i in fpts ]
    part = Symbol[ gensym(:part) for i in fpts ]
    pposi = Symbol[ gensym(:pos) for i in 1:(n+1) ]
    substate = Symbol[ gensym(:s) for i in fpts ]
    init = if states<:MatchState
        [
            quote
            $(substate[i])::MatchState = MatchState()
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(t,p)) in enumerate(zip(spts,fpts))
        ]
    else
        [
            quote
            $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(t,p)) in enumerate(zip(spts,fpts))
        ]
    end
    parseparts = [
        quote
        $(pposi[i+1]) = rightof(sequence,$(pposi[i]),$(part[i]),$(substate[i]))
        $(subresult[i]) = get($(part[i]),sequence, till, $(pposi[i+1]), $(pposi[i]), $(substate[i]))
        end
        for (i,t) in enumerate(fpts)
    ]
    R = quote
        $(pposi[end])::Int = after
        $(init...)
        $(pposi[1]) = posi
        $(parseparts...)
        tuple($(subresult...))
    end
    R
end

## Base.get(x::Palimdrome, str, state) = SubString(str,state...)
## TODO: dispatch Base.get(x::CombinedParser, str, till::Int, after::Int, posi::Int, state) = Base.get(x, str, after, posi, state)
## TODO: dispatch Base.get(x::CombinedParser, str, after::Int, posi::Int, state) = Base.get(x, str, posi, state)
## TODO: dispatch Base.get(x::CombinedParser, str, posi::Int, state) = Base.get(x, str, state)

export Transformation
"""
    Transformation{T}(f::Function, p_) where {T}
    Base.map(f::Function, Tc::Type, p::CombinedParser, a...)
    Base.map(f::Function, p::CombinedParser, a...)

Parser transforming result of a wrapped parser. 
`a...` is passed as additional arguments to `f` (at front .
"""
@auto_hash_equals struct Transformation{F,P,S,T} <: WrappedParser{P,S,T}
    transform::F
    parser::P
    Transformation(T::Type, p_) = 
        let p = parser(p_)
            new{Type,typeof(p),state_type(p),T}(T, p)
        end
    Transformation{T}(transform, p_) where {T} =
        let p = parser(p_)
            new{typeof(transform),typeof(p),state_type(p),T}(transform, p)
        end
    Transformation{T}(transform, p_::NamedParser) where {T} =
        let p = p_.parser
            tp = new{typeof(transform),typeof(p),state_type(p),T}(transform, p)
            with_name(p_.name,tp)
        end
end
deepmap_parser(f::Function,mem::AbstractDict,x::Transformation,a...;kw...) =
    get!(mem,x) do
        Transformation{result_type(x)}(
            x.transform,
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end

export MatchRange
"""
    MatchRange(p::CombinedParser)

Succeed iif `p` succeeds, if so results in sequence match index `UnitRange`.
"""
struct MatchRange
end
MatchRange(p::CombinedParser) =
    Transformation{UnitRange{Int}}(MatchRange(), p)

Base.show(io::IO, x::MatchRange) = print(io,"@")

"""
    Base.get(parser::Transformation{MatchRange}, a...)

Does not evaluate `get(parser.transform,...)`.
"""
function Base.get(parser::Transformation{MatchRange}, sequence, till, after, i, state)
    i:_prevind(sequence,after)
end



"""
A Transformation{<:Constant} skips evaluation of get(.parser).
"""
struct Constant{T}
    value::T
end
Base.show(io::IO, x::Constant) = show(io,x.value)
function map_constant(transform, p::CombinedParser)
    T=typeof(transform)
    Transformation{T}(Constant(transform), p)
end
"""
    parser(constant::Pair)
    map_constant(constant, parser::CombinedParser)

A parser mapping matches of `parser` `x.first` to `constant` `x.second`.
"""
parser(constant::Pair) =
    map_constant(constant.second, parser(constant.first))
"""
    Base.get(parser::Transformation{<:Constant}, a...)

does not evaluate `get(parser.transform,...)`.
"""
function Base.get(parser::Transformation{<:Constant}, sequence, till, after, i, state)
    parser.transform.value
end

function _string(io::IO,x::Constant)
    print(io,"Constant(")
    show(io,x.value)
    print(io,")")
end
_string(io::IO,x::Function) = print(io,x)
_string(io::IO,x::Type) = print(io,x)
children(x::Transformation) = children(x.parser)
function print_constructor(io::IO,x::Transformation)
    print_constructor(io,x.parser)
    print(io," |> map(")
    printstyled(io,x.transform, color=:blue)
    print(io,")")
end

function print_constructor(io::IO,x::Transformation{Type, <:JoinSubstring})
    print_constructor(io,x.parser)
    print(io,"!")
end

function print_constructor(io::IO,x::Transformation{<:Constant})
    print_constructor(io,x.parser)
    printstyled(io," => ",string(x.transform), color=:blue)
    
end

"""
    Base.get(parser::Transformation{<:Function}, a...)
    Base.get(parser::Transformation{<:Type}, a...)

Function call `parser.transform(get(parser.parser,a...))`.
"""
function Base.get(parser::Transformation{<:Function}, sequence, till, after, i, state)
    v = get(parser.parser, sequence, till, after, i, state)
    parser.transform(v)
end
function Base.get(parser::Transformation{<:Type}, sequence, till, after, i, state)
    v = get(parser.parser, sequence, till, after, i, state)
    parser.transform(v)
end

export IndexAt
"""
Struct for fast access to an index of a `Transformation`.

```jldoctest
julia> using CombinedParsers.Regexp

julia> p = re"(?:a|b*)."[1]
🗄 Sequence[1]
├─ |🗄... Either
│  ├─ a 
│  └─ b*  |> Repeat
└─ [^\\n] CharNotIn
::Union{Char, Array{Char,1}}
```

See also [`getindex`](@ref), [`Sequence`](@ref).
"""
struct IndexAt{I}
    i::I
end

"""
    Base.get(parser::Transformation{<:IndexAt}, a...)

`getindex(get(parser.parser,a...).parser.transform)`
"""
function Base.get(parser::Transformation{IndexAt{I}}, sequence, till, after, i, state) where {I <: Integer}
    v = get(parser.parser,sequence, till, after, i, state)
    v[parser.transform.i]
end
function Base.get(parser::Transformation{IndexAt{Is}}, sequence, till, after, i, state) where {Is <: Union{Tuple, Vector, UnitRange}}
    tuple(get(parser.parser,sequence, till, after, i, state)[parser.transform.i]...)
end

function print_constructor(io::IO,x::Transformation{<:IndexAt})
    print_constructor(io,x.parser)
    printstyled(io,"[",x.transform.i,"]", color=:blue)
end

"""
    infer_result_type(f::Function,Tc::Type,p::CombinedParser,onerror::AbstractString,ts::Type...; throw_empty_union=true)

Used by Parser Transformations to infer result type of a parser.
Throws error if type inference fails, if throw_empty_union=true.
"""
function infer_result_type(f::Function,Tc::Type,p::CombinedParser,onerror::AbstractString,ts::Type...; throw_empty_union=true)
    Ts = Base.return_types(f, tuple(result_type(p),ts...))
    isempty(Ts) && error("transformation type signature mismatch $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    ( length(Ts) > 1 || Any <: first(Ts) ) && return Tc ##error(onerror*"  $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    T = first(Ts)
    if throw_empty_union && T <: Union{}
        error("transformation type signature mismatch $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    elseif T <: Tc
        T
    else
        @warn "type mismatch $f$(tuple(result_type(p),ts...))::$T<:$Tc"
        Tc
    end
end




# export Index
# Index = map_at((v,i) -> i, Always())

import Base: map
"""
    map(f::Function, p::CombinedParser, a...)

Parser matching `p`, transforming parsing results (`x`) with function `f(x,a...)`.

See also: [`get`](@ref), [`Transformation`](@ref)
"""
function Base.map(f::Function, p::CombinedParser, a...;
                  throw_empty_union=true)
    T = infer_result_type(f,Any,p,"call seq(function,type,parts...)",typeof.(a)...;
                          throw_empty_union=throw_empty_union)
    Transformation{T}(isempty(a) ? f : v -> f(v, a...), p)
end

"""
    map(T::Type, p::CombinedParser, a...)

Parser matching `p`, transforming `p`s parsing result with constructor `T(x,a...)`.

See also: [`get`](@ref), [`Transformation`](@ref)
"""
function Base.map(Tc::Type, p::CombinedParser, a...)
    Transformation{Tc}(isempty(a) ? Tc : v -> Tc(a..., v), p)
end


function Base.map(inner::CombinedParser, p::CombinedParser)
    Transformation{result_type(inner)}(s -> parse(inner,s), p)
end

"""
    map(index::IndexAt, p::CombinedParser, a...)
    map(constant, p::CombinedParser, a...)

Parser matching `p`, transforming `p`s parsing results to `getindex(x,index)` or `constant`.

See also: [`get`](@ref), [`Transformation`](@ref)

"""
function Base.map(index::IndexAt{<:Integer}, p::CombinedParser)
    T=result_type(p)    
    Transformation{fieldtype(T,index.i)}(index, p)
end
function Base.map(index::IndexAt{<:UnitRange}, p::CombinedParser)
    T=Tuple{fieldtypes(result_type(p))[index.i]...}
    Transformation{T}(index, p)
end
function instance(Tc::Type, p::CombinedParser, a...)
    Transformation{Tc}((v) -> Tc(a..., v), p)
end
function instance(Tc::Type, p::CombinedParser)
    Transformation(Tc, p)
end
function Base.map(f::Function, Tc::Type, p::CombinedParser, a...)
    T = infer_result_type(f,Tc,p,"call seq(function,type,parts...)",typeof.(a)...)
    Transformation{Tc}(isempty(a) ? f : v -> f(v, a...), p)
end
Base.map(f::typeof(identity), p::CombinedParser) = p


@deprecate map(T::Type, f::Function, p::CombinedParser, a...) map(f,T,p,a...)
@deprecate instance(f::Function,p,a...) map(f,parser(p),a...)

