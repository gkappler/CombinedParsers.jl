import Base: map
export Transformation
"""
    Transformation(T::Type, parser)
    Transformation{T}(transform, parser) where {T}
    Base.map(f::Function, Tc::Type, p::CombinedParser, a...)
    Base.map(f::Function, p::CombinedParser, a...)

Parser transforming result of a wrapped parser. 
`a...` is passed as additional arguments to `f` (at front .


If `parser isa NamedParser`, transformation is done within the wrapped parser
(i.e. name applies to result-transforming parser).
"""
@auto_hash_equals struct Transformation{F,P} <: WrappedParser{P}
    transform::F
    parser::P
    Transformation(transform, p_) =
        let p = parser(p_)
            new{typeof(transform),typeof(p)}(transform, p)
        end
    function Transformation(transform, p::NamedParser) 
        tp = new{typeof(transform),typeof(p.parser)}(transform, p.parser)
        with_name(p.name, tp, p.doc)
    end
end

result_type(p::Transformation{<:Function}, sequence; kw...)  =
    infer_result_type(p.transform, Any, p.parser, sequence,
                      "call seq(function,type,parts...)";
                      kw...)

result_type(p::Transformation{<:Type}, sequence; kw...) =
    p.transform

_deepmap_parser(f::Function,mem::AbstractDict,x::Transformation,a...;kw...) =
    Transformation(
        x.transform,
        deepmap_parser(f,mem,x.parser,a...;kw...))

"""
    Base.get(parser::Transformation{<:Function}, a...)
    Base.get(parser::Transformation{<:Type}, a...)

Function call `parser.transform(get(parser.parser,a...))`.
"""
function Base.get(parser::Transformation{<:Function}, sequence, till, after, i, state)
    v = get(parser.parser, sequence, till, after, i, state)
    parser.transform(v)
end

function Base.get(parser::Transformation{T}, sequence, till, after, i, state) where {T<:Type}
    v = get(parser.parser, sequence, till, after, i, state)
    if isbitstype(parser.transform)
        reinterpret(parser.transform,v)[1]
    else
        v isa parser.transform ? v : parser.transform(v)
        #T(sequence[i:after-1])
    end
end


export MatchedSubSequence
import Base: (!)
struct MatchedSubSequence end


@deprecate MatchedSubSequence(x) map(MatchedSubSequence(), parser(x))
export JoinSubstring
"""
    JoinSubstring = MatchedSubSequence

Deprecated but kept (because legacy `join(...; wrap=JoinSubstring)` syntax does not word with `@deprecate`).
"""
JoinSubstring = MatchedSubSequence

Base.map(::Type{MatchedSubSequence}, x::CombinedParser) = 
    Transformation(MatchedSubSequence(), x)
Base.map(::MatchedSubSequence, x::CombinedParser) = #
    Transformation(MatchedSubSequence(), x)

result_type(p::Transformation{MatchedSubSequence}, sequence::AbstractString) =
    SubString{typeof(sequence)}

function Base.get(x::Union{Transformation{MatchedSubSequence},
                           ConstantParser{<:AbstractString}},
                  sequence, till, after, i, state)
    li = _prevind(sequence,after)
    li<i ? "" : @inbounds SubString(sequence,i,li)
end

(!)(x::CombinedParser) = map(MatchedSubSequence,x)

using InternedStrings
intern(v::AbstractString) =
    InternedStrings.intern(v)::String

intern(v::Nothing) = ""
(!)(x::Transformation{MatchedSubSequence}) = map(intern, x)
map(::typeof(intern), x::Transformation{typeof(intern)}) = x

"""
    MatchedSubSequence(x)
    (!)(x::CombinedParser)
    (!)(x::CombinedParser{<:Any,<:AbstractString})

Parser [`Base.map`](@ref) transformation getting 
- either the matched `SubString` 
- or an `InternedStrings.intern`ed copy thereof iif `result_type<:AbstractString` already.
Transformation does not evaluate `get(parser.transform,...)`.

```jldoctest
julia> Repeat(AnyChar())
.* AnyValue |> Repeat
::Vector{Char}

julia> !Repeat(AnyChar())
.* AnyValue |> Repeat |> !
::SubString{String}

julia> !!Repeat(AnyChar())
.* AnyValue |> Repeat |> ! |> map(intern)
::String

```

!!! warn
    MatchedSubSequence currently only support `SubString`, extension to Vector views requires minor effort.
    - `map(MatchedSubSequence(), SubArray{Int,1}, parser)` constructor is required (currently no sequence type is known at construction).
    - `get` function currently missing

"""
(!), MatchedSubSequence



"""
    map_constant(constant, p::CombinedParser)
    parser((p,constant)::Pair)

Construct a [`Base.map`](@ref) `Transformation{<:Constant}` resulting in `p` when calling [`get`](@ref) fast,
instead of computing result from state, 
if `parser(p)` matches.

```jldocs
julia> parser("constant" | "fixed" => :constant)
|🗄 Either => :constant
├─ constant 
└─ fixed 
::Symbol
```

!!! note
    If the `Pair` key is a symbol, a [`NamedParser`](@ref) is created.
    ```jldocs
    julia> parser(:constant => "constant" | "fixed")
    |🗄 Either |> with_name(:constant)
    ├─ constant 
    └─ fixed 
    ::SubString{String}
    ```
"""
struct Constant{T}
    value::T
end
Base.show(io::IO, x::Constant) = show(io,x.value)


function map_constant(transform, p::CombinedParser)
    Transformation(Constant(transform), p)
end

result_type(p::Transformation{<:Constant}, sequence) =
    typeof(p.transform.value)

parser(constant::Pair) =
    map_constant(constant.second, parser(constant.first))

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

export MatchRange
"""
    MatchRange(p::CombinedParser)

Construct a [`Base.map`](@ref)`Transformation{UnitRange{Int}}` resulting in `p` when calling [`get`](@ref) fast,
Succeed iif `p` succeeds, if so results in sequence match index `UnitRange`.
Transformation does not evaluate `get(parser.transform,...)`.
"""
struct MatchRange
end
MatchRange(p::CombinedParser) =
    Transformation{UnitRange{Int}}(MatchRange(), p)

Base.show(io::IO, x::MatchRange) = print(io,"@")

function Base.get(parser::Transformation{MatchRange}, sequence, till, after, i, state)
    i:_prevind(sequence,after)
end

export IndexAt
"""
Struct for fast access to an index of a `Transformation`.

```jldoctest
julia> using CombinedParsers.Regexp

julia> p = re"(?:a|b*)."[1]
🗄 Sequence[1]
├─ |🗄 Either
│  ├─ a 
│  └─ b*  |> Repeat
└─ [^\\n] ValueNotIn
::Union{Char, Vector{Char}}
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
function Base.get(parser::Transformation{<:IndexAt{<:Integer}}, sequence, till, after, i, state)
    v = get(parser.parser,sequence, till, after, i, state)
    v[parser.transform.i]
end
function Base.get(parser::Transformation{IndexAt{Is}}, sequence, till, after, i, state) where {Is <: Union{Tuple, Vector, UnitRange}}
    tuple(get(parser.parser,sequence, till, after, i, state)[parser.transform.i]...)
end

result_type(p::Transformation{<:IndexAt{<:Integer}}, sequence) =
    fieldtypes(result_type(p.parser, sequence))[p.transform.i]

"""
    map(index::IndexAt, p::CombinedParser, a...)
    map(constant, p::CombinedParser, a...)

Parser matching `p`, transforming `p`s parsing results to `getindex(x,index)` or `constant`.

See also: [`get`](@ref), [`deepmap`](@ref)

"""
function Base.map(index::IndexAt{<:Integer}, p::CombinedParser)
    Transformation(index, p)
end
function Base.map(index::IndexAt{<:UnitRange}, p::CombinedParser)
    Transformation(index, p)
end

"""
    map(f::Function, p::CombinedParser, a...)

Parser matching `p`, transforming parsing results (`x`) with function `f(x,a...)`.

See also: [`get`](@ref), [`deepmap`](@ref)
"""
function Base.map(f::Function, p::CombinedParser, a...; kw...)
    Transformation(isempty(a) ? f : v -> f(v, a...; kw...), p)
end




function Base.map(f::Function, Tc::Type, p::CombinedParser, a...)
    Transformation(Tc, 
                   Transformation(isempty(a) ? f : v -> f(v, a...), p))
end

"""
    map(T::Type, p::CombinedParser, a...)

Parser matching `p`, transforming `p`s parsing result with constructor `T(x,a...)`.

See also: [`get`](@ref), [`deepmap`](@ref)
"""
function Base.map(Tc::Type, p::CombinedParser, a...)
    Transformation(isempty(a) ? Tc : v -> Tc(a..., v), p)
end

function instance(Tc::Type, p::CombinedParser, a...)
    Transformation((v) -> Tc(a..., v), p)
end

function instance(Tc::Type, p::CombinedParser)
    Transformation(Tc, p)
end

function Base.map(inner::CombinedParser, p::CombinedParser)
    Transformation(s -> parse(inner,s), p)
end

Base.map(f::typeof(identity), p::CombinedParser) = p

@deprecate map(T::Type, f::Function, p::CombinedParser, a...) map(f,T,p,a...)
@deprecate instance(f::Function,p,a...) map(f,parser(p),a...)

infer_result_type(f::Function,Tc::Type,p::CombinedParser, sequence,onerror::AbstractString,ts::Type...; kw...) =
    infer_result_type(f,Tc,p, typeof(sequence),onerror,ts...; kw...)

"""
    infer_result_type(f::Function,Tc::Type,p::CombinedParser,onerror::AbstractString,ts::Type...; throw_empty_union=true)

Used by Parser Transformations to infer result type of a parser.
Throws error if type inference fails, if throw_empty_union=true.
"""
function infer_result_type(f::Function,Tc::Type,p::CombinedParser, sequence::Type,onerror::AbstractString,ts::Type...; throw_empty_union=true)
    Ts = Base.return_types(f, tuple(result_type(p,sequence),ts...))
    isempty(Ts) && error("transformation type signature mismatch $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    ( length(Ts) > 1 || Any <: first(Ts) ) && return Tc ##error(onerror*"  $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    T = first(Ts)
    if throw_empty_union && T <: Union{}
        println(p)
        error("transformation type signature mismatch $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    elseif T <: Tc
        T
    else
        @warn "type mismatch $f$(tuple(result_type(p),ts...))::$T<:$Tc"
        Tc
    end
end

export deepmap
"""
    deepmap(f, parser, predicate, a...; kw...)

Substitute all `sub_parser`s with [`Base.map`](@ref)`(f,sub_parser, a...; kw...)` iif 
`dodeepmap(parser, predicate)`;
otherwise keep `sub_parser`


```jldocs
julia> p = re"(a+)b+"
🗄 Sequence |> regular expression combinator with 1 capturing groups
├─ (a+)  |> Repeat |> Capture 1
└─ b+  |> Repeat
::Tuple{Vector{Char}, Vector{Char}}

julia> p("aaabb")
(['a', 'a', 'a'], ['b', 'b'])

julia> deepmap(MatchedSubSequence, p, Capture)("aaabb")
("aaa", ['b', 'b'])

julia> deepmap(length, p, re"b+")("abb")
(['a'], 2)
```

Implementation is an example when the a custom leaf [`_deepmap`](@ref) method is useful and sufficient for [`deepmap_parser`](@ref).
"""
deepmap(f, parser, predicate, a...; kw...) = 
    deepmap_parser(_deepmap, parser, predicate, f, a...; kw...)

"""
    _deepmap(parser, predicate, f, a...; kw...)

Implementation example when the a custom leaf [`_deepmap`](@ref) method is useful and sufficient for [`deepmap_parser`](@ref).
```julia
if dodeepmap(parser, predicate)
    map(f,parser, a...; kw...)
else
    parser
end
```
"""
function _deepmap(parser, predicate, f, a...; kw...)
    if dodeepmap(parser, predicate)
        map(f,parser, a...; kw...)
    else
        parser
    end
end

"""
    dodeepmap(parser, predicate)

`parser == predicate`.
Specialize for custom predicate type.

    dodeepmap(parser, predicate::Type)

`sub_parser isa predicate`

    dodeepmap(parser, predicate::Function)

`predicate(sub_parser)`

    dodeepmap(parser::NamedParser, predicate::Symbol)

`sub_parser.name == predicate`
"""
dodeepmap(parser, predicate) = parser == predicate
dodeepmap(parser, predicate::Function) = predicate(parser)
dodeepmap(parser, predicate::Type) = parser isa predicate
dodeepmap(parser::NamedParser, predicate::Symbol) = parser.name == predicate

export reinfer
"""
    reinfer(parser)

Run julia type inference again on a parser for optimization.
[`Either`](@ref)`{<:Vector}` parsers are converted to `Either{<:Tuple}`.

Implementation is an example when the a custom [`deepmap_parser`](@ref) method is useful.
"""
reinfer(parser) = deepmap_parser(_reinfer, parser)
_reinfer(parser) = parser
function deepmap_parser(::typeof(_reinfer),mem::AbstractDict,x::Either{<:Vector},a...;kw...)
    if haskey(mem,x)
        mem[x]
    else
        mem[x] = r = Either{result_type(x)}(Any[])
        for p in x.options
            push!(r,deepmap_parser(_reinfer,mem,p,a...;kw...))
        end
        mem[x] = Either(r.options...)
    end
end

