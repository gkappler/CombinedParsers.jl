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
    getsplat(parser.transform, v)
end

function getsplat(transform::Type, v)
    if isbitstype(transform)
        reinterpret(transform,v)[1]
    else
        v isa transform ? v : transform(v)
        #T(sequence[i:after-1])
    end
end

function getsplat(transform::Type, v::Tuple)
    if isbitstype(transform)
        reinterpret(transform,v)[1]
    else
        v isa transform ? v : transform(v...)
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
    AbstractString

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
Base.getindex(x::CombinedParser, i) = map(IndexAt(i),x)

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


"""
    infer_result_type(f::Function,Tc::Type,p::CombinedParser,onerror::AbstractString,ts::Type...; throw_empty_union=true)

Used by Parser Transformations to infer result type of a parser.
Throws error if type inference fails, if throw_empty_union=true.
"""
function infer_result_type(f, Tc::Type, p::CombinedParser, sequence, onerror::AbstractString, ts::Type...; throw_empty_union=true)
    arg_types = tuple(result_type(p, sequence), ts...)
    Ts = Base.return_types(f, arg_types)

    if isempty(Ts)
        bt = stacktrace(backtrace())
        @error "Transformation method not found or signature mismatch. No methods exist for the transformation function with the given argument types." f=f attempted_arguments=arg_types parser_context=p target_return_type=Tc
        Base.show_backtrace(stdout, bt)
        return Any
    end

    if length(Ts) > 1 || Any <: first(Ts)
        # Multiple possible return types or a very general return type, inference is not precise.
        # bt = stacktrace(backtrace())
        #@warn "Ambiguous or overly broad return type inference for transformation. Multiple methods matched or the inferred type is too general. Falling back to the target type." f=f attempted_arguments=arg_types inferred_possible_types=Ts target_return_type=Tc parser_context=p
        #Base.show_backtrace(stdout, bt)
        Tc <: AbstractString ? AbstractString : Tc
    end

    T = first(Ts)
    if throw_empty_union && T <: Union{}
        bt = stacktrace(backtrace())[3:end]
        @warn "Transformation function infers an empty Union `($T)`. This indicates that the function `$f` with arguments `$arg_types` has no successful return path (e.g., always throws an error or contains unreachable code)." f=f attempted_arguments=arg_types inferred_type=T parser_context=p target_return_type=Tc
        Base.show_backtrace(stdout, bt)
        Any # Keep existing behavior of returning Any
    elseif T <: Tc
        T <: AbstractString ? AbstractString : T
    else
        bt = stacktrace(backtrace())
        @warn "Transformation inferred return type mismatch. The inferred type `$T` for `$f` with arguments `$arg_types` is not a subtype of the target return type `$Tc`. Falling back to target type."  f=f attempted_arguments=arg_types inferred_type=T target_return_type=Tc parser_context=p
        Base.show_backtrace(stdout, bt)
        Tc <: AbstractString ? AbstractString : Tc
    end
end
