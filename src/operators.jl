import Base: (^), (*), (~), (/), (|)
ParserOperatorTypes = Union{AbstractToken, AbstractString, Char}

(*)(x::Any, y::AbstractToken) = sSequence(parser(x),y)
(*)(x::AbstractToken, y::Any) = sSequence(x,parser(y))
"""
    (*)(x::Any, y::AbstractToken)
    (*)(x::AbstractToken, y::Any)
    (*)(x::AbstractToken, y::AbstractToken)

Chain parsers in [`sSequence`](@ref).
See also [`@seq`](@ref).
"""
(*)(x::AbstractToken, y::AbstractToken) = sSequence(x,y)

## todo: cuts

"""
    (/)(x::ParserOperatorTypes, y::ParserOperatorTypes)

`Sequence(PositiveLookbehind(x),y)`

```jldoctest
julia> match("is "/"match", "no match is match").offset
13
```

!!! note
    This syntax is reviewed and I your appreciate your comments!
"""
(/)(x::ParserOperatorTypes, y::ParserOperatorTypes) =
    Sequence(PositiveLookbehind(x),y)


"""
    Base.broadcasted(::typeof((&)), x::ValueNotIn, y::ValueNotIn)

Character matchers `m` like `Union{ValueIn,ValueNotIn,T}`, or any 
type `T` providing a `ismatch(m::T,c::Char)::Bool` method represent a 
"sparse" bitarray for all characters.

!!! note
    Please consider the broadcast API a draft you are invited to comment to.

```jldoctest
julia> ValueNotIn("abc") .& ValueNotIn("z")

julia> ValueIn("abc") .& ValueNotIn("c")
```
"""
Base.broadcasted(::typeof((&)), x::ValueNotIn, y::ValueNotIn) =
    ValueNotIn(x.pcre*y.pcre, x.sets, y.sets)

Base.broadcasted(::typeof((&)), x::ValueIn, y::ValueNotIn) =
    ValueIn(setdiff(x.sets, y.sets))

Base.broadcasted(::typeof((&)), x::Union{ValueIn,ValueNotIn}, ::AnyValue) =
    x


Base.broadcasted(::typeof((&)), x::NamedParser, y) =
    NamedParser(x.name,x.parser .& y; doc=x.doc)

Base.broadcasted(::typeof((&)), x::JoinSubstring, y) =
    JoinSubstring(x.parser .& y)

Base.broadcasted(::typeof((&)), x::Transformation, y) =
    Transformation(x.transform, x.parser .& y)


Base.broadcasted(::typeof((|)), x::ValueIn, y::ValueIn) =
    ValueIn(x.pcre*y.pcre, x.sets,y.sets)



(|)(x, y::ParserOperatorTypes) = sEither(parser(x),y)
(|)(x::ParserOperatorTypes, y) = sEither(x,parser(y))
(|)(x::ParserOperatorTypes, y::ParserOperatorTypes) = sEither(x,y)

"""
    (|)(x::AbstractToken, y)
    (|)(x, y::AbstractToken)
    (|)(x::AbstractToken, y::AbstractToken)

Operator syntax for `sEither(x, y)`.

```jldoctest
julia> 'a' | ValueIn("AB") | "bc"
|🗄 Either
├─ a
├─ [AB] ValueIn
└─ bc
::Union{Char, SubString{String}}

```
"""
(|)(x::CombinedParser, y::CombinedParser)

"""
    (|)(x::AbstractToken{T}, default::Union{T,Missing})

Operator syntax for `Optional(x, default=default)`.

```jldoctest
julia> parser("abc") | "nothing"
|🗄 Either
├─ abc
└─ nothing
::SubString{String}

julia> parser("abc") | missing
abc? |missing
::Union{Missing, SubString{String}}

```

"""
function (|)(x::AbstractToken{T}, default::Union{T,Missing}) where { T }
    Optional(x,default=default)
end
function (|)(x::Char, y::Char)
    ValueIn(tuple(x,y))
end
function (|)(x::ValueIn, y::Char)
    ValueIn(tuple(x.sets...,y))
end


"""
    `(|)(x::Either, T::Type)`

Return new Either with `T` added to result_type(x).
todo: Note that the options array is kept. As a consequence `push!`on result will also push to `x`.
"""
(|)(x::Either, T::Type) =
    Either{Union{result_type(x),T}}(x.options)

