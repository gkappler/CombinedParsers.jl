import Base: (^), (*), (~), (/), (|), (!)

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
    (/)(x::ParserTypes, y::ParserTypes)

`Sequence(PositiveLookbehind(x),y)`

```jldoctest
julia> match("is "/"match", "no match is match").start
13
```

!!! note
    This syntax is reviewed and I your appreciate your comments!
"""
(/)(x::ParserTypes, y::ParserTypes) =
    Sequence(PositiveLookbehind(x),y)


"""
    Base.broadcasted(::typeof((&)), x::CharNotIn, y::CharNotIn)

Character matchers `m` like `Union{CharIn,CharNotIn,T}`, or any 
type `T` providing a `ismatch(m::T,c::Char)::Bool` method represent a 
lazy bitarray for all characters.
"""
Base.broadcasted(::typeof((&)), x::CharNotIn, y::CharNotIn) =
    CharNotIn(x.pcre*y.pcre, x.sets,y.sets)

Base.broadcasted(::typeof((&)), x::CharIn, y::CharNotIn) =
    CharIn(setdiff(x.sets, y.sets))

Base.broadcasted(::typeof((&)), x::Union{CharIn,CharNotIn}, ::AnyChar) =
    x


Base.broadcasted(::typeof((&)), x::NamedParser, y) =
    NamedParser(x.name,x.parser .& y; doc=x.doc)

Base.broadcasted(::typeof((&)), x::JoinSubstring, y) =
    JoinSubstring(x.parser .& y)

Base.broadcasted(::typeof((&)), x::Transformation, y) =
    Transformation(x.transform, x.parser .& y)

Base.broadcasted(::typeof((&)), x::Repeat, y) =
    Repeat(x.range, x.parser .& y)


Base.broadcasted(::typeof((|)), x::CharIn, y::CharIn) =
    CharIn(x.pcre*y.pcre, x.sets,y.sets)

"""
    (!)(x::AbstractToken)

Parser Transformation getting the matched SubString.

```jldoctest
julia> parse(Repeat(CharIn(:L)),"abc123")
3-element Array{Char,1}:
 'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
 'b': ASCII/Unicode U+0062 (category Ll: Letter, lowercase)
 'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

julia> parse(!Repeat(CharIn(:L)),"abc123")
"abc"

```

"""
(!)(x::AbstractToken) = JoinSubstring(x)
using InternedStrings
import InternedStrings: intern
"""
    (!)(x::JoinSubstring)

Parser transformating result `v -> InternedStrings.intern(v)`.
"""
(!)(x::AbstractToken{<:SubString}) =
    instance(String, x)
(!)(x::Transformation{<:SubString}) =
    map(InternedStrings.intern, x.parser)

"""
    (!)(x::NamedParser)

Parser transformating result `v -> v=>x.name`.
"""
(!)(x::NamedParser) = map(v -> v => x.name, x)






(|)(x, y::ParserTypes) = sEither(parser(x),y)
(|)(x::ParserTypes, y) = sEither(x,parser(y))
"""
    (|)(x::AbstractToken, y)
    (|)(x, y::AbstractToken)
    (|)(x::AbstractToken, y::AbstractToken)

Operator syntax for `sEither(x, y)`.

```jldoctest
julia> 'a' | CharIn("AB") | "bc"
|🗄... Either
├─ a
├─ [AB] CharIn
└─ bc
::Union{Char, SubString}

```

"""
(|)(x::ParserTypes, y::ParserTypes) = sEither(x,y)

"""
    (|)(x::AbstractToken{T}, default::Union{T,Missing})

Operator syntax for `Optional(x, default=default)`.

```jldoctest
julia> parser("abc") | "nothing"
|🗄... Either
├─ abc
└─ nothing
::SubString

```

"""
function (|)(x::AbstractToken{T}, default::Union{T,Missing}) where { T }
    Optional(x,default=default)
end
function (|)(x::Char, y::Char)
    CharIn(tuple(x,y))
end
function (|)(x::CharIn, y::Char)
    CharIn(tuple(x.sets...,y))
end


"""
    `(|)(x::Either, T::Type)`

Return new Either with `T` added to result_type(x).
todo: Note that the options array is kept. As a consequence `push!`on result will also push to `x`.
"""
(|)(x::Either, T::Type) =
    Either{Union{result_type(x),T}}(x.options)

