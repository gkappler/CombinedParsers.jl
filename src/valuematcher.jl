include("unicode.jl")

"""
`ValueMatcher` match value at point `c` iif [`ismatch`](@ref)`(c, parser)`.
A `ValueMatcher{T}=NIndexParser{1,T}` and has `state_type` `MatchState`.

See [`AnyValue`](@ref), [`ValueIn`](@ref), and [`ValueNotIn`](@ref).
"""
abstract type ValueMatcher{T} <: NIndexParser{1,T} end

export AnyValue, AnyChar
"""
    AnyValue(T=Char)

Parser matching exactly one `x::T`, returning the value.
```jldoctest
julia> AnyChar()
. AnyValue
::Char
```

"""
struct AnyValue{T} <: ValueMatcher{T} end

AnyValue(T::Type) = AnyValue{T}()
AnyChar() = AnyValue(Char)
#@deprecate AnyChar() AnyValue(Char)
regex_inner(x::AnyValue{Char}) = "."
regex_inner(x::AnyValue{T}) where T = "(.::$T)"

"""
    _iterate(parser::ValueMatcher, sequence, till, posi, next_i, state::Nothing)

When implementing a `Custom<:ValueMatcher` it suffices to provide a method [`CombinedParsers._ismatch`](@ref)`(c, parser::Custom)`.
"""
@inline function _iterate(parser::ValueMatcher, sequence, till, posi, next_i, state::Nothing)
    next_i>till && return nothing
    @inbounds c,ni = sequence[next_i], _nextind(sequence, next_i)
    !ismatch(c,parser) && return nothing
    return ni, MatchState()
end

"""
    _ismatch(x::Char, set::Union{Tuple,Vector})::Bool

Return `_ismatch(x,set...)`.
"""
function _ismatch(x, set::Union{Tuple,Vector})::Bool
    return _ismatch(x,set...)
end

"""
    _ismatch(x, f, r1, r...)

Check if `x` matches any of the options `f, r1,r...`:
If `ismatch(x,f)` return `true`,
otherwise return `_ismatch(x, r1, r...)`.
"""
function _ismatch(x, f, r1, r...)::Bool
    ismatch(x,f) && return true
    return _ismatch(x, r1, r...)
end

"""
    _ismatch(x)

returns `false` (out of options)
"""
function _ismatch(x)::Bool
    return false
end

"""
    _ismatch(x, p)

returns `x==p`
"""
function _ismatch(c,p)::Bool
    c==p
end

"""
    ismatch(c,p)

returns [`_ismatch`](@ref)`(c, p)`
"""
function ismatch(c,p)::Bool
    _ismatch(c, p)
end

"""
    _ismatch(c,p::Function)

returns `p(c)`
"""
_ismatch(c,p::Function)::Bool = p(c)::Bool
"""
    _ismatch(c,p::AnyValue)

`true`
"""
_ismatch(c,p::AnyValue)::Bool = true
"""
    _ismatch(c,p::Union{StepRange,Set})

returns `c in p`
"""
_ismatch(c,p::Union{StepRange,Set})::Bool = c in p




export ValueIn
"""
    ValueIn(x)

Parser matching exactly one element `c` (character) in a sequence, iif [`_ismatch`](@ref)`(c,x)`.

```jldoctest
julia> a_z = ValueIn('a':'z')
[a-z] ValueIn
::Char

julia> parse(a_z, "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> ac = ValueIn("ac")
[ac] ValueIn
::Char

julia> parse(ac, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

julia> l = ValueIn(islowercase)
[islowercase(...)] ValueIn
::Char

julia> parse(l, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

```
"""
@auto_hash_equals struct ValueIn{S,T} <: ValueMatcher{T}
    pcre::String
    sets::S
    function ValueIn(pcre::String,x_) # <:Union{Char,Set{Char},<:Function,<:UnicodeClass,<:Tuple}}=
        x = flatten_valuepatterns(ValueIn, x_)
        new{typeof(x),eltype(x)}(pcre,x)
    end
end
@inline _ismatch(c,p::ValueIn)::Bool = _ismatch(c,p.sets)

ValueIn(pcre::String,x::ValueIn) =
    ValueIn(pcre,x.sets)
ValueIn(pcre::String,x::ConstantParser{Char}) =
    ValueIn(pcre,x.parser)
ValueIn(pcre::String,x::AbstractString) =
    isempty(x) ? Never() : ValueIn(pcre,x...)
ValueIn(chars::String) =
    ValueIn(regex_escape(chars),chars)
ValueIn(pcre::String,x_...) =
    ValueIn(pcre,x_)
ValueIn(x_...) =
    ValueIn("",x_...)
ValueIn(chars::StepRange) =
    ValueIn("$(chars.start)-$(chars.stop)",chars)

_regex_string(x) = "$x"

"""
    ValueIn(unicode_category::Symbol...)

succeeds if char at cursor is in one of the unicode classes.

```jldoctest
julia> match(ValueIn(:L), "aB")
ParseMatch("a")

julia> match(ValueIn(:Lu), "aB")
ParseMatch("B")

julia> match(ValueIn(:N), "aA1")
ParseMatch("1")
```

Respects boolean logic:
```jldoctest
julia> parse(ValueIn(ValueIn("ab")),     "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> parse(ValueIn(ValueNotIn("bc")),  "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> parse(ValueNotIn(ValueIn("bc")),  "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

See also [`CombinedParsers.unicode_classes`](@ref).
"""
ValueIn(unicode_classes::Symbol...) =
    ValueIn(UnicodeClass(unicode_classes...))


export regex_escape
## https://github.com/JuliaLang/julia/pull/29643/commits/dfb865385edf19b681bc0936028af23b1f282b1d
"""
        regex_escape(s::AbstractString)

regular expression metacharacters are escaped along with whitespace.
"""
function regex_escape(s)
    res = replace(escape_string(string(s)), r"([()[\]{}?*+\-|^\$.&~#\s=!<>|:])" => s"\\\1")
    replace(res, "\0" => "\\0")
end
export regex_string
regex_string(x::AbstractString) = regex_escape(x)
_regex_string(x::AbstractString) = regex_escape(x)

result_type(::Type{ValueIn{<:Any,T}}) where T = T
_regex_string(x::Union{Vector,Set}) = join(_regex_string.(x),",")
_regex_string(x::Union{Vector{<:AbstractChar},Set{<:AbstractChar}}) = join(_regex_string.(x))
regex_string(x::Char) = regex_escape("$x") ##x == '\\' ? "\\\\" : "$x" ## for [] char ranges
_regex_string(x::Char) = regex_escape("$x") ##x == '\\' ? "\\\\" : "$x" ## for [] char ranges
_regex_string(x::StepRange) =
    if x.start == x.stop
        x.start
    else
        x.start*"-"*x.stop
    end
_regex_string(x::Tuple) = join([_regex_string(s) for s in x])
_regex_string(x::Function) = "$x(...)"
_regex_string(x::ValueIn) = ( x.pcre =="" ? _regex_string(x.sets) : x.pcre )
regex_inner(x::ValueIn) =
    "["*_regex_string(x)*"]"

print_constructor(io::IO,x::ValueIn{Char}) = nothing
regex_inner(x::ValueIn{Char}) =
    _regex_string(x.sets)



export ValueNotIn
"""
    ValueNotIn(x)

Parser matching exactly one element (character) in a sequence, iif not in `x`.

```jldoctest
julia> a_z = ValueNotIn('a':'z')
[^a-z] ValueNotIn
::Char

julia> ac = ValueNotIn("ac")
[^ac] ValueNotIn
::Char

```

Respects boolean logic:
```jldoctest
julia> p = ValueNotIn(ValueNotIn("ab"));

julia> parse(p,"a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

"""
@auto_hash_equals struct ValueNotIn{S,T} <: ValueMatcher{T}
    pcre::String
    sets::S
    function ValueNotIn(pcre::String, x_) # where {T<:Union{Char,Set{Char},Function,UnicodeClass,Tuple}}=
        x = flatten_valuepatterns(ValueNotIn, x_)
        new{typeof(x),eltype(x)}(pcre,x)
    end
end
ValueNotIn(pcre::String,x...) =
    ValueNotIn(pcre,x)
ValueNotIn(pcre::String,x::ConstantParser{Char}) =
    ValueNotIn(pcre,x.parser)
ValueNotIn(chars::String) =
    ValueNotIn(regex_escape(chars),chars)
ValueNotIn(chars::StepRange) =
    ValueNotIn("$(chars.start)-$(chars.stop)",chars)
ValueNotIn(x_...) =
    ValueNotIn("",x_...)

result_type(::Type{ValueNotIn{<:Any,T}}) where T = T
_regex_string(x::ValueNotIn) = "^"*( x.pcre =="" ? _regex_string(x.sets) : x.pcre )
regex_inner(x::ValueNotIn) = "["*_regex_string(x)*"]"
@inline _ismatch(c,p::ValueNotIn)::Bool = !_ismatch(c,p.sets)

ValueIn(x::Tuple{<:ValueNotIn}) = x[1]

"""
    ValueIn(unicode_classes::Symbol...)

succeeds if char at cursor is not in any of the `unicode_classes`.
"""
ValueNotIn(unicode_classes::Symbol...) =
    ValueNotIn(UnicodeClass(unicode_classes...))






_push!(charset::Nothing, x) = x
_push!(charset::T, x::T) where T = Set{T}(tuple(charset,x))
_push!(charset::Set, x) where T = push!(charset,x)
_push!(charset::Vector, x) where T = push!(charset,x)
_push!(charset::Nothing, x::Union{<:Function,<:UnicodeClass,<:ValueNotIn}) = Any[x]

flatten_valuepatterns!(charset,otherstuff) =
    charset,otherstuff

flatten_valuepatterns!(charset,otherstuff,x) =
    _push!(charset,x),otherstuff

flatten_valuepatterns!(charset,otherstuff,x::ConstantParser{Char}) =
    flatten_valuepatterns!(charset,otherstuff, x.parser)

# e.g. ValueIn{UnicodeClass}
flatten_valuepatterns!(charset,otherstuff,c::ValueIn) = 
    flatten_valuepatterns!(charset,otherstuff, c.sets)

flatten_valuepatterns!(charset,otherstuff,x::Union{<:Function,<:UnicodeClass,<:ValueNotIn}) =
    charset, _push!(otherstuff,x)

ElementIterators = Union{<:Vector,<:Tuple,<:StepRange,<:Set,<:AbstractString}
flatten_valuepatterns!(charset,otherstuff,x::ElementIterators) =
    flatten_valuepatterns!(charset,otherstuff,x...)

function flatten_valuepatterns!(charset,otherstuff,x1,x2,x...)
    flatten_valuepatterns!(flatten_valuepatterns!(charset,otherstuff,x1)...,x2,x...)
end

function flatten_valuepatterns(::Type{<:Union{ValueIn,ValueNotIn}},x...)
    charset,otherstuff = flatten_valuepatterns!(nothing,nothing,x...)
    if otherstuff===nothing
        charset === nothing ? tuple() : charset
    elseif charset===nothing
        if length(otherstuff) == 1
            otherstuff[1]
        else
            tuple(otherstuff...)
        end
    elseif isempty(otherstuff)
        charset
    else
        tuple(charset,otherstuff...)
    end
end

@deprecate CharIn(a...; kw...) ValueIn(a...; kw...)
@deprecate CharNotIn(a...; kw...) ValueNotIn(a...; kw...)
