include("unicode.jl")

"""
`ValueMatcher` match value at point `c` iif [`ismatch`](@ref)`(c, parser)`.
A `ValueMatcher{T}=NIndexParser{1,T}` and has `state_type` `MatchState`.

See [`AnyChar`](@ref), [`CharIn`](@ref), and [`CharNotIn`](@ref).
"""
abstract type ValueMatcher{T} <: NIndexParser{1,T} end

export AnyChar, any
"""
    AnyChar(T=Char)

Parser matching exactly one `x::T`, returning the value.
```jldoctest
julia> AnyChar()
. AnyChar
::Char
```

"""
struct AnyChar{T} <: ValueMatcher{T} end

AnyChar(T=Char) = AnyChar{T}()
#@deprecate AnyChar() AnyValue(Char)
regex_inner(x::AnyChar{Char}) = "."
regex_inner(x::AnyChar{T}) where T = "(.::$T)"

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
    _ismatch(c,p::AnyChar)

`true`
"""
_ismatch(c,p::AnyChar)::Bool = true
"""
    _ismatch(c,p::Union{StepRange,Set})

returns `c in p`
"""
_ismatch(c,p::Union{StepRange,Set})::Bool = c in p




export CharIn
"""
    CharIn(x)

Parser matching exactly one element `c` (character) in a sequence, iif [`_ismatch`](@ref)`(c,x)`.

```jldoctest
julia> a_z = CharIn('a':'z')
[a-z] CharIn
::Char

julia> parse(a_z, "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> ac = CharIn("ac")
[ac] CharIn
::Char

julia> parse(ac, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

julia> l = CharIn(islowercase)
[islowercase(.)] CharIn
::Char

julia> parse(l, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

```
"""
@auto_hash_equals struct CharIn{S,T} <: ValueMatcher{T}
    pcre::String
    sets::S
    function CharIn(pcre::String,x_) # <:Union{Char,Set{Char},<:Function,<:UnicodeClass,<:Tuple}}=
        x = flatten_valuepatterns(CharIn, x_)
        new{typeof(x),eltype(x)}(pcre,x)
    end
end
@inline _ismatch(c,p::CharIn)::Bool = _ismatch(c,p.sets)

CharIn(pcre::String,x::CharIn) =
    CharIn(pcre,x.sets)
CharIn(pcre::String,x::ConstantParser{Char}) =
    CharIn(pcre,x.parser)
CharIn(pcre::String,x::AbstractString) =
    isempty(x) ? Never() : CharIn(pcre,x...)
CharIn(chars::String) =
    CharIn(regex_escape(chars),chars)
CharIn(pcre::String,x_...) =
    CharIn(pcre,x_)
CharIn(x_...) =
    CharIn("",x_...)
CharIn(chars::StepRange) =
    CharIn("$(chars.start)-$(chars.stop)",chars)

regex_string_(x) = "$x"

"""
    CharIn(unicode_category::Symbol...)

succeeds if char at cursor is in one of the unicode classes.

```jldoctest
julia> match(CharIn(:L), "aB")
ParseMatch("a")

julia> match(CharIn(:Lu), "aB")
ParseMatch("B")

julia> match(CharIn(:N), "aA1")
ParseMatch("1")
```

Respects boolean logic:
```jldoctest
julia> parse(CharIn(CharIn("ab")),     "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> parse(CharIn(CharNotIn("bc")),  "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> parse(CharNotIn(CharIn("bc")),  "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

See also [`CombinedParsers.unicode_classes`](@ref).
"""
CharIn(unicode_classes::Symbol...) =
    CharIn(UnicodeClass(unicode_classes...))


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
regex_string_(x::AbstractString) = regex_escape(x)

result_type(::Type{<:CharIn}) = Char
regex_string_(x::Union{Vector,Set}) = join(regex_string_.(x))
regex_string(x::Char) = regex_escape("$x") ##x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string_(x::Char) = regex_escape("$x") ##x == '\\' ? "\\\\" : "$x" ## for [] char ranges
regex_string_(x::StepRange) =
    if x.start == x.stop
        x.start
    else
        x.start*"-"*x.stop
    end
regex_string_(x::Tuple) = join([regex_string_(s) for s in x])
regex_string_(x::Function) = "$x(...)"
regex_string_(x::CharIn) = ( x.pcre =="" ? regex_string_(x.sets) : x.pcre )
regex_inner(x::CharIn) =
    "["*regex_string_(x)*"]"

print_constructor(io::IO,x::CharIn{Char}) = nothing
regex_inner(x::CharIn{Char}) =
    regex_string_(x.sets)



export CharNotIn
"""
    CharNotIn(x)

Parser matching exactly one element (character) in a sequence, iif not in `x`.

```jldoctest
julia> a_z = CharNotIn('a':'z')
[^a-z] CharNotIn
::Char

julia> ac = CharNotIn("ac")
[^ac] CharNotIn
::Char

```

Respects boolean logic:
```jldoctest
julia> p = CharNotIn(CharNotIn("ab"));

julia> parse(p,"a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

"""
@auto_hash_equals struct CharNotIn{S,T} <: ValueMatcher{T}
    pcre::String
    sets::S
    function CharNotIn(pcre::String, x_) # where {T<:Union{Char,Set{Char},Function,UnicodeClass,Tuple}}=
        x = flatten_valuepatterns(CharNotIn, x_)
        new{typeof(x),eltype(x)}(pcre,x)
    end
end
CharNotIn(pcre::String,x...) =
    CharNotIn(pcre,x)
CharNotIn(pcre::String,x::ConstantParser{Char}) =
    CharNotIn(pcre,x.parser)
CharNotIn(chars::String) =
    CharNotIn("^"*regex_escape(chars),chars)
CharNotIn(chars::StepRange) =
    CharNotIn("^$(chars.start)-$(chars.stop)",chars)
CharNotIn(x_...) =
    CharNotIn("",x_...)

result_type(::Type{<:CharNotIn}) = Char
regex_string_(x::CharNotIn) = ( x.pcre =="" ? "^"*regex_string_(x.sets) : x.pcre )
regex_inner(x::CharNotIn) =
    "["*regex_string_(x)*"]"
@inline _ismatch(c,p::CharNotIn)::Bool = !_ismatch(c,p.sets)

CharIn(x::Tuple{<:CharNotIn}) = x[1]

"""
    CharIn(unicode_classes::Symbol...)

succeeds if char at cursor is not in any of the `unicode_classes`.
"""
CharNotIn(unicode_classes::Symbol...) =
    CharNotIn(UnicodeClass(unicode_classes...))

# @inline function _iterate(parser::CharNotIn, sequence, till, posi, next_i, state::Nothing)
#     next_i>till && return(nothing)
#     @inbounds c,ni = iterate(sequence,next_i)
#     ismatch(c,parser.sets) && return nothing 
#     return ni, MatchState()
# end

# @inline function _iterate(parser::CharIn, sequence, till, posi, next_i, state::Nothing)
#     next_i>till && return nothing
#     @inbounds c,ni = sequence[next_i], _nextind(sequence,next_i)
#     !ismatch(c,parser.sets) && return nothing
#     return ni, MatchState()
# end


# export CharMatcher
# BaseCharMatcher = Union{Char, AnyChar, UnicodeClass, StepRange{Char,Int}}
# CharMatcher = Union{CharIn, CharNotIn, BaseCharMatcher}


_push!(charset::Nothing, x) = x
_push!(charset::T, x::T) where T = Set{T}(tuple(charset,x))
_push!(charset::Set, x) where T = push!(charset,x)
_push!(charset::Vector, x) where T = push!(charset,x)
_push!(charset::Nothing, x::Union{<:Function,<:UnicodeClass,<:CharNotIn}) = Any[x]

flatten_valuepatterns!(charset,otherstuff) =
    charset,otherstuff

flatten_valuepatterns!(charset,otherstuff,x) =
    _push!(charset,x),otherstuff

flatten_valuepatterns!(charset,otherstuff,x::ConstantParser{Char}) =
    flatten_valuepatterns!(charset,otherstuff, x.parser)

# e.g. CharIn{UnicodeClass}
flatten_valuepatterns!(charset,otherstuff,c::CharIn) = 
    flatten_valuepatterns!(charset,otherstuff, c.sets)

flatten_valuepatterns!(charset,otherstuff,x::Union{<:Function,<:UnicodeClass,<:CharNotIn}) =
    charset, _push!(otherstuff,x)

ElementIterators = Union{<:Vector,<:Tuple,<:StepRange,<:Set,<:AbstractString}
flatten_valuepatterns!(charset,otherstuff,x::ElementIterators) =
    flatten_valuepatterns!(charset,otherstuff,x...)

function flatten_valuepatterns!(charset,otherstuff,x1,x2,x...)
    flatten_valuepatterns!(flatten_valuepatterns!(charset,otherstuff,x1)...,x2,x...)
end

function flatten_valuepatterns(::Type{<:Union{CharIn,CharNotIn}},x...)
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

