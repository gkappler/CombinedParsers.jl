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
re"."
```

"""
struct AnyChar{T} <: ValueMatcher{T} end

AnyChar(T=Char) = AnyChar{T}()
#@deprecate AnyChar() AnyValue(Char)
regex_inner(x::AnyChar{Char}) = "."
regex_inner(x::AnyChar{T}) where T = "(.::$T)"

"""
    _iterate(parser::AnyChar, sequence, till, posi, next_i, state::Nothing)

Matches value at point `c` iif [`ismatch`](@ref)`(c, parser)` with state MatchState.
"""
@inline function _iterate(parser::ValueMatcher, sequence, till, posi, next_i, state::Nothing)
    next_i>till && return nothing
    @inbounds c,ni = sequence[next_i], _nextind(sequence, next_i)
    !ismatch(c,parser) && return nothing
    return ni, MatchState()
end

"""
    _ismatch(x::Char, set::Union{Tuple,Vector})::Bool

`_ismatch(x,set...)` respects boolean logic:

Example:
```jldoctest
julia> p = CharNotIn(CharNotIn("ab"));

julia> parse(p,"a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

```
"""
function _ismatch(x, set::Union{Tuple,Vector})::Bool
    return _ismatch(x,set...)
end

function _ismatch(x)::Bool
    return false
end

function _ismatch(x, f, r1, r...)::Bool
    ismatch(x,f) && return true
    return _ismatch(x::Char, r1, r...)
end

function _ismatch(c,p)::Bool
    c==p
end
function ismatch(c,p)::Bool
    _ismatch(c, p)
end
_ismatch(c,p::Function)::Bool = p(c)::Bool
_ismatch(c,p::AnyChar)::Bool = true
_ismatch(c,p::Union{StepRange,Set})::Bool = c in p




export CharIn
"""
    CharIn(x)

Parser matching exactly one element (character) in a sequence, iif in `x`.

```jldoctest
julia> a_z = CharIn('a':'z')
re"[a-z]"

julia> parse(a_z, "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> ac = CharIn("ac")
re"[ac]"


julia> parse(ac, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

```
"""
@auto_hash_equals struct CharIn{S,T} <: ValueMatcher{T}
    pcre::String
    sets::S
    function CharIn(pcre::String,x_) # <:Union{Char,Set{Char},<:Function,<:UnicodeClass,<:Tuple}}=
        x = optimize(CharIn, x_)
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
re"[^a-z]"

julia> ac = CharNotIn("ac")
re"[^ac]"

```
"""
@auto_hash_equals struct CharNotIn{S,T} <: ValueMatcher{T}
    pcre::String
    sets::S
    function CharNotIn(pcre::String, x_) # where {T<:Union{Char,Set{Char},Function,UnicodeClass,Tuple}}=
        x = optimize(CharNotIn, x_)
        new{typeof(x),eltype(x)}(pcre,x)
    end
end
CharNotIn(pcre::String,x...) =
    CharNotIn(pcre,x)
CharNotIn(pcre::String,x::ConstantParser{Char}) =
    CharNotIn(pcre,x.parser)
CharNotIn(chars::String) =
    CharNotIn(regex_escape(chars),chars)
CharNotIn(chars::StepRange) =
    CharNotIn("$(chars.start)-$(chars.stop)",chars)
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

optimize!(charset,otherstuff) =
    charset,otherstuff
optimize!(charset,otherstuff,x) =
    _push!(charset,x),otherstuff

optimize!(charset,otherstuff,x::ConstantParser{Char}) =
    optimize!(charset,otherstuff, x.parser)

optimize!(charset,otherstuff,x::Union{<:Function,<:UnicodeClass,<:CharNotIn}) =
    charset, _push!(otherstuff,x)

optimize!(charset,otherstuff,c::CharIn) = 
    optimize!(charset,otherstuff, c.sets...)

ElementIterators = Union{Vector,Tuple,StepRange,Set,AbstractString}
optimize!(charset,otherstuff,x::ElementIterators) =
    optimize!(charset,otherstuff,x...)

function optimize!(charset,otherstuff,x1,x2,x...)
    optimize!(optimize!(charset,otherstuff,x1)...,x2,x...)
end

function optimize(::Type{<:Union{CharIn,CharNotIn}},x...)
    charset,otherstuff = optimize!(nothing,nothing,x...)
    if otherstuff===nothing
        charset === nothing ? tuple() : charset
    elseif charset===nothing
        tuple(otherstuff...)
    else
        tuple(charset,otherstuff...)
    end
end

