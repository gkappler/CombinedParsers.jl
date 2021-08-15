include("unicode.jl")

"""
`ValueMatcher` match value at point `c` iif [`ismatch`](@ref)`(c, parser)`.
A `ValueMatcher{T}=NIndexParser{1,T}` and has `state_type` `MatchState`.

See [`AnyValue`](@ref), [`ValueIn`](@ref), and [`ValueNotIn`](@ref).
"""
abstract type ValueMatcher{T} <: NIndexParser{1,T} end
regex_prefix(x::ValueMatcher) = "["
regex_suffix(x::ValueMatcher) = "]"
result_type(::Type{ValueMatcher{T}}) where T = T

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
"""
    AnyChar() = AnyValue(Char)
"""
AnyChar() = AnyValue(Char)
@deprecate AnyValue() AnyChar()
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




export ValueIn, CharIn
"""
    ValueIn(x)

Parser matching exactly one element `c` (character) in a sequence, iif [`_ismatch`](@ref)`(c,x)`.

```jldoctest
julia> a_z = ValueIn('a':'z')
[a-z] ValueIn
::Char

julia> parse(a_z, "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> ac = CharIn("ac")
[ac] ValueIn
::Char

julia> parse(ac, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

julia> l = CharIn(islowercase)
[islowercase(...)] ValueIn
::Char

julia> parse(l, "c")
'c': ASCII/Unicode U+0063 (category Ll: Letter, lowercase)

```
"""
@auto_hash_equals struct ValueIn{T,S} <: ValueMatcher{T}
    pcre::String
    sets::S
    function ValueIn{T}(pcre::String, x_...) where T
        label, x = flatten_valuepatterns(x_...)
        new{T,typeof(x)}(pcre == "" ? label : pcre,x)
    end
    function ValueIn(pcre::String, x_...)
        ## @show pcre
        label, x = flatten_valuepatterns(x_...)
        T = valuepattern_type(x)
        ## @show pcre == "" ? label : pcre
        new{T,typeof(x)}(pcre == "" ? label : pcre,x)
    end
end
@inline _ismatch(c,p::ValueIn)::Bool = _ismatch(c,p.sets)
regex_inner(x::ValueIn) = ( x.pcre == "" ? _regex_string(x.sets) : x.pcre )

ValueIn{T}(x_...) where T = ValueIn{T}("",x_...)
ValueIn(x_...) = ValueIn("",x_...)
ValueIn{Char}(chars::String) = ValueIn{Char}(regex_escape(chars),chars)

parser(x::UnicodeClass) = ValueIn{Char}(x)




@deprecate ValueIn(unicode_classes::Symbol...) ValueIn{Char}(UnicodeClass(unicode_classes...))




export ValueNotIn, CharNotIn
"""
    ValueNotIn{T}(label::AbstractString, x)

Parser matching exactly one element (character) in a sequence, iif not in `x`.


    ValueNotIn([label::AbstractString="", ]x...)
    ValueNotIn{T}([label::AbstractString="", ]x...)

Flattens `x` with [`CombinedParsers.flatten_valuepatterns`](@ref), and tries to infer `T` if not provided.

```jldoctest
julia> a_z = CharNotIn('a':'z')
[^a-z] ValueNotIn
::Char

julia> ac = CharNotIn("ca")
[^ca] ValueNotIn
::Char
```

Respects boolean logic:
```jldoctest
julia> CharNotIn(CharNotIn("ab"))("a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

Respects boolean logic:
```jldoctest
julia> CharIn(CharIn("ab"))("a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> CharIn(CharNotIn("bc"))("a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)

julia> parse(CharNotIn(CharIn("bc")), "a")
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

"""
@auto_hash_equals struct ValueNotIn{T,S} <: ValueMatcher{T}
    pcre::String
    sets::S
    function ValueNotIn{T}(pcre::String, x_...) where T
        label, x = flatten_valuepatterns(x_...)
        new{T,typeof(x)}(pcre == "" ? label : pcre,x)
    end
    function ValueNotIn(pcre::String, x_...) 
        label, x = flatten_valuepatterns(x_...)
        T = valuepattern_type(x)
        new{T,typeof(x)}(pcre == "" ? label : pcre,x)
    end
end
@inline _ismatch(c,p::ValueNotIn)::Bool = !_ismatch(c,p.sets)
regex_inner(x::ValueNotIn) = "^"*( x.pcre =="" ? _regex_string(x.sets) : x.pcre )

ValueNotIn{T}(x_...) where T = ValueNotIn{T}("",x_...)
ValueNotIn(x_...) = ValueNotIn("",x_...)
ValueNotIn{Char}(chars::String) = ValueNotIn{Char}(regex_escape(chars),chars)
@deprecate ValueNotIn(unicode_classes::Symbol...) ValueNotIn(UnicodeClass(unicode_classes...))











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





_push!(charset::Nothing, ::Nothing) = nothing
_push!(charset::Nothing, x) = x
_push!(charset::T, x::T) where T = Set{T}(tuple(charset,x))
_push!(charset::Set, x) where T = push!(charset,x)
_push!(charset::Vector, x) where T = push!(charset,x)
_push!(charset::Nothing, x::Union{<:Function,<:UnicodeClass,<:ValueNotIn}) = Any[x]


flatten_valuepatterns!(label,charset,otherstuff,x) =
    label*_regex_string(x), _push!(charset,x), otherstuff

flatten_valuepatterns!(label,charset,otherstuff,x::Union{<:Function,<:UnicodeClass,<:ValueNotIn}) =
    label*_regex_string(x), charset, _push!(otherstuff,x)

flatten_valuepatterns!(label,charset,otherstuff,x::ConstantParser{Char}) =
    flatten_valuepatterns!(label,charset,otherstuff, x.parser)

# e.g. ValueIn{UnicodeClass}
flatten_valuepatterns!(label,charset,otherstuff,c::ValueIn) = 
    label * regex_inner(c), flatten_valuepatterns!(label,charset,otherstuff, c.sets)[2:3]...

ElementIterators = Union{<:Vector,<:Tuple,<:StepRange,<:Set,<:AbstractString,<:AbstractSet}
function flatten_valuepatterns!(label,charset,otherstuff,x::ElementIterators)
    for e in x
        label2, charset, otherstuff = flatten_valuepatterns!(label, charset, otherstuff,e)
    end
    label * _regex_string(x), charset, otherstuff
end


"""
    flatten_valuepatterns(x...)

Used in `ValueMatcher` constructors.

Heuristic is roughly:
- collect `ElementIterators` in a `Set`
- collect everything else in a `Tuple` (`Function`s etc.)
- in the process the `label` is concatenated
- return all that was collected as `Tuple{String, <:Set, <:Tuple}` or `Tuple{String, <:Set}` or `Tuple{String, <:Tuple}`.
"""
function flatten_valuepatterns(x...)
    label, charset,otherstuff = flatten_valuepatterns!("",nothing,nothing,x)
    label, if otherstuff===nothing
        charset === nothing ? nothing : charset
    elseif charset===nothing
        tuple(otherstuff...)
    elseif isempty(otherstuff)
        charset
    else
        tuple(charset,otherstuff...)
    end
end

valuepattern_type(x) =
    if x isa Tuple
        valuepattern_type(x[1])
    elseif x isa AbstractSet
        eltype(x)
    else
        @info "value type" x
        typeof(x) # error()
    end

"""
    CharIn(a...; kw...) = ValueIn{Char}(a...; kw...)
"""
CharIn(a...; kw...) = ValueIn{Char}(a...; kw...)

"""
    CharNotIn(a...; kw...) = ValueNotIn{Char}(a...; kw...)
"""
CharNotIn(a...; kw...) = ValueNotIn{Char}(a...; kw...)

