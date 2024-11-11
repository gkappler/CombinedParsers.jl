include("unicode.jl")
"""
`ValueMatcher` match value at point `c` iif [`ismatch`](@ref)`(c, parser)`.
A `ValueMatcher=NIndexParser{1}` and has `state_type` `MatchState`.

See [`AnyValue`](@ref), [`ValueIn`](@ref), and [`ValueNotIn`](@ref).
"""
abstract type ValueMatcher <: NIndexParser{1} end

result_type(::ValueMatcher, sequence::Type) =
    eltype(sequence)


export AnyValue, AnyChar
"""
    AnyValue()

Parser matching exactly one `position`, returning the value.
```jldoctest
julia> AnyValue()
. AnyValue
```

"""
struct AnyValue <: ValueMatcher end

@deprecate AnyChar() AnyValue()

"""
    iterate_state(parser::ValueMatcher, sequence, till, posi, next_i, state::Nothing)

When implementing a `Custom<:ValueMatcher` it suffices to provide a method [`CombinedParsers._ismatch`](@ref)`(c, parser::Custom)`.
"""
@inline function iterate_state(parser::ValueMatcher, sequence, till, posi, next_i, state::Nothing)
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




valuepattern_type(x::Type) =
    if x <: Tuple
        valuepattern_type(x[1])
    elseif x <: AbstractSet
        eltype(x)
    else
        typeof(x) # error()
    end

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
@auto_hash_equals struct ValueIn{S} <: ValueMatcher
    pcre::String
    sets::S
    function ValueIn(pcre::AbstractString, x_...)
        label, x = flatten_valuepatterns(x_)
        new{typeof(x)}(pcre == "" ? label : pcre,x)
    end
end
ValueIn(x_...) = ValueIn("", x_...)

@inline _ismatch(c,p::ValueIn)::Bool = _ismatch(c,p.sets)

parser(x::UnicodeClass) = ValueIn(x)

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
@auto_hash_equals struct ValueNotIn{S} <: ValueMatcher
    pcre::String
    sets::S
    function ValueNotIn(pcre::String, x_...) 
        label, x = flatten_valuepatterns(x_)
        new{typeof(x)}(pcre == "" ? label : pcre,x)
    end
end
# result_type(::Type{T}) where T = T
@inline _ismatch(c,p::ValueNotIn)::Bool = !_ismatch(c,p.sets)

ValueNotIn(x_...) = ValueNotIn("",x_...)




"""
    CharIn(a...; kw...) = ValueIn{Char}(a...; kw...)
"""
CharIn(a...; kw...) = ValueIn(a...; kw...)
CharIn(str::AbstractString; kw...) = ValueIn(str,str...; kw...)

"""
    CharNotIn(a...; kw...) = ValueNotIn{Char}(a...; kw...)
"""
CharNotIn(a...; kw...) = ValueNotIn(a...; kw...)
CharNotIn(str::AbstractString; kw...) = ValueNotIn(str,str...; kw...)

ValueIn{Char}(chars::AbstractString) =
    isempty(chars) ? Never() : ValueIn(chars,chars...)
ValueNotIn{Char}(chars::AbstractString) =
    isempty(chars) ? Always() : ValueNotIn(chars,chars...)

@deprecate ValueIn{T}(x_...) where T ValueIn("",x_...)
@deprecate ValueNotIn{T}(x_...) where T ValueNotIn("",x_...)

@deprecate ValueIn{T}(label::AbstractString=constructor_name(T)) where T ValueIn(label, x-> x isa T)
@deprecate ValueNotIn{T}(label::AbstractString=constructor_name(T)) where T ValueNotIn(label, x-> x isa T)

@deprecate ValueIn(unicode_classes::Symbol...) ValueIn(UnicodeClass(unicode_classes...))
@deprecate ValueNotIn(unicode_classes::Symbol...) ValueNotIn(UnicodeClass(unicode_classes...))


# ValueNotIn(chars::StepRange) =
#     ValueNotIn{eltype(chars)}("$(chars.start)-$(chars.stop)",chars)
# ValueNotIn(pcre::String,x::ConstantParser{Char}) =
#     ValueNotIn{Char}(pcre,x.parser)
# ValueIn(x::Tuple{<:ValueNotIn}) = x[1]












function _regex_backect(x)
    io = IOBuffer()
    _print_bracket(io,x)
    String(take!(io))
end
ElementIterators = Union{<:Vector,<:Tuple,<:StepRange,<:Set,<:AbstractString,<:AbstractSet}
function flatten_valuepatterns!(x,
                                label = "",
                                charset = Any[],
                                otherstuff = Any[])
    for e in x
        if e isa ConstantParser{<:AbstractChar}
            label = label*_regex_backect(e.parser)
            push!(charset, e.parser)
        elseif e isa AbstractChar
            label = label*_regex_backect(e)
            push!(charset, e)
        elseif e isa ElementIterators
            #@info "flatten" typeof(x) x
            label2, charset, otherstuff = flatten_valuepatterns!(e, label, charset, otherstuff)
            label = if e isa StepRange
                label*_regex_backect(e)
            else
                label2
            end
        elseif e isa WrappedParser
            label, charset, otherstuff = flatten_valuepatterns!((e.parser,), label, charset, otherstuff)
        elseif e isa ValueIn
            if e.sets !== nothing
                _, charset, otherstuff = flatten_valuepatterns!(e.sets, label, charset, otherstuff)
            else
                #print(e)
                #error(e)
                # warning in pcre
            end
            label = label * e.pcre
        elseif e isa Union{<:Function,<:UnicodeClass,<:ValueNotIn}
            label = label*_regex_backect(e)
            push!(otherstuff, e)
        else
            error(e)
            label, charset, otherstuff = flatten_valuepatterns!(e, label, charset, otherstuff)
        end
    end

    label, charset, otherstuff
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
function flatten_valuepatterns(x)
    label, charset, otherstuff = flatten_valuepatterns!(x)
    label, if isempty(otherstuff)
        isempty(charset) ? nothing : Set([charset...])
    elseif isempty(charset)
        tuple(otherstuff...)
    elseif isempty(otherstuff)
        Set([charset...])
    else
        tuple(Set([charset...]),otherstuff...)
    end
end
