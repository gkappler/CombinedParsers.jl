@nospecialize
export deepmap_parser

struct RecursionMarker end
getcache!(f,mem,x::CombinedParser) = get!(f,mem,x)
getcache!(f,mem,x) = f()

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


_deepmap_parser(f::Function,mem::AbstractDict,x::Transformation,a...;kw...) =
    Transformation(
        x.transform,
        deepmap_parser(f,mem,x.parser,a...;kw...))

"""
    deepmap_parser(f::Function[, mem::AbstractDict=IdDict()], x::CombinedParser,a...;kw...)

Perform a deep transformation of a `x`.

Default method
1. Returns cached result if `haskey(x, mem)` to avoid infinite recursion.
2. construct deep transformation `dt = _deepmap_parser(f, mem, x, a...; kw...)`
3. cache and return `f(dt, a...; kw...)`

Used for [`log_names`](@ref).

## For a new `CombinedParser`, 
define either `deepmap_parser` or `_deepmap_parser`.

## For a parser transformation `f`, 
define either custom
- `deepmap_parser(::typeof(f),...)` (see example implementation [`substitute`](@ref))
- construction method `_deepmap_parser(::typeof(f),...)`  (see example implementation [`caseless`](@ref))
- leaf method `f` (see example implementation [`deepmap`](@ref))
"""
deepmap_parser(f,x::CombinedParser, a...;kw...) =
    deepmap_parser(f,IdDict(),x,a...;kw...)

deepmap_parser(f,mem::AbstractDict, x, a...; kw...) =
    getcache!(mem,x) do
        dt = _deepmap_parser(f, mem, x, a...; kw...)
        f(dt, a...; kw...)
    end

_deepmap_parser(f, mem::AbstractDict, x::LeafParser,a...;kw...) = x

_deepmap_parser(f, mem::AbstractDict, x::Union{AtStart,AtEnd,Always,Never}, a...; kw...) = x

_deepmap_parser(f,mem::AbstractDict, x::ConstantParser,a...;kw...) = x

"""
    deepmap_parser(f,mem::AbstractDict,x,a...;kw...)

Perform a deep transformation of a CombinedParser.

!!! note
    For a custom parser `P<:CombinedParser` with sub-parsers, provide a method
    ```julia
    CombinedParsers._deepmap_parser(f,mem::AbstractDict,x::P,a...;kw...) =
         ## construct replacement, e.g. if P <: WrappedParser
         P(deepmap_parser(f,mem,x.parser,a...;kw...))
    ```
"""
_deepmap_parser(f,mem::AbstractDict,x::CombinedParser,a...;kw...) =
    error("""
    For a custom parser `$(typeof(x))` with sub-parsers, provide a method
    ```julia
    CombinedParsers._deepmap_parser(f,mem::AbstractDict,x::$(typeof(x)),a...;kw...) =
          ## construct replacement, e.g. if P <: WrappedParser
          $(typeof(x))(deepmap_parser(f,mem,x.parser,a...;kw...))
    ```
""")

_deepmap_parser(f,mem::AbstractDict,x::Atomic,a...;kw...) =
    Atomic(deepmap_parser(f,mem,x.parser,a...;kw...))

_deepmap_parser(f,mem::AbstractDict,x::Lazy,a...;kw...) =
    Lazy(deepmap_parser(f,mem,x.parser,a...;kw...))

_deepmap_parser(f,mem::AbstractDict,x::Sequence,a...;kw...) =
    Sequence( ( deepmap_parser(f,mem,p,a...;kw...)
                for p in x.parts)... )

_deepmap_parser(f,mem::AbstractDict,x::Optional,a...;kw...) =
    Optional(deepmap_parser(f,mem,x.parser,a...;kw...);
             default=x.default)

_deepmap_parser(f,mem::AbstractDict,x::NamedParser,a...;kw...) =
    NamedParser(x.name,deepmap_parser(f,mem,x.parser,a...;kw...))

_deepmap_parser(f,mem::AbstractDict,x::Repeat,a...;kw...) =
    Repeat(x.range,
           deepmap_parser(f,mem,x.parser,a...))


_deepmap_parser(f,mem::AbstractDict,x::MappedSequenceParser,a...;kw...) =
    MappedSequenceParser(x.f,deepmap_parser(f,mem,x.parser,a...;kw...))

_deepmap_parser(f,mem::AbstractDict,x::SideeffectParser,a...;kw...) =
    SideeffectParser(
        x.effect,
        deepmap_parser(f,mem,x.parser,a...;kw...),
        x.args...; x.keywords...)

_deepmap_parser(f,mem::AbstractDict,x::FlatMap,a...;kw...) =
    FlatMap{result_type(x)}(
        x.right,# v -> deepmap_parser(f,mem,x.right(v),a...;kw...),
        deepmap_parser(f,mem,x.left,a...;kw...))

_deepmap_parser(f,mem::AbstractDict,x::Either,a...;kw...) =
    deepmap_either(f,mem,x,a...;kw...)

deepmap_either(f,mem::AbstractDict,x::Either{<:Tuple},a...;kw...) =
    Either((deepmap_parser(f,mem,p,a...;kw...) for p in x.options)... )

function deepmap_either(f,mem::AbstractDict,x::Either{<:Vector},a...;kw...)
    r = Either(Any[])
    wrapped = f(r, a...; kw...) 
    mem[x] = wrapped # Pre-wrap with transducer to cache left-recursion knot correctly
    for p in x.options
        push!(r,deepmap_parser(f,mem,p,a...;kw...))
    end
    wrapped
end

"""
    strip_either1(x::CombinedParser)

Replace all `Either` parsers with one option with that option.

Used in 2-stage [`substitute`](@ref) (stage 1: collect for recursion, stage 2: simplify).
"""
strip_either1(x::CombinedParser) = deepmap_parser(_strip_either1, x)
_strip_either1(x::CombinedParser) = x

function deepmap_parser(::typeof(_strip_either1), mem::AbstractDict, x::Either)
    haskey(mem, x) && return mem[x]
    
    if length(x.options) == 1
        # Set cache upfront to prevent StackOverflow on unit-length cyclic nodes
        mem[x] = x 
        r = deepmap_parser(_strip_either1, mem, first(x.options))
        mem[x] = r
        return r
    else
        # deepmap_either caches natively for Either{<:Vector}, but not Tuples.
        if x isa Either{<:Tuple}
            mem[x] = x
            r = deepmap_either(_strip_either1, mem, x)
            mem[x] = r
            return r
        else
            return deepmap_either(_strip_either1, mem, x)
        end
    end
end

export substitute

struct Substitution<:CombinedParser
    name::Symbol
end
print_regex(io::IO, s::Substitution) =
    printstyled(io,"\\", s.name)
state_type(::Type{Substitution}) =
    Nothing

"""
    substitute(name::Symbol)

Define a parser substitution.

    substitute(parser::CombinedParser)

Apply parser substitution, respecting scope in the defined tree:

- Parser variables are defined within scope of `Either`s, for all its `NamedParser` options.
- `Substitution` parsers are replaced with parser variables.
- [`strip_either1`](@ref) is used to simplify in a second phase.

!!! note
    Substitution implementation is experimental pending feedback. 

    todo: scope NamedParser objects in WrappedParser, Sequence, etc.?

```jldoctest
julia> Either(:a => !Either(
                 :b => "X", 
                 :d => substitute(:b),
                 substitute(:c)),
              :b => "b",
              :c => substitute(:b)
              ) |> substitute
|🗄 Either
├─ |🗄 Either |> ! |> with_name(:a)
│  ├─ X  |> with_name(:b)
│  ├─ X  |> with_name(:b) |> with_name(:d)
│  └─ b  |> with_name(:b) |> with_name(:c)
├─ b  |> with_name(:b)
└─ b  |> with_name(:b) |> with_name(:c)
::SubString{String}
```

# Example
With `substitute` you can write recursive parsers in a style inspired by (E)BNF.
[`CombinedParsers.BNF.ebnf`](@ref) uses `substitute`.

```jldoctest
julia> def = Either(:integer => !Either("0", Sequence(Optional("-"), substitute(:natural_number))),
                    :natural_number => !Sequence(substitute(:nonzero_digit), Repeat(substitute(:digit))),
                    :nonzero_digit => re"[1-9]",
                    :digit => Either("0", substitute(:nonzero_digit)))
|🗄 Either
├─ |🗄 Either |> ! |> with_name(:integer)
│  ├─ 0 
│  └─ 🗄 Sequence
│     ├─ \\-? |
│     └─  natural_number call substitute!
├─ 🗄 Sequence |> ! |> with_name(:natural_number)
│  ├─  nonzero_digit call substitute!
│  └─ * digit call substitute! |> Repeat
├─ [1-9] ValueIn |> with_name(:nonzero_digit)
└─ |🗄 Either |> with_name(:digit)
   ├─ 0 
   └─  nonzero_digit call substitute!
::Union{Nothing, Char, SubString{String}}

julia> substitute(def)
|🗄 Either
├─ |🗄 Either |> ! |> with_name(:integer)
│  ├─ 0 
│  └─ 🗄 Sequence
│     ├─ \\-? |
│     └─ 🗄 Sequence |> ! |> with_name(:natural_number) # branches hidden
├─ 🗄 Sequence |> ! |> with_name(:natural_number)
│  ├─ [1-9] ValueIn |> with_name(:nonzero_digit)
│  └─ |🗄* Either |> with_name(:digit) |> Repeat
│     ├─ 0 
│     └─ [1-9] ValueIn |> with_name(:nonzero_digit)
├─ [1-9] ValueIn |> with_name(:nonzero_digit)
└─ |🗄 Either |> with_name(:digit)
   ├─ 0 
   └─ [1-9] ValueIn |> with_name(:nonzero_digit)
::Union{Char, SubString{String}}
```
"""
substitute(name::Symbol) = 
    Substitution(name)
substitute(name::AbstractString) = 
    Substitution(Symbol(name))
CombinedParsers.iterate_state(parser::Substitution, a...) = error(" call substitute")
CombinedParsers._deepmap_parser(f,mem::AbstractDict,x::Substitution,a...;kw...) = x

substitute(x::CombinedParser) =
    strip_either1(deepmap_parser(_substitute, x, Dict{Symbol,CombinedParser}()))

_substitute(parser, assignments) = parser
function _substitute(parser::Substitution, assignments::AbstractDict)
    get(assignments,parser.name) do
        error("parser $(parser.name) is not defined")
    end
end

# set assignments
function deepmap_parser(::typeof(_substitute), mem::AbstractDict, x::Either, assignments)
    haskey(mem, x) && return mem[x]
    
    _assignments = copy(assignments)
    for o in x.options
        while o isa WrappedParser
            if o isa NamedParser
                _assignments[o.name] = Either(Any[])
            end
            o = o.parser
        end
    end
    for o in x.options
        while o isa WrappedParser
            if o isa NamedParser
                push!(_assignments[o.name], deepmap_parser(_substitute, mem, o, _assignments))
            end
            o = o.parser
        end
    end
    
    # Break cycles on immutable tuples manually
    if x isa Either{<:Tuple}
        mem[x] = x
        r = deepmap_either(_substitute, mem, x, _assignments)
        mem[x] = r
        return r
    else
        return deepmap_either(_substitute, mem, x, _assignments)
    end
end

# skip mem lookup (accept no assignments outside call/nesting parser stack!)
function deepmap_parser(::typeof(_substitute), mem::AbstractDict, x::Substitution, assignments)
    _substitute(x, assignments)
end


Base.foldl(f::Function, x::Either{<:AbstractTrie}, acc, a...; cache=IdDict{CombinedParser,Any}(), kw...) = f(x,acc,a...; kw...)
Base.foldl(f::Function, x::FlatMap, acc, a...; cache=IdDict{CombinedParser,Any}(), kw...) = f(x,f(x.left,acc,a...; kw...),a...; kw...)
function Base.foldl(f::Function, x::CombinedParser, acc,a...; cache=IdDict{CombinedParser,Any}(), kw...)
    ##printnode(stdout,x)
    ##print(" ", length(cache)," ")
    cachable(x) = x isa Either{<:Vector}
    if cachable(x) && haskey(cache,x)
        cache[x]
    else
        acc = f(x,acc,a...; kw...)
        if cachable(x) # && true || x isa NamedParser
            cache[x] = acc
            haskey(cache,x) || error()
        end
        for c in children(x)
            acc = foldl(f, c, acc,a...; kw..., cache=cache)
        end
        if cachable(x) # && true || x isa NamedParser
            cache[x] = acc
        end
        acc
    end
end


export with_log


"""
    with_log(s::AbstractString,p, delta=5;nomatch=false)

Log matching process of parser `p`, displaying `delta` characters left of and right of match.

If `nomatch==true`, also log when parser does not match.

See also: [`log_names`](@ref), [`with_effect`](@ref)
"""
with_log(log::AbstractString,p_; nomatch=false, kw...) =
    let p = parser(p_)
        ##SideeffectParser()
        with_effect(nomatch ? log_effect : log_effect_match ,p, log; kw...)
    end

function log_parser(message::Function, x::CombinedParser, a...; delta_char::Integer=5, nomatch=false, io=stdout, kw...)
    function _log_names(x´::CombinedParser)
        log = message(x´,a...; kw...)
        if log!==nothing
            with_log("$(log)",x´; io=io, nomatch=nomatch, delta_char=delta_char)
        else
            x´
        end
    end
    deepmap_parser(_log_names,Dict(),x)
end

"""
    log_parser(message::Type, x::CombinedParser, a...; kw...)
    log_parser(message::Function, x::CombinedParser, a...; kw...)

Transform parser including logging statements for sub-parsers 
of type `message` or 
for which calling `message` does not return `nothing`.
"""
function log_parser(message::Type, x::CombinedParser; kw...)
    function log_type(p)
        if p isa message
            iostring(printnode, p)
        else
            nothing
        end
    end
    log_parser(log_type, x; kw...)
end




export log_parser, log_names

"""
    log_names(x,names=true; exclude=nothing, kw...)

Rebuild parser replacing `NamedParser` instances with `with_log` parsers.
Log all `NamedParser` instanses if `names==true` or `name in names` and not `name in exclude`.

See also: [`with_log`](@ref), [`log_parser`](@ref), [`deepmap_parser`](@ref)
"""
function log_names(x, names=true; exclude=nothing, kw...)
    message = if names === true
        if exclude === nothing
            x -> x isa NamedParser && x.doc=="" ? x.name : nothing
        else
            x -> ( x isa NamedParser && !in(x.name,exclude) ) ? x.name : nothing
        end
    elseif names isa Type
        return log_parser(names, x)
    else
        x -> ( x isa NamedParser && in(x.name,names) ) ? x.name : nothing
    end
    log_parser(message, x; kw...)
end

include("log.jl")

export optimize
optimize(x) = deepmap_parser(_optimize,x)
_optimize(x,a...) = x
_deepmap_parser(::typeof(_optimize),dict::AbstractDict,x::SideeffectParser) = x.parser

@specialize
