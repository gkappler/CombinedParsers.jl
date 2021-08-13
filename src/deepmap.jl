export deepmap_parser

"""
    deepmap_parser(f::Function[, mem::AbstractDict=IdDict()], x::CombinedParser,a...;kw...)

Perform a deep transformation of a `x`.

Default method
1. Returns cached result if `haskey(x, mem)` to avoid infinite recursion.
2. construct deep transformation `dt = _deepmap_parser(f, mem, x, a...; kw...)`
3. cache and return `f(dt, a...; kw...)`

For a new `CombinedParser`, define either `deepmap_parser` or `_deepmap_parser`.

For a parser transformation `f`, define either 
- `deepmap_parser(::typeof(f),...)`
- `_deepmap_parser(::typeof(f),...)`
- `f`

Used for [`log_names`](@ref).
"""
deepmap_parser(f,x::CombinedParser, a...;kw...) =
    deepmap_parser(f,IdDict(),x,a...;kw...)

deepmap_parser(f,mem::AbstractDict, x, a...; kw...) =
    get!(mem,x) do
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
        x.args...)

_deepmap_parser(f,mem::AbstractDict,x::FlatMap,a...;kw...) =
    FlatMap{result_type(x)}(
        x.right,# v -> deepmap_parser(f,mem,x.right(v),a...;kw...),
        deepmap_parser(f,mem,x.left,a...;kw...))


@inline deepmap_parser(f,mem::AbstractDict,x::Either,a...;kw...) =
    get!(mem,x) do
        f(deepmap_either(f,mem,x,a...;kw...),a...;kw...)
    end

deepmap_either(f,mem::AbstractDict,x::Either{<:Tuple},a...;kw...) =
    Either((deepmap_parser(f,mem,p,a...;kw...) for p in x.options)... )

function deepmap_either(f,mem::AbstractDict,x::Either{<:Vector},a...;kw...)
    if haskey(mem,x)
        mem[x]
    else
        mem[x] = r = Either{result_type(x)}(Any[])
        ## f(x,a...)
        for p in x.options
            push!(r,deepmap_parser(f,mem,p,a...;kw...))
        end
        r
    end
end

