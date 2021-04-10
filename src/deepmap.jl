export deepmap_parser
deepmap_parser(f::Function,mem::AbstractDict,x::SideeffectParser,a...;kw...) =
    get!(mem,x) do
        SideeffectParser(x.effect,
                         deepmap_parser(f,mem,x.parser,a...;kw...),
                         x.args...)
    end

deepmap_parser(f::Function,mem::AbstractDict,x::FlatMap,a...;kw...) =
    get!(mem,x) do
        FlatMap{result_type(x)}(x.right,# v -> deepmap_parser(f,mem,x.right(v),a...;kw...),
                                deepmap_parser(f,mem,x.left,a...;kw...))
    end

deepmap_parser(f::Function,mem::AbstractDict,x::Sequence,a...;kw...) =
    get!(mem,x) do
        Sequence( ( deepmap_parser(f,mem,p,a...;kw...)
                    for p in x.parts)... )
    end

deepmap_parser(f::Function,mem::AbstractDict,x::Repeat,a...;kw...) =
    get!(mem,x) do
        f(Repeat(x.range,
                 deepmap_parser(f,mem,x.parser,a...)),a...;kw...)
    end

deepmap_parser(f::Function,mem::AbstractDict,x::Optional,a...;kw...) =
    get!(mem,x) do
        Optional(deepmap_parser(f,mem,x.parser,a...;kw...);
                 default=x.default)
    end


function deepmap_parser(f::Function,mem::AbstractDict,x::Either{<:Vector},a...;kw...)
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

function deepmap_parser(f::Function,mem::AbstractDict,x::Either{<:Tuple},a...;kw...)
    get!(mem,x) do
        Either((deepmap_parser(f,mem,p,a...;kw...) for p in x.options)... )
    end
end

deepmap_parser(f::Function,mem::AbstractDict,x::Atomic,a...;kw...) =
    get!(mem,x) do
        Atomic(
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end

deepmap_parser(f::Function,mem::AbstractDict,x::Lazy,a...;kw...) =
    get!(mem,x) do
        Lazy(deepmap_parser(f,mem,x.parser,a...;kw...))
    end

deepmap_parser(f::Function,mem::AbstractDict,x::CombinedParser,a...;kw...) =
    f(x,a...;kw...)

"""
    deepmap_parser(f::Function,x::CombinedParser,a...;kw...)

Perform a deep transformation of a CombinedParser.
Used for [`log_names`](@ref).

Calls `deepmap_parser(f,IdDict(),x,a...)`.
"""
deepmap_parser(f::Function,x::CombinedParser,a...;kw...) =
    deepmap_parser(f,IdDict(),x,a...;kw...)

"""
    deepmap_parser(f::Function,mem::AbstractDict,x,a...;kw...)

Perform a deep transformation of a CombinedParser.

!!! note
    For a custom parser `P<:CombinedParser` with sub-parsers, provide a method
    ```julia
    deepmap_parser(f::Function,mem::AbstractDict,x::P,a...;kw...) =
        get!(mem,x) do
            ## construct replacement, e.g. if P <: WrappedParser
            P(deepmap_parser(f,mem,x.parser,a...;kw...))
        end
    ```
"""
deepmap_parser(f::Function,mem::AbstractDict,x,a...;kw...) =
    error("""
    For a custom parser `$(typeof(x))` with sub-parsers, provide a method
    ```julia
    deepmap_parser(f::$(typeof(f)),mem::AbstractDict,x::$(typeof(x)),a...;kw...) =
        get!(mem,x) do
            ## construct replacement, e.g. if P <: WrappedParser
            P(deepmap_parser(f,mem,x.parser,a...;kw...))
        end
    ```
""")

"""
    deepmap_parser(f::Function,mem::AbstractDict,x::LeafParser,a...;kw...)

return 
```julia
    get!(mem,x) do
        f(x,a...;kw...)
    end
```
"""
deepmap_parser(f::Function,mem::AbstractDict,x::LeafParser,a...;kw...) =
    get!(mem,x) do
        f(x,a...;kw...)
    end

deepmap_parser(f::Function,mem::AbstractDict,x::NamedParser,a...;kw...) =
    get!(mem,x) do
        NamedParser(x.name,deepmap_parser(f,mem,x.parser,a...;kw...))
    end

deepmap_parser(f::Function,mem::AbstractDict,x::Numeric,a...; kw...) = x

