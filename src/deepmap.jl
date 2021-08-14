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

"""
    strip_either1(x::CombinedParser)

Replace all `Either` parsers with one option with that option.

Used in 2-stage [`substitute`](@ref) (stage 1: collect for recursion, stage 2: simplify).
"""
strip_either1(x::CombinedParser) = deepmap_parser(_strip_either1, x)
_strip_either1(x::CombinedParser) = x
deepmap_parser(::typeof(_strip_either1),mem::AbstractDict,x::Either) = 
    if length(x.options) == 1
        deepmap_parser(_strip_either1,mem,first(x.options))
    else
        deepmap_either(_strip_either1,mem,x)
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

export substitute

struct Substitution<:CombinedParser{Nothing,Nothing}
    name::Symbol
end
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
[`bnf`](@ref) uses `substitute`.

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
CombinedParsers._iterate(parser::Substitution, a...) = error(" call substitute")
function CombinedParsers.print_constructor(io::IO, x::Substitution)
    printstyled(io, x.name, color=:red)
    print(io, " call substitute!")
end
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
    # deepmap_parser(_strip_either1, deepmap_either(_substitute, mem, x, _assignments))
    deepmap_either(_substitute, mem, x, _assignments)
end

# skip mem lookup (accept no assignments outside call/nesting parser stack!)
function deepmap_parser(::typeof(_substitute), mem::AbstractDict, x::Substitution, assignments)
    _substitute(x, assignments)
end
