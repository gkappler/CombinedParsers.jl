# Using `CombinedParsers`

## Printing
Printing `CombinedParser`s uses [`AbstractTrees.jl`](https://github.com/JuliaCollections/AbstractTrees.jl) for printing.
The tree nodes are printed with 
1. a colored [regular expressions](regexp.md)ish prefix
2. `🗄` Sub-parsers are shown as children branches.
3. [`CombinedParsers.WrappedParser`](@ref) [constructors](constructors.md) are displayed with pipe `|>` syntax.
In the last line of printing the infered result type of the `CombinedParser` is printed.

Printing is useful to understand the structure of [regular expressions](regexp.md), 
while also learning `CombinedParser` syntax:
```@jldoctest
julia> p = trim(re"(?:a+c)*b")
🗄 Sequence[2]
├─ (?>[\h]*) CharIn |> Repeat |> Atomic
├─ 🗄 Sequence
│  ├─ 🗄* Sequence |> Repeat
│  │  ├─ a+  |> Repeat
│  │  └─ c 
│  └─ b 
└─ (?>[\h]*) CharIn |> Repeat |> Atomic
::Tuple{Vector{Tuple{Vector{Char}, Char}}, Char}
```
Parser [templates](parsers.md) 


## Matching
```@docs
match
```

## Parsing
`CombinedParser` comprise of a pattern as well transformation functions to produce a Julia [`result_type`](@ref) from a [`match`](@ref) with [`get`](@ref).
```@jldoctest
julia> match(trim(re"(?:a+c)*b"), "aacacb")
ParseMatch("aacacb")

julia> get(m)
([(['a', 'a'], 'c'), (['a'], 'c')], 'b')
```

!!! note 
    Defining transformations is detailed in the [transformation](transformation.md) section.

```@docs
get
parse
tryparse
tryparse_pos
```

## Iterating matches
`CombinedParsers` iterates through all matches if parsing is ambiguous.
How to write custom parser match iterations is detailed in the [internals](internals.md) section.

```@docs
Base.iterate
match_all
parse_all
```

