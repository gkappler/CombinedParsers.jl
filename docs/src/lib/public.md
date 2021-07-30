# Matching and Parsing
```@docs
CombinedParsers
```
Documentation for `CombinedParsers.jl`'s `match` and `parse` interface.

!!! note 
    Building parsers is detailed in the sections
	- [constructors](constructors.md),  
	- [regex](regexp.md), and
    - [templates](parsers.md).

## Matching
```@docs
match
```

## Parsing
A [`match`](@ref) can be transformed to a Julia [`result_type`](@ref).
!!! note 
    Defining transformations is detailed in the [transformation](transformation.md) section.

```@docs
parse
tryparse
tryparse_pos
```

## Iterating matches
`CombinedParsers` iterates through matches if parsing is ambiguous.
!!! note 
    Iteration is detailed in the [internals](internals.md) section.

```@docs
Base.iterate
match_all
parse_all
```

