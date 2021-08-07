# Internal API
## Iterating
Iteration is done with states.
```@docs
CombinedParsers.state_type
```

```@docs
MatchesIterator
ParseMatch
CombinedParsers.parsematch_tuple
```

```@docs
CombinedParsers._iterate
```

```@docs
CombinedParsers.tuple_pos
CombinedParsers.tuple_state
CombinedParsers.leftof
CombinedParsers._leftof
CombinedParsers.rightof
CombinedParsers._rightof
```

From result can (re-)construct [`CombinedParsers.leftof`](@ref).
# Internal Types
## Abstract Parsers
```@docs
CombinedParsers.CombinedParser
CombinedParsers.LeafParser
CombinedParsers.Assertion
```

## States
```@docs
CombinedParsers.MatchState
CombinedParsers.NoMatch
CombinedParsers.NCodeunitsState
```

## Wrapped Parsers
```@docs
CombinedParsers.FilterParser
CombinedParsers.ConstantParser
CombinedParsers.NIndexParser
CombinedParsers.WrappedParser
CombinedParsers.WrappedAssertion
```

## Printing
```@docs
CombinedParsers.print_constructor
CombinedParsers.MemoTreeChildren
```

### PCRE
printing currently in tree view, but has inconsistencies (might not result in the PCRE regex equivalent to the parser).
```@docs
Base.escape_string
regex_string
CombinedParsers.regex_prefix
CombinedParsers.regex_inner
CombinedParsers.regex_suffix
```

## Rewriting Parsers
```@docs
CombinedParsers.deepmap_parser
CombinedParsers._deepmap_parser
CombinedParsers.Regexp.NoDict
```

