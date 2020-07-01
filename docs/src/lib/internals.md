# Internal API
## Iterating Parsings
```@docs
CombinedParsers.ismatch
CombinedParsers._ismatch
CombinedParsers._iterate
CombinedParsers.tuple_pos
CombinedParsers.tuple_state
```

# Internal Types
## Abstract Parsers
```@docs
CombinedParsers.ParserTypes
CombinedParsers.CombinedParser
CombinedParsers.LeafParser
```

## Wrapped Parsers
```@docs
CombinedParsers.FilterParser
CombinedParsers.ConstantParser
CombinedParsers.NIndexParser
CombinedParsers.WrappedParser
CombinedParsers.Transformation
CombinedParsers.LookAround
```

## States
```@docs
CombinedParsers.MatchState
CombinedParsers.None
CombinedParsers.NCodeunitsState
```

## Printing
```@docs
regex_string
CombinedParsers.regex_prefix
CombinedParsers.regex_inner
CombinedParsers.regex_suffix
CombinedParsers.print_constructor
CombinedParsers.MemoTreeChildren
```



## Rewriting Parsers
```@docs
CombinedParsers.deepmap_parser
CombinedParsers.Regexp.NoDict
```

