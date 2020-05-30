# Internal API
## Iterating Parsings
```@docs
CombinedParsers._iterate
CombinedParsers.get
CombinedParsers.ismatch
```

## Rewriting Parsers
```@docs
CombinedParsers.deepmap_parser
```

## Parsing Options
```@docs
CombinedParsers.Regexp.ParserOptions
CombinedParsers.Regexp.FilterOptions
CombinedParsers.Regexp.WithOptions
CombinedParsers.Regexp.OnOptionsParser
```

# Internal Types
## Abstract Parser Types
```@docs
CombinedParsers.ParserTypes
CombinedParsers.CombinedParser
CombinedParsers.LeafParser
CombinedParsers.ConstantParser
CombinedParsers.NIndexParser
CombinedParsers.WrappedParser
CombinedParsers.LookAround
CombinedParsers.Transformation
```

## Regular Expression Types
```@docs
CombinedParsers.Regexp.ParseMatch
CombinedParsers.Regexp.Capture
CombinedParsers.Regexp.Backreference
CombinedParsers.Regexp.Subroutine
CombinedParsers.Regexp.Conditional
CombinedParsers.Regexp.DupSubpatternNumbers
CombinedParsers.Regexp.ParserWithCaptures
CombinedParsers.Regexp.SequenceWithCaptures
```

