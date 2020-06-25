# Internal API
## Iterating Parsings
```@docs
CombinedParsers._iterate
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
CombinedParsers.MatchState
CombinedParsers.None
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


## Regular Expression Types
```@docs
CombinedParsers.Regexp.parse_options
regex_escape
CombinedParsers._iterate(::CombinedParsers.Regexp.ParserWithCaptures,::CombinedParsers.Regexp.SequenceWithCaptures,a...)
```

```@docs
CombinedParsers.Regexp.ParserWithCaptures
CombinedParsers.Regexp.SequenceWithCaptures
CombinedParsers.Regexp.ParseMatch
CombinedParsers.Regexp.Capture
CombinedParsers.Regexp.Backreference
CombinedParsers.Regexp.Subroutine
CombinedParsers.Regexp.Conditional
CombinedParsers.Regexp.DupSubpatternNumbers
```

