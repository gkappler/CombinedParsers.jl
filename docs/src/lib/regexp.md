## CombinedParsers.Regexp
```@docs
CombinedParsers.Regexp
CombinedParsers.Regexp.Regcomb
(==)(::RegexMatch,::ParseMatch)
getindex(::ParseMatch{<:Any,<:CombinedParsers.Regexp.SequenceWithCaptures,<:Any},::Integer)
getproperty(::ParseMatch{<:Any,<:CombinedParsers.Regexp.SequenceWithCaptures,<:Any},::Symbol)
with_options
CombinedParsers.Regexp.@pcre_tests
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
CombinedParsers.Regexp.Capture
CombinedParsers.Regexp.Backreference
CombinedParsers.Regexp.Subroutine
CombinedParsers.Regexp.index(::Subroutine,::Any)
CombinedParsers.Regexp.subroutine_index_reset(::CombinedParsers.Regexp.ParserWithCaptures,::Capture)
CombinedParsers.Regexp.Conditional
CombinedParsers.Regexp.DupSubpatternNumbers
```

## Parsing Options
```@docs
CombinedParsers.Regexp.ParserOptions
CombinedParsers.Regexp.FilterOptions
CombinedParsers.Regexp.WithOptions
CombinedParsers.Regexp.OnOptionsParser
```
