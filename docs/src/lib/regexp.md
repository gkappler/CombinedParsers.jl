# PCRE Regular expressions
You can use PCRE `@re_str` in combination with
`CombinedParser`'s [constructors](constructors.md).

## Constructing Regular expressions
```@docs
CombinedParsers.Regexp.@re_str
CombinedParsers.Regexp
CombinedParsers.Regexp.Regcomb
getindex(::ParseMatch{<:ParserWithCaptures,<:SequenceWithCaptures,<:Any},::Integer)
getproperty(::ParseMatch{<:ParserWithCaptures,<:SequenceWithCaptures,<:Any},::Symbol)
regex_escape
```

## Compatibility & Unit Tests
```@docs
CombinedParsers.Regexp.character_class
(==)(::RegexMatch,::ParseMatch)
CombinedParsers.Regexp.@pcre_tests
```

## CombinedParsers.Regexp
```@docs
CombinedParsers._iterate(::CombinedParsers.Regexp.ParserWithCaptures,::CombinedParsers.Regexp.SequenceWithCaptures,a...)
```

## Parsing Options
PCRE options are supported 
```@docs
with_options
CombinedParsers.Regexp.parse_options
CombinedParsers.Regexp.StringWithOptions
CombinedParsers.Regexp.CharWithOptions
CombinedParsers.Regexp.OnOptionsParser
CombinedParsers.Regexp.on_options
CombinedParsers.Regexp.ParserOptions
CombinedParsers.Regexp.FilterOptions
CombinedParsers.Regexp.MatchingNever
```



## Regular Expression Types
```@docs
CombinedParsers.Regexp.ParserWithCaptures
CombinedParsers.Regexp.SequenceWithCaptures
CombinedParsers.Regexp.Capture
CombinedParsers.Regexp.Backreference
CombinedParsers.Regexp.Subroutine
CombinedParsers.Regexp.subroutine_index_reset(::CombinedParsers.Regexp.ParserWithCaptures,::Capture)
CombinedParsers.Regexp.index(::Subroutine,::Any)
CombinedParsers.Regexp.Conditional
CombinedParsers.Regexp.DupSubpatternNumbers
```

