# Public Documentation

Documentation for `CombinedParsers.jl`'s public interface.

See the Internals section of the manual for internal package docs covering all submodules.

# Public Interface

## CombinedParsers


```@docs
CombinedParsers
```

## CombinedParsers.Regexp

```@docs
CombinedParsers.Regexp
CombinedParsers.Regexp.@re_str
CombinedParsers.Regexp.match
regex_escape
```

## Parsing, Logging and Side-Effects
```@docs
parse
with_name
log_names
with_log
instrument
with_effect
```

## Transformations
```@docs
Base.map
map_at
JoinSubstring
```

## Parser Constructors
### Character Matchers
```@docs
AnyChar
CharIn
CharNotIn
```

### Combining Parser
```@docs
Either
Base.(|)
Base.:| :: Tuple{Either,Type}
sEither
Sequence
sSequence
FlatMap
after
Atomic
```

### Repeating
```@docs
Optional
Base.(|)(x::TextParse.AbstractToken{T}, default::Union{T,Missing}) where { T }
Repeat
Repeat1
Repeat_stop
Repeat_until
Base.join
```


### Assertions
```@docs
Always
Never
PositiveLookahead
NegativeLookahead
PositiveLookbehind
NegativeLookbehind
```

## Printing
```@docs
regex_string
CombinedParsers.regex_prefix
CombinedParsers.regex_inner
CombinedParsers.regex_suffix
```



# Internals
```@docs
CombinedParsers.AbstractParser
CombinedParsers.ConstantParser
CombinedParsers.NIndexParser
CombinedParsers.WrappedParser
CombinedParsers.LookAround
CombinedParsers._iterate
```
