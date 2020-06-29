# Public Documentation

Documentation for `CombinedParsers.jl`'s public interface.

See the Internals section of the manual for internal package docs covering all submodules.

## Matching
```@docs
CombinedParsers
match
get
parse
tryparse
tryparse_pos
```

## CombinedParsers.Regexp
```@docs
CombinedParsers.Regexp
CombinedParsers.Regexp.@re_str
with_options
CombinedParsers.Regexp.@pcre_tests
```


## Transformations
```@docs
map
map_at
JoinSubstring
(!)(::CombinedParsers.AbstractToken)
```

## Parser Constructors

```@docs
parser
convert
```

### Character Matchers
```@docs
AnyChar
CharIn
CharNotIn
```

### Combining Parser
```@docs
@syntax
Either
(|)(::CombinedParsers.ParserTypes, ::CombinedParsers.ParserTypes)
push!
pushfirst!
sEither
Sequence
sSequence
Atomic
```

### Repeating
```@docs
Optional
(|)(::CombinedParsers.AbstractToken{T}, ::Union{T,Missing}) where { T }
Repeat
Lazy
Repeat1
Repeat_stop
Repeat_until
Base.join
```

### Logging and Side-Effects
```@docs
with_name
@with_names
log_names
with_log
with_effect
```

### Assertions
```@docs
AtStart
AtEnd
Always
Never
Lookahead
PositiveLookahead
NegativeLookahead
Lookbehind
PositiveLookbehind
NegativeLookbehind
```

