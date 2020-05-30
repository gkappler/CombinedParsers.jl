# Public Documentation

Documentation for `CombinedParsers.jl`'s public interface.

See the Internals section of the manual for internal package docs covering all submodules.

## CombinedParsers

```@docs
CombinedParsers
parser
convert
```

## CombinedParsers.Regexp

```@docs
CombinedParsers.Regexp
CombinedParsers.Regexp.@re_str
CombinedParsers.Regexp.match
with_options
CombinedParsers.Regexp.parse_options
regex_escape
```

## Parsing, Logging and Side-Effects
```@docs
parse
tryparse
with_name
@with_names
log_names
with_log
instrument
with_effect
```

## Transformations
```@docs
map
map_at
JoinSubstring
(!)(::CombinedParsers.AbstractToken)
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
alternate
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

## Printing
```@docs
regex_string
CombinedParsers.regex_prefix
CombinedParsers.regex_inner
CombinedParsers.regex_suffix
CombinedParsers.print_constructor
CombinedParsers.MemoTreeChildren
```

