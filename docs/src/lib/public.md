# Public Documentation

Documentation for `CombinedParsers.jl`'s public interface.

See the Internals section of the manual for internal package docs covering all submodules.

## Matching
```@docs
CombinedParsers
match
CombinedParsers.MatchesIterator
ParseMatch
(==)(::RegexMatch,::ParseMatch)
getindex(::ParseMatch{<:Any,<:CombinedParsers.Regexp.SequenceWithCaptures,<:Any},::Integer)
getproperty(::ParseMatch{<:Any,<:CombinedParsers.Regexp.SequenceWithCaptures,<:Any},::Symbol)
```

### Parsing

```@docs
get
parse
tryparse
tryparse_pos
```

## Typed Transformation Parsers

```@docs
CombinedParsers.state_type
map
map_at
JoinSubstring
CombinedParsers.Constant
(!)(::CombinedParsers.AbstractToken)
CombinedParsers.result_type
```

## Parser Constructors
```@docs
parser
convert
CombinedParsers.Regexp.@re_str
```

### useful importable sub-parsers
```@repl
using CombinedParsers.Regexp
import CombinedParsers.Regexp.whitespace_horizontal
import CombinedParsers.Regexp.whitespace_maybe
import CombinedParsers.Regexp.word
```

```@docs
CombinedParsers.Regexp.integer_base
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
CombinedParsers.either_types
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
(|)(::Either, ::Type)
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

