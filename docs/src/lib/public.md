# Public Documentation

Documentation for `CombinedParsers.jl`'s public interface.

See the Internals section of the manual for internal package docs covering all submodules.

## Matching
```@docs
CombinedParsers
match
ParseMatch
Base.iterate
match_all
```

### Parsing

```@docs
get
parse
tryparse
tryparse_pos
parse_all
```

## Typed Transformation Parsers
```@docs
(!)
JoinSubstring
map
MatchRange
CombinedParsers.Constant
CombinedParsers.IndexAt
CombinedParsers.result_type
CombinedParsers.infer_result_type
CombinedParsers.state_type
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
Bytes
AnyChar
CharIn
CharNotIn
Base.broadcasted
```

## other
```@docs
CombinedParsers.WithMemory
CombinedParsers.MappedChars
```

### Sequences
```@docs
Sequence
(*)(::CombinedParsers.AbstractToken, ::CombinedParsers.AbstractToken)
sSequence
CombinedParsers.@seq
```

### Either
```@docs
Either
(|)(::CombinedParsers.AbstractToken, ::CombinedParsers.AbstractToken)
(|)(::Union{Char, Regex, AbstractString, CombinedParsers.AbstractToken}, ::Union{Char, Regex, AbstractString, CombinedParsers.AbstractToken})
(|)(::Either, ::Type)
push!
pushfirst!
sEither
CombinedParsers.either_result_type
```


### Combining Parser
```@docs
Atomic
CombinedParsers.FlatMap
after
```

### Repeating
```@docs
Repeat
Lazy
Optional
(|)(::CombinedParsers.AbstractToken{T}, ::Union{T,Missing}) where { T }
Repeat1
Repeat_stop
Repeat_until
Base.join
```

### Logging and Side-Effects
```@docs
@syntax
CombinedParsers.NamedParser
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
```

#### Look behind
```@docs
(/)(::CombinedParsers.ParserTypes, ::CombinedParsers.ParserTypes)
Lookbehind
PositiveLookbehind
NegativeLookbehind
CombinedParsers.reverse_index
```

#### Look ahead
```@docs
Lookahead
PositiveLookahead
NegativeLookahead
```

