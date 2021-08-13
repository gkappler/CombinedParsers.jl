# Constructing Parsers

## Character Matchers
```@docs
AnyChar
Bytes
CombinedParsers.ValueMatcher
CharIn
CombinedParsers.unicode_classes
CharNotIn
CombinedParsers.ismatch
CombinedParsers._ismatch
Base.broadcasted
```

## Repeating
```@docs
Repeat
(|)(::CombinedParsers.AbstractToken{T}, ::Union{T,Missing}) where { T }
Repeat1
Optional
CombinedParsers.defaultvalue
CombinedParsers._copy
Lazy
Repeat_stop
Repeat_until
Base.join
```

## Atomic
```@docs
Atomic
```

## Sequences
```@docs
Sequence
(*)(::CombinedParsers.AbstractToken, ::CombinedParsers.AbstractToken)
sSequence
CombinedParsers.@seq
CombinedParsers.sequence_result_type
CombinedParsers.sequence_state_type
```


## Recursive Parsers with [`Either`](@ref)
```@docs
Delayed
Either
@syntax
substitute
Base.push!
Base.pushfirst!
```


```@docs
(|)(::CombinedParser, ::CombinedParser)
sEither
CombinedParsers.either_result_type
```

## Parser generating parsers
```@docs
CombinedParsers.FlatMap
after
```

## Assertions
```@docs
AtStart
AtEnd
Always
Never
```

### Look behind
```@docs
Lookbehind
PositiveLookbehind
NegativeLookbehind
```

### Look ahead
```@docs
Lookahead
PositiveLookahead
NegativeLookahead
```

## Logging and Side-Effects
```@docs
CombinedParsers.NamedParser
with_name
@with_names
log_names
log_parser
with_log
with_effect
```


## other
```@docs
CombinedParsers.MappedSequenceParser
CombinedParsers.MemoizingParser
CombinedParsers.WithMemory
```
