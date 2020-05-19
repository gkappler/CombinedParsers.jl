
# Basics
The simplest parser matches a `String` or `Char` iterator.
```julia
parse_a = parser("aa")

parse(parse_a,"aa")
# "aa"

parse(parse_a,"ab")
# ArgumentError: expected re"a" in "ab" at index 2 (todo!)
```



## Character Sets
```julia
parse(CharIn('a':'z'),"c") =='c'
parse(CharIn(isuppercase),"A") =='A'
parse(CharNotIn('a':'z'),"A") =='A'
parse(CharNotIn(isuppercase),"c") =='c'
```

## Sequence
Several parsers can be combined with the `Sequence` constructor and the `*` operator.
The `result_type` of a `Sequence` is the Tuple of the `result_type`s of its parts.
```julia
parse(Sequence(CharIn(isuppercase) * CharIn(islowercase)),"Ab") == ('A','b')
parse(CharIn(isuppercase) * CharIn(islowercase),"Ab") == ('A','b')
```

`getindex` on a sequence creates a transforming parser selecting from the result of the parsing.

Sequence keyword argument constructors transform the parsing into a named tuple.
```julia
parse(Sequence(first = CharIn(isuppercase), second = CharIn(islowercase)),"Ab") == 
	(first='A',second='b')
```

If some Sequence arguments are <:`Pair{Symbol}`, only those are retained in a NamedTuple.
```julia
parse(Sequence(CharIn(isuppercase), :second => CharIn(islowercase)),"Ab") == 
	(second='b',)
```



## Either
The `|` operator and constructor `Either` try matching the provided parsers in order, accepting the first match, and fails if all parsers fail.

```julia
parse(("a"|"ab"),"ab")
# "a"
```

Feedback inquiry:

## Assertions
Parsers that do not advance the parsing position can be used to assert conditions during parsing.
## AtStart() and AtEnd()
The `AtStart()` only succeeds if at the start of the input, and similarly the `AtEnd()` succeeds only at the end of the input.
By default, `parse` does not need to consume the full input but succeeds with the first match.
With `AtEnd()` the parser can be forced to consume the full input or fail otherwise.
```julia
parse(("a"|"ab")*AtEnd(),"ab")
# "ab"
```

## Looking around
A `Lookaround` parser wraps a parser `p`, succeeds if `p` matches without advancing the position, and fails if `p` fails.


The `@re_str` macro has a regex parser for lookahead and lookbehind expressions (simplified):
```julia
@with_names lookahead=Sequence(
    2,
    "(?",
    Either(Sequence(v->PositiveLookahead(v[2]), "=", alternation),
           Sequence(v->NegativeLookahead(v[2]), "!", alternation)),
           Sequence(v->PositiveLookbehind(v[2]), "<=", alternation)),
           Sequence(v->NegativeLookbehind(v[2]), "<!", alternation)),
    ")");
```

## Repeat
The `Repeat(p)` constructor creates a new parser repeating its argument zero or more times, and by default transforming to
`Vector{result_type(p)}`.
Repeating a specified number of times can be achieved with `Repeat(p,min=1,max=2)`, or `Repeat(1,p)` or `Repeat(1,2,p)`.
Equivalently the `^` operator can be used similar as for String, e.g. `p^2`, 
and like in regular expressions `p^(+)`, `p^(*)`.

```julia
parse(join(parser('a')^(*)," "),"a a") == 
	['a','a']
```

## Optional
Similar to Repeat, `Optional(p)` creates a parser, repeating 0 or 1 times. 
The `result_type(Optional(p, default=d))` is `promote_type` (or `Union` type is type promotion results in Any).

```julia
option = Optional('a') * join(Repeat('b'),"-")
```

Feedback appreciated:

```julia
option = ( CharIn('a':'z') | missing ) * join(Repeat('b'),"-")
```


## Lazy repetitions and optional parsers
Repetition and optional parsers are greedy by default, and can be switched to lazy matching by wrapping in `Lazy(Repeat(p))`.




## Atomic groups
Backtracking of a parser `p` can be prevented by wrapping in `Atomic(Repeat(p))`.
An atomic parser fails if `p` fails or if the first successfull parsing with `p` leads to a failing later in the parsing process.


```julia
parse(Either("a","ab","ac")*AtEnd(),"ab") == ("ab", AtEnd())
parse(Atomic(Either("a","ab","ac"))*AtEnd(),"ab") # fails
```

