# Getting Started

## Installation
Install with
```julia
] add https://github.com/gkappler/CombinedParsers.jl
```

## Regular expression syntax
CombinedParsers.jl provides the `@re_str` macro as a plug-in replacement for the base Julia `@r_str` macro.

Base Julia PCRE regular expressions:
```@repl
pattern = r"reg(ular )?ex(?<a>p(ression)?)?\??"i
mr = match(pattern,"regexp")
```

CombinedParsers.Regexp regular expression:
```@setup session
using CombinedParsers
using CombinedParsers.Regexp
```

```@repl session
pattern = re"reg(ular )?ex(?<a>p(ression)?)?\??"i
mre = match(pattern,"regexp")

# The ParseMatch type has `getproperty` and `getindex` methods for handling like `RegexMatch`.
mre.match
mre.captures
mre[2]
mre[:a]
```

!!! note 
    The `@re_str` supports the following PCRE features
    - ☑ fundamentals: sequences, alternations, repetitions optional, matches (`*`,`+`,`{n}`, `{min,}`, `{min,max}`, `?`)
    - ☑ escaped characters and generic character types
    - ☑ character ranges (`[]`)
    - ☑ non-capturing groups,
    - ☑ capturing groups, backreferences, subroutines (all by index, relative index and name)
    - ☑ atomic groups
    - ☑ lazy repetitions
    - ☑ conditional expressions
    - ☑ internal and pattern options setting
    - ☑ simple assertions (`\A`, `\z`, `\Z`, `\b`, `\B`, `^`, `$`), 
    - ☑ lookaheads and lookbehinds
    - ☑ comments
    
    PCRE functionality that is currently not supported:
    - ☐ capture groups in lookbehinds.
    - ☐ ACCEPT, SKIP, COMMIT, THEN, PRUNE, \K


## Parsing 

`match` searches for the first match of the Regex in the String and return a `RegexMatch`/`Parsematch` object containing the match and captures, or nothing if the match failed.
`Base.parse` methods parse a String into a Julia type.
A CombinedParser `p` will parse into an instance of `result_type(p)`.
For parsers defined with the `@re_str` the `result_type`s are nested Tuples and Vectors of SubString, Chars and Missing.


```@repl session
p = re"(a)*bc?"
parse(p,"aaab")
```


## Iterating
If a parsing is not uniquely defined different parsings can be lazily iterated, conforming to Julia's `iterate` interface.
```@repl session
collect(parse_all(re"^(a|ab|b)+$","abab"))
for p in parse_all(re"^(a|ab|b)+$","abab")
	println(p)
end
```



## Transformations
Transform the result of a parsing with `map`.
The `result_type` inferred automatically using julia type inference.

```@repl session
p = map(length,re"(ab)*")
result_type(p)
parse(p,"abababab")
```

<!-- A supertype `T >: result_type(map(f,p))` can be set as `result_type` with `map(f, T, p)`. -->

Conveniently, calling `getindex(::AbstractParser,::Integer)` and `map(::Integer,::AbstractParser)` create a transforming parser selecting from the result of the parsing.
```@repl session
parse(map(2,re"abc"),"abc")
parse(re"abc"[2],"abc")
```
