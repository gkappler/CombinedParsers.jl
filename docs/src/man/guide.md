# Overview
```@setup session
using CombinedParsers
using CombinedParsers.Regexp
```

## `ParseMatch`
CombinedParsers.jl provides the `@re_str` macro as a plug-in replacement for the base Julia `@r_str` macro.
Base Julia PCRE regular expressions:
```@repl
pattern = r"(?<a>a|B)+c"
mr = match(pattern,"aBc")
```

CombinedParsers.Regexp regular expression:
```@repl session
pattern = re"(?<a>a|B)+c"
mre = match(pattern,"aBc")
```

The ParseMatch type has `getproperty` and `getindex` methods for handling like `RegexMatch`.
```@repl session
mre.match
mre.captures
mre[1]
mre[:a]
```

!!! note
    CombinedParsers.jl is tested and benchmarked against the PCRE C library testset, see [compliance report](pcre-compliance.md).

## Parsing 

`match` searches for the first match of the Regex in the String and return a `RegexMatch`/`Parsematch` object containing the match and captures, or nothing if the match failed.
If a capture matches repeatedly only the last match is captured.
```@repl session
match(pattern,"aBBac")
```

`Base.parse` methods parse a String into a Julia type.
A CombinedParser `p` will parse into an instance of `result_type(p)`.
For parsers defined with the `@re_str` the `result_type`s are nested Tuples and Vectors of SubString, Chars and Missing.


```@repl session
parse(pattern,"aBBac")
```


## Iterating
If a parsing is not uniquely defined different parsings can be lazily iterated, conforming to Julia's `iterate` interface.
```@example session
for p in parse_all(re"^(a|ab|b)+$","abab")
	println(p)
end
```



## Performance
`CombinedParsers` are fast, utilizing parametric types and generated functions in the Julia compiler.

Compared with the Base.Regex (PCRE C implementation)
```@example session
using BenchmarkTools
pattern = r"[aB]+c";
@benchmark match(pattern,"aBaBc")
```
`CombinedParsers` are slightly faster in this case,
[and for many other tested parsers](pcre-compliance.md).
```@example session
pattern = re"[aB]+c";
@benchmark match(pattern,"aBaBc")
```

Matching Regex captures are supported for compatibility
```@example session
pattern = r"([aB])+c"
@benchmark match(pattern,"aBaBc")
```
`CombinedParsers.Regexp.Capture`s are slow compared with PCRE,
```@example session
pattern = re"([aB])+c";
@benchmark match(pattern,"aBaBc")
```

But with `CombinedParsers` you capture more flexibly with transformations anyway.
```@repl session
pattern = re"[aB]+c";
@btime (mre = match(pattern,"aBaBc"))
@btime get(mre)
```

## Transformations
Transform the result of a parsing with [`map`](@ref).
The [`result_type`](@ref) is inferred automatically using julia type inference.

```@repl session
p = map(length,re"(ab)*")
parse(p,"abababab")
```

Conveniently, calling `getindex(::CombinedParser,::Integer)` and `map(::Integer,::CombinedParser)` create a transforming parser selecting from the result of the parsing.
```@repl session
parse(map(IndexAt(2),re"abc"),"abc")
parse(re"abc"[2],"abc")
```

Next: The [User guide](user.md) provides a summary of CombinedParsers types.
