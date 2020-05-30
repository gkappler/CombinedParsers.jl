# Compliance with the PCRE test set
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
```@setup session
using CombinedParsers
using CombinedParsers.Regexp
```
The test set is downloaded from [the PCRE source repository](https://github.com/rurban/pcre/blob/master/testdata/testoutput1).
The PCRE test output is parsed with [a `CombinedParser`](https://github.com/gkappler/CombinedParsers.jl/blob/master/test/pcretest-parser.jl).
## Test Overview
3034 successful tests on 1252 patterns
(See [list of compliant patterns](pcre-compliance-succeeded.html)).

58 failed tests on 43 patterns
(See [list of failed patterns](pcre-compliance-failed.html)).
### Performance Overview:
CombinedParsers is a very young package that will be optimized further, 
but already `@re_str` pure Julia regular expression parsing is competitive with `@r_str` with the PCRE C backend which has arrived at a widely optimized codebase after decades of improvements.

Benchmark timings for regular expression construction and matching comparing `Regex` (x axis) and `Regcomb` (y axis), both on a log10 scale:


![](log_btimes.png)


Points represent PCRE an individual benchmark. 
PCRE benchmarks have a range between 82ns to 518ns.
CombinedParsers benchmarks have a ~20-times larger range between 33ns to 140μs.
### Benchmark ratios histogram:
![](log_btime_ratio_histogram.svg)



The equal are histograms of log10-ratios of `time_Regcomb/time_Regex` shows that `CombinedParser` implementation is competitive, with speeds up to 10x faster in best cases and only rarely 100x slower in worst cases.
Worst cases are investigated for improving [in this IJulia notebook](...).

## Unsupported
251 unsupported patterns were omitted for the following reasons:

- `ACCEPT` failed on 16 patterns.
- `COMMIT` failed on 35 patterns.
- `PRUNE` failed on 18 patterns.
- `SKIP` failed on 29 patterns.
- `THEN` failed on 50 patterns.
- `\K` failed on 13 patterns.
- `checking for pattern recursion` failed on 5 patterns.
- `options aftertext` failed on 12 patterns.
- `options g` failed on 5 patterns.
- `options g,aftertext` failed on 7 patterns.
- `options g,dupnames` failed on 5 patterns.
- `options gm` failed on 2 patterns.
- `options imsx,mark` failed on 2 patterns.
- `options mark` failed on 38 patterns.
- `options mark,no_start_optimize` failed on 1 patterns.
- `options x,mark` failed on 13 patterns.

## Skipped
14 patterns were skipped for the following reasons:


---
```
/^\�/
```
(#14) skipped, unicode escape in test parser needs to support "\u81".



(#69) skipped, because of very long compile time. The complex pattern parses email adresses


(#70) skipped, because of very long compile time. The complex pattern parses email adresses

---
```
/^[W-c]+$/i
```
(#89) skipped, CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.


---
```
/^[\x3f-\x5F]+$/i
```
(#90) skipped, CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.


---
```
/(abc)\1/i
```
(#104) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,



(#135) skipped, escaped characters needs investigation.

---
```
/((?i)blah)\s+(?i:\1)/
```
(#260) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


---
```
/((?i)blah)\s+(?m)A(?i:\1)/
```
(#261) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


---
```
/(ab)\d\1/i
```
(#562) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


---
```
/(a+)*b/
```
(#664) skipped, because of very long matching time for `Repeat(Repeat1('a'))` when there is no match. Avoid such patterns, contributions optimizing these cases are also very welcome.


---
```
/(?P<abn>(?P=abn)xxx|)+/
```
(#887) skipped, do I misunderstand [recursive backreferences](https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19)?

```@repl session
pattern = "(?P<abn>(?P=abn)xxx|)+"
match(Regex(pattern),"xxx")
match(Regcomb(pattern),"xxx")
```

IMO the pcre behaviour is confusing after considering the logic of expanding the repeat
```@repl session
pattern = "(?P<abn>(?P=abn)xxx|)((?P=abn)xxx|)"
match(Regex(pattern),"xxx")
match(Regcomb(pattern),"xxx")
```




---
```
/(Z)(a)\2{1,2}?(?-i)\1X/i
```
(#1146) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,



(#1173) skipped, because of very long compile time. The complex pattern parses PCRE regex syntax.
