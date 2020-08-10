

# Failed PCRE Tests
23 failed tests on 15 patterns.



## Skipped
12 patterns were skipped for the following reasons:


---


(no 69) skipped, because of very long compile time. The complex pattern parses email adresses

---


(no 70) skipped, because of very long compile time. The complex pattern parses email adresses

---

```
/^[W-c]+$/i
```

(no 89) skipped, CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.


---

```
/^[\x3f-\x5F]+$/i
```

(no 90) skipped, CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.


---


(no 135) skipped, escaped characters needs investigation.

---

```
/^(?:b|a(?=(.)))*\1/
```

(no 524) skipped, Captures in Lookaheads need refactoring. Low prio because instead of using backreference, `FlatMap` is recommended in `CombinedParsers.jl`.


---

```
/^((a|b)+)*ax/
```

(no 527) skipped, Capture backtracking needs refactoring. Low prio because instead of using captures, `parse` is recommended in `CombinedParsers.jl`.


---

```
/^((a|bc)+)*ax/
```

(no 528) skipped, Capture backtracking needs refactoring. Low prio because instead of using captures, `parse` is recommended in `CombinedParsers.jl`.


---

```
/(?<=\d{3}(?!999)...)foo/
```

(no 625) skipped, 
!!! note
    Lookahead in Lookbehind needs refactoring.


---

```
/(a+)*b/
```

(no 664) skipped, because of very long matching time for `Repeat(Repeat1('a'))` when there is no match. Avoid such patterns, contributions optimizing these cases are also very welcome.


---

```
/(?P<abn>(?P=abn)xxx|)+/
```

(no 887) skipped, do I misunderstand [recursive backreferences](https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19)?

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



```@repl session
parse(re"(\1xxx|)+","xxx")
match(r"(\1xxx|)+","xxx")
parse(re"(|\1xxx)+", "xxx")
parse(r"(|\1xxx)+", "xxx")
```




---


(no 1173) skipped, because of very long compile time. The complex pattern parses PCRE regex syntax.


## Failed tests


---

```
/(([a-c])b*?\2){3}/
```
(no 397) failed  0 of 1 times:
- ❌ `ababbbcbc`

---

```
/((\3|b)\2(a)){2,}/
```
(no 399) failed  0 of 1 times:
- ❌ `bbaababbabaaaaabbaaaabba`

---

```
/()?(?(1)b|a)/
```
(no 586) failed  0 of 1 times:
- ❌ `a`

---

```
/(?:(?(1)\1a|b)(X|Y))+/
```
(no 676) failed  0 of 2 times:
- ❌ `bXXaYYaY`
- ✅ `bXYaXXaX`

---

```
/^\Eabc/
```
(no 693) failed  0 of 0 times:

---

```
/^[a-\Ec]/
```
(no 695) failed  1 of 3 times:
- ❌ `b`
- ❌ `-` no match
- ✅ `E` no match

---

```
/^[a\E\E-\Ec]/
```
(no 696) failed  1 of 3 times:
- ❌ `b`
- ❌ `-` no match
- ✅ `E` no match

---

```
/^[\E\Qa\E-\Qz\E]+/
```
(no 697) failed  1 of 3 times:
- ❌ `b`
- ❌ `-` no match
- ✅ `` no match

---

```
/(?(R)a+|(?R)b)/
```
(no 918) failed  0 of 0 times:

---

```
/(?<=(?=.)?)/
```
(no 1189) failed  0 of 0 times:

---

```
/(?<=(?=.)?+)/
```
(no 1190) failed  0 of 0 times:

---

```
/(?<=(?=.)*)/
```
(no 1191) failed  0 of 0 times:

---

```
/(?<=(?=.){4,5})/
```
(no 1192) failed  0 of 0 times:

---

```
/(?<=(?=.){4,5}x)/
```
(no 1193) failed  0 of 0 times:

---

```
/^(?=.*(?=(([A-Z]).*(?(1)\1)))(?!.+\2)){26}/i
```
(no 1246) failed  5 of 6 times:
- ❌ `The quick brown fox jumps over the lazy dog.`
- ❌ `Jackdaws love my big sphinx of quartz.`
- ❌ `Pack my box with five dozen liquor jugs.`
- ❌ `The quick brown fox jumps over the lazy cat.` no match
- ❌ `Hackdaws love my big sphinx of quartz.` no match
- ❌ `Pack my fox with five dozen liquor jugs.` no match
