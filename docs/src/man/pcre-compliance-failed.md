
# Failed PCRE Tests
41 failed tests on 27 patterns.


## Skipped
13 patterns were skipped for the following reasons:


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

```
/(abc)\1/i
```

(no 104) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


---


(no 135) skipped, escaped characters needs investigation.

---

```
/((?i)blah)\s+(?i:\1)/
```

(no 260) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


---

```
/((?i)blah)\s+(?m)A(?i:\1)/
```

(no 261) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


---

```
/(ab)\d\1/i
```

(no 562) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


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




---

```
/(Z)(a)\2{1,2}?(?-i)\1X/i
```

(no 1146) skipped, case-ignoring backreferences still need to be done, contributions are very welcome,


---


(no 1173) skipped, because of very long compile time. The complex pattern parses PCRE regex syntax.

## Failed tests


---

```
/a(?)b/
```
(no 140) failed  0 of 0 times:

---

```
/a\\b/
```
(no 344) failed  0 of 1 times:
- ✕ `a\\b`

---

```
/(([a-c])b*?\2){3}/
```
(no 397) failed  0 of 1 times:
- ✕ `ababbbcbc`

---

```
/((\3|b)\2(a)){2,}/
```
(no 399) failed  0 of 1 times:
- ✕ `bbaababbabaaaaabbaaaabba`

---

```
/a\\b/i
```
(no 442) failed  1 of 2 times:
- ✕ `A\\b`
- ✕ `a\\B`

---

```
/^(?:b|a(?=(.)))*\1/
```
(no 524) failed  0 of 1 times:
- ✕ `abc`

---

```
/^((a|b)+)*ax/
```
(no 527) failed  0 of 1 times:
- ✕ `aax`

---

```
/^((a|bc)+)*ax/
```
(no 528) failed  0 of 1 times:
- ✕ `aax`

---

```
/()?(?(1)b|a)/
```
(no 586) failed  0 of 1 times:
- ✕ `a`

---

```
/(?<=\d{3}(?!999)...)foo/
```
(no 625) failed  0 of 4 times:
- ✓ `123abcfoo`
- ✓ `123456foo`
- ✕ `123999foo` no match
- ✓ `` no match

---

```
/\Qabc\$xyz\E/
```
(no 650) failed  0 of 1 times:
- ✕ `abc\\$xyz`

---

```
/(?:(?(1)\1a|b)(X|Y))+/
```
(no 676) failed  0 of 2 times:
- ✕ `bXXaYYaY`
- ✓ `bXYaXXaX`

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
- ✕ `b`
- ✕ `-` no match
- ✓ `E` no match

---

```
/^[a\E\E-\Ec]/
```
(no 696) failed  1 of 3 times:
- ✕ `b`
- ✕ `-` no match
- ✓ `E` no match

---

```
/^[\E\Qa\E-\Qz\E]+/
```
(no 697) failed  1 of 3 times:
- ✕ `b`
- ✕ `-` no match
- ✓ `` no match

---

```
/^(?:((.)(?1)\2|)|((.)(?3)\4|.))$/i
```
(no 827) failed  2 of 5 times:
- ✓ `1221`
- ✕ `Satanoscillatemymetallicsonatas`
- ✕ `AmanaplanacanalPanama`
- ✕ `AblewasIereIsawElba`
- ✓ `Thequickbrownfox` no match

---

```
/^\W*+(?:((.)\W*+(?1)\W*+\2|)|((.)\W*+(?3)\W*+\4|\W*+.\W*+))\W*+$/i
```
(no 840) failed  2 of 5 times:
- ✓ `1221`
- ✕ `Satan, oscillate my metallic sonatas!`
- ✕ `A man, a plan, a canal: Panama!`
- ✕ `Able was I ere I saw Elba.`
- ✓ `The quick brown fox` no match

---

```
/(?(R)a+|(?R)b)/
```
(no 918) failed  0 of 0 times:

---

```
/(?(R)a+|((?R))b)/
```
(no 919) failed  0 of 0 times:

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
- ✕ `The quick brown fox jumps over the lazy dog.`
- ✕ `Jackdaws love my big sphinx of quartz.`
- ✕ `Pack my box with five dozen liquor jugs.`
- ✕ `The quick brown fox jumps over the lazy cat.` no match
- ✕ `Hackdaws love my big sphinx of quartz.` no match
- ✕ `Pack my fox with five dozen liquor jugs.` no match

---

```
/^(?>.*?([A-Z])(?!.*\1)){26}/i
```
(no 1247) failed  1 of 6 times:
- ✕ `The quick brown fox jumps over the lazy dog.`
- ✓ `Jackdaws love my big sphinx of quartz.`
- ✓ `Pack my box with five dozen liquor jugs.`
- ✓ `The quick brown fox jumps over the lazy cat.` no match
- ✕ `Hackdaws love my big sphinx of quartz.` no match
- ✓ `Pack my fox with five dozen liquor jugs.` no match
