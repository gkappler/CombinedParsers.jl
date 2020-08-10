# PCRE Compliance
3074 successful tests on 973 patterns.



---

```
/the quick brown fox/
```
(no 1) succeeded 4 of 4 times:

- ✅ `the quick brown fox`
- ✅ `What do you know about the quick brown fox?`
- ✅ `The quick brown FOX` no match
- ✅ `What do you know about THE QUICK BROWN FOX?` no match

---

```
/The quick brown fox/i
```
(no 2) succeeded 4 of 4 times:

- ✅ `the quick brown fox`
- ✅ `The quick brown FOX`
- ✅ `What do you know about the quick brown fox?`
- ✅ `What do you know about THE QUICK BROWN FOX?`

---

```
/abcd\t\n\r\f\a\e\071\x3b\$\\\?caxyz/
```
(no 3) succeeded 1 of 1 times:

- ✅ `abcd\t\n\r\f\a\e9;$\\?caxyz`

---

```
/a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz/
```
(no 4) succeeded 36 of 36 times:

- ✅ `abxyzpqrrrabbxyyyypqAzz`
- ✅ `abxyzpqrrrabbxyyyypqAzz`
- ✅ `aabxyzpqrrrabbxyyyypqAzz`
- ✅ `aaabxyzpqrrrabbxyyyypqAzz`
- ✅ `aaaabxyzpqrrrabbxyyyypqAzz`
- ✅ `abcxyzpqrrrabbxyyyypqAzz`
- ✅ `aabcxyzpqrrrabbxyyyypqAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypqAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypqqAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypqqqAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypqqqqAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypqqqqqAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypqqqqqqAzz`
- ✅ `aaaabcxyzpqrrrabbxyyyypqAzz`
- ✅ `abxyzzpqrrrabbxyyyypqAzz`
- ✅ `aabxyzzzpqrrrabbxyyyypqAzz`
- ✅ `aaabxyzzzzpqrrrabbxyyyypqAzz`
- ✅ `aaaabxyzzzzpqrrrabbxyyyypqAzz`
- ✅ `abcxyzzpqrrrabbxyyyypqAzz`
- ✅ `aabcxyzzzpqrrrabbxyyyypqAzz`
- ✅ `aaabcxyzzzzpqrrrabbxyyyypqAzz`
- ✅ `aaaabcxyzzzzpqrrrabbxyyyypqAzz`
- ✅ `aaaabcxyzzzzpqrrrabbbxyyyypqAzz`
- ✅ `aaaabcxyzzzzpqrrrabbbxyyyyypqAzz`
- ✅ `aaabcxyzpqrrrabbxyyyypABzz`
- ✅ `aaabcxyzpqrrrabbxyyyypABBzz`
- ✅ `>>>aaabxyzpqrrrabbxyyyypqAzz`
- ✅ `>aaaabxyzpqrrrabbxyyyypqAzz`
- ✅ `>>>>abcxyzpqrrrabbxyyyypqAzz`
- ✅ `abxyzpqrrabbxyyyypqAzz` no match
- ✅ `abxyzpqrrrrabbxyyyypqAzz` no match
- ✅ `abxyzpqrrrabxyyyypqAzz` no match
- ✅ `aaaabcxyzzzzpqrrrabbbxyyyyyypqAzz` no match
- ✅ `aaaabcxyzzzzpqrrrabbbxyyypqAzz` no match
- ✅ `aaabcxyzpqrrrabbxyyyypqqqqqqqAzz` no match

---

```
/^(abc){1,2}zz/
```
(no 5) succeeded 5 of 5 times:

- ✅ `abczz`
- ✅ `abcabczz`
- ✅ `zz` no match
- ✅ `abcabcabczz` no match
- ✅ `>>abczz` no match

---

```
/^(b+?|a){1,2}?c/
```
(no 6) succeeded 10 of 10 times:

- ✅ `bc`
- ✅ `bbc`
- ✅ `bbbc`
- ✅ `bac`
- ✅ `bbac`
- ✅ `aac`
- ✅ `abbbbbbbbbbbc`
- ✅ `bbbbbbbbbbbac`
- ✅ `aaac` no match
- ✅ `abbbbbbbbbbbac` no match

---

```
/^(b+|a){1,2}c/
```
(no 7) succeeded 10 of 10 times:

- ✅ `bc`
- ✅ `bbc`
- ✅ `bbbc`
- ✅ `bac`
- ✅ `bbac`
- ✅ `aac`
- ✅ `abbbbbbbbbbbc`
- ✅ `bbbbbbbbbbbac`
- ✅ `aaac` no match
- ✅ `abbbbbbbbbbbac` no match

---

```
/^(ba|b*){1,2}?bc/
```
(no 8) succeeded 5 of 5 times:

- ✅ `babc`
- ✅ `bbabc`
- ✅ `bababc`
- ✅ `bababbc` no match
- ✅ `babababc` no match

---

```
/^\ca\cA\c[;\c:/
```
(no 9) succeeded 1 of 1 times:

- ✅ `\x01\x01\e;z`

---

```
/^[ab\]cde]/
```
(no 10) succeeded 9 of 9 times:

- ✅ `athing`
- ✅ `bthing`
- ✅ `]thing`
- ✅ `cthing`
- ✅ `dthing`
- ✅ `ething`
- ✅ `fthing` no match
- ✅ `[thing` no match
- ✅ `\\thing` no match

---

```
/^[]cde]/
```
(no 11) succeeded 6 of 6 times:

- ✅ `]thing`
- ✅ `cthing`
- ✅ `dthing`
- ✅ `ething`
- ✅ `athing` no match
- ✅ `fthing` no match

---

```
/^[^ab\]cde]/
```
(no 12) succeeded 9 of 9 times:

- ✅ `fthing`
- ✅ `[thing`
- ✅ `\\thing`
- ✅ `athing` no match
- ✅ `bthing` no match
- ✅ `]thing` no match
- ✅ `cthing` no match
- ✅ `dthing` no match
- ✅ `ething` no match

---

```
/^[^]cde]/
```
(no 13) succeeded 6 of 6 times:

- ✅ `athing`
- ✅ `fthing`
- ✅ `]thing` no match
- ✅ `cthing` no match
- ✅ `dthing` no match
- ✅ `ething` no match

---

```
/^\201/
```
(no 14) succeeded 1 of 1 times:

- ✅ `\u81`

---

```
/^\377/
```
(no 15) succeeded 1 of 1 times:

- ✅ `ÿ`

---

```
/^[0-9]+$/
```
(no 16) succeeded 13 of 13 times:

- ✅ `0`
- ✅ `1`
- ✅ `2`
- ✅ `3`
- ✅ `4`
- ✅ `5`
- ✅ `6`
- ✅ `7`
- ✅ `8`
- ✅ `9`
- ✅ `10`
- ✅ `100`
- ✅ `abc` no match

---

```
/^.*nter/
```
(no 17) succeeded 3 of 3 times:

- ✅ `enter`
- ✅ `inter`
- ✅ `uponter`

---

```
/^xxx[0-9]+$/
```
(no 18) succeeded 3 of 3 times:

- ✅ `xxx0`
- ✅ `xxx1234`
- ✅ `xxx` no match

---

```
/^.+[0-9][0-9][0-9]$/
```
(no 19) succeeded 5 of 5 times:

- ✅ `x123`
- ✅ `x1234`
- ✅ `xx123`
- ✅ `123456`
- ✅ `123` no match

---

```
/^.+?[0-9][0-9][0-9]$/
```
(no 20) succeeded 5 of 5 times:

- ✅ `x123`
- ✅ `x1234`
- ✅ `xx123`
- ✅ `123456`
- ✅ `123` no match

---

```
/^([^!]+)!(.+)=apquxz\.ixr\.zzz\.ac\.uk$/
```
(no 21) succeeded 5 of 5 times:

- ✅ `abc!pqr=apquxz.ixr.zzz.ac.uk`
- ✅ `!pqr=apquxz.ixr.zzz.ac.uk` no match
- ✅ `abc!=apquxz.ixr.zzz.ac.uk` no match
- ✅ `abc!pqr=apquxz:ixr.zzz.ac.uk` no match
- ✅ `abc!pqr=apquxz.ixr.zzz.ac.ukk` no match

---

```
/:/
```
(no 22) succeeded 2 of 2 times:

- ✅ `Well, we need a colon: somewhere`
- ✅ `Fail without a colon` no match

---

```
/([\da-f:]+)$/i
```
(no 23) succeeded 12 of 12 times:

- ✅ `0abc`
- ✅ `abc`
- ✅ `fed`
- ✅ `E`
- ✅ `::`
- ✅ `5f03:12C0::932e`
- ✅ `fed def`
- ✅ `Any old stuff`
- ✅ `0zzz` no match
- ✅ `gzzz` no match
- ✅ `fed ` no match
- ✅ `Any old rubbish` no match

---

```
/^.*\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/
```
(no 24) succeeded 5 of 5 times:

- ✅ `.1.2.3`
- ✅ `A.12.123.0`
- ✅ `.1.2.3333` no match
- ✅ `1.2.3` no match
- ✅ `1234.2.3` no match

---

```
/^(\d+)\s+IN\s+SOA\s+(\S+)\s+(\S+)\s*\(\s*$/
```
(no 25) succeeded 3 of 3 times:

- ✅ `1 IN SOA non-sp1 non-sp2(`
- ✅ `1    IN    SOA    non-sp1    non-sp2   (`
- ✅ `1IN SOA non-sp1 non-sp2(` no match

---

```
/^[a-zA-Z\d][a-zA-Z\d\-]*(\.[a-zA-Z\d][a-zA-z\d\-]*)*\.$/
```
(no 26) succeeded 7 of 7 times:

- ✅ `a.`
- ✅ `Z.`
- ✅ `2.`
- ✅ `ab-c.pq-r.`
- ✅ `sxk.zzz.ac.uk.`
- ✅ `x-.y-.`
- ✅ `-abc.peq.` no match

---

```
/^\*\.[a-z]([a-z\-\d]*[a-z\d]+)?(\.[a-z]([a-z\-\d]*[a-z\d]+)?)*$/
```
(no 27) succeeded 8 of 8 times:

- ✅ `*.a`
- ✅ `*.b0-a`
- ✅ `*.c3-b.c`
- ✅ `*.c-a.b-c`
- ✅ `*.0` no match
- ✅ `*.a-` no match
- ✅ `*.a-b.c-` no match
- ✅ `*.c-a.0-c` no match

---

```
/^(?=ab(de))(abd)(e)/
```
(no 28) succeeded 1 of 1 times:

- ✅ `abde`

---

```
/^(?!(ab)de|x)(abd)(f)/
```
(no 29) succeeded 1 of 1 times:

- ✅ `abdf`

---

```
/^(?=(ab(cd)))(ab)/
```
(no 30) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/^[\da-f](\.[\da-f])*$/i
```
(no 31) succeeded 3 of 3 times:

- ✅ `a.b.c.d`
- ✅ `A.B.C.D`
- ✅ `a.b.c.1.2.3.C`

---

```
/^\".*\"\s*(;.*)?$/
```
(no 32) succeeded 4 of 4 times:

- ✅ `\"1234\"`
- ✅ `\"abcd\" ;`
- ✅ `\"\" ; rhubarb`
- ✅ `\"1234\" : things` no match

---

```
/^$/
```
(no 33) succeeded 2 of 2 times:

- ✅ ``
- ✅ `A non-empty line` no match

---

```
/   ^    a   (?# begins with a)  b\sc (?# then b c) $ (?# then end)/x
```
(no 34) succeeded 3 of 3 times:

- ✅ `ab c`
- ✅ `abc` no match
- ✅ `ab cde` no match

---

```
/(?x)   ^    a   (?# begins with a)  b\sc (?# then b c) $ (?# then end)/
```
(no 35) succeeded 3 of 3 times:

- ✅ `ab c`
- ✅ `abc` no match
- ✅ `ab cde` no match

---

```
/^   a\ b[c ]d       $/x
```
(no 36) succeeded 4 of 4 times:

- ✅ `a bcd`
- ✅ `a b d`
- ✅ `abcd` no match
- ✅ `ab d` no match

---

```
/^(a(b(c)))(d(e(f)))(h(i(j)))(k(l(m)))$/
```
(no 37) succeeded 1 of 1 times:

- ✅ `abcdefhijklm`

---

```
/^(?:a(b(c)))(?:d(e(f)))(?:h(i(j)))(?:k(l(m)))$/
```
(no 38) succeeded 1 of 1 times:

- ✅ `abcdefhijklm`

---

```
/^[\w][\W][\s][\S][\d][\D][\b][\n][\c]][\022]/
```
(no 39) succeeded 1 of 1 times:

- ✅ `a+ Z0+\b\n\x1d\x12`

---

```
/^[.^$|()*+?{,}]+/
```
(no 40) succeeded 1 of 1 times:

- ✅ `.^$(*+)|{?,?}`

---

```
/^a*\w/
```
(no 41) succeeded 8 of 8 times:

- ✅ `z`
- ✅ `az`
- ✅ `aaaz`
- ✅ `a`
- ✅ `aa`
- ✅ `aaaa`
- ✅ `a+`
- ✅ `aa+`

---

```
/^a*?\w/
```
(no 42) succeeded 8 of 8 times:

- ✅ `z`
- ✅ `az`
- ✅ `aaaz`
- ✅ `a`
- ✅ `aa`
- ✅ `aaaa`
- ✅ `a+`
- ✅ `aa+`

---

```
/^a+\w/
```
(no 43) succeeded 5 of 5 times:

- ✅ `az`
- ✅ `aaaz`
- ✅ `aa`
- ✅ `aaaa`
- ✅ `aa+`

---

```
/^a+?\w/
```
(no 44) succeeded 5 of 5 times:

- ✅ `az`
- ✅ `aaaz`
- ✅ `aa`
- ✅ `aaaa`
- ✅ `aa+`

---

```
/^\d{8}\w{2,}/
```
(no 45) succeeded 4 of 4 times:

- ✅ `1234567890`
- ✅ `12345678ab`
- ✅ `12345678__`
- ✅ `1234567` no match

---

```
/^[aeiou\d]{4,5}$/
```
(no 46) succeeded 5 of 5 times:

- ✅ `uoie`
- ✅ `1234`
- ✅ `12345`
- ✅ `aaaaa`
- ✅ `123456` no match

---

```
/^[aeiou\d]{4,5}?/
```
(no 47) succeeded 5 of 5 times:

- ✅ `uoie`
- ✅ `1234`
- ✅ `12345`
- ✅ `aaaaa`
- ✅ `123456`

---

```
/\A(abc|def)=(\1){2,3}\Z/
```
(no 48) succeeded 3 of 3 times:

- ✅ `abc=abcabc`
- ✅ `def=defdefdef`
- ✅ `abc=defdef` no match

---

```
/^(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\11*(\3\4)\1(?#)2$/
```
(no 49) succeeded 2 of 2 times:

- ✅ `abcdefghijkcda2`
- ✅ `abcdefghijkkkkcda2`

---

```
/(cat(a(ract|tonic)|erpillar)) \1()2(3)/
```
(no 50) succeeded 3 of 3 times:

- ✅ `cataract cataract23`
- ✅ `catatonic catatonic23`
- ✅ `caterpillar caterpillar23`

---

```
/^From +([^ ]+) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]/
```
(no 51) succeeded 1 of 1 times:

- ✅ `From abcd  Mon Sep 01 12:33:02 1997`

---

```
/^From\s+\S+\s+([a-zA-Z]{3}\s+){2}\d{1,2}\s+\d\d:\d\d/
```
(no 52) succeeded 3 of 3 times:

- ✅ `From abcd  Mon Sep 01 12:33:02 1997`
- ✅ `From abcd  Mon Sep  1 12:33:02 1997`
- ✅ `From abcd  Sep 01 12:33:02 1997` no match

---

```
/^12.34/s
```
(no 53) succeeded 2 of 2 times:

- ✅ `12\n34`
- ✅ `12\r34`

---

```
/\w+(?=\t)/
```
(no 54) succeeded 1 of 1 times:

- ✅ `the quick brown\t fox`

---

```
/foo(?!bar)(.*)/
```
(no 55) succeeded 1 of 1 times:

- ✅ `foobar is foolish see?`

---

```
/(?:(?!foo)...|^.{0,2})bar(.*)/
```
(no 56) succeeded 4 of 4 times:

- ✅ `foobar crowbar etc`
- ✅ `barrel`
- ✅ `2barrel`
- ✅ `A barrel`

---

```
/^(\D*)(?=\d)(?!123)/
```
(no 57) succeeded 2 of 2 times:

- ✅ `abc456`
- ✅ `abc123` no match

---

```
/^1234(?# test newlines
  inside)/
```
(no 58) succeeded 1 of 1 times:

- ✅ `1234`

---

```
/^1234 #comment in extended re
  /x
```
(no 59) succeeded 1 of 1 times:

- ✅ `1234`

---

```
/#rhubarb
  abcd/x
```
(no 60) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/^abcd#rhubarb/x
```
(no 61) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/^(a)\1{2,3}(.)/
```
(no 62) succeeded 4 of 4 times:

- ✅ `aaab`
- ✅ `aaaab`
- ✅ `aaaaab`
- ✅ `aaaaaab`

---

```
/(?!^)abc/
```
(no 63) succeeded 2 of 2 times:

- ✅ `the abc`
- ✅ `abc` no match

---

```
/(?=^)abc/
```
(no 64) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ `the abc` no match

---

```
/^[ab]{1,3}(ab*|b)/
```
(no 65) succeeded 1 of 1 times:

- ✅ `aabbbbb`

---

```
/^[ab]{1,3}?(ab*|b)/
```
(no 66) succeeded 1 of 1 times:

- ✅ `aabbbbb`

---

```
/^[ab]{1,3}?(ab*?|b)/
```
(no 67) succeeded 1 of 1 times:

- ✅ `aabbbbb`

---

```
/^[ab]{1,3}(ab*?|b)/
```
(no 68) succeeded 1 of 1 times:

- ✅ `aabbbbb`

---

```
/abc\0def\00pqr\000xyz\0000AB/
```
(no 71) succeeded 2 of 2 times:

- ✅ `abc\0def\0pqr\0xyz\x000AB`
- ✅ `abc456 abc\0def\0pqr\0xyz\x000ABCDE`

---

```
/abc\x0def\x00pqr\x000xyz\x0000AB/
```
(no 72) succeeded 2 of 2 times:

- ✅ `abc\ref\0pqr\x000xyz\x0000AB`
- ✅ `abc456 abc\ref\0pqr\x000xyz\x0000ABCDE`

---

```
/^[\000-\037]/
```
(no 73) succeeded 3 of 3 times:

- ✅ `\0A`
- ✅ `\x01B`
- ✅ `\x1fC`

---

```
/\0*/
```
(no 74) succeeded 1 of 1 times:

- ✅ `\0\0\0\0`

---

```
/A\x0{2,3}Z/
```
(no 75) succeeded 4 of 4 times:

- ✅ `The A\0\0Z`
- ✅ `An A\0\0\0Z`
- ✅ `A\0Z` no match
- ✅ `A\0\0\0\0Z` no match

---

```
/^(cow|)\1(bell)/
```
(no 76) succeeded 3 of 3 times:

- ✅ `cowcowbell`
- ✅ `bell`
- ✅ `cowbell` no match

---

```
/^\s/
```
(no 77) succeeded 6 of 6 times:

- ✅ ` abc`
- ✅ `\fabc`
- ✅ `\nabc`
- ✅ `\rabc`
- ✅ `\tabc`
- ✅ `abc` no match

---

```
/^a	b
      c/x
```
(no 78) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/^(a|)\1*b/
```
(no 79) succeeded 4 of 4 times:

- ✅ `ab`
- ✅ `aaaab`
- ✅ `b`
- ✅ `acb` no match

---

```
/^(a|)\1+b/
```
(no 80) succeeded 4 of 4 times:

- ✅ `aab`
- ✅ `aaaab`
- ✅ `b`
- ✅ `ab` no match

---

```
/^(a|)\1?b/
```
(no 81) succeeded 4 of 4 times:

- ✅ `ab`
- ✅ `aab`
- ✅ `b`
- ✅ `acb` no match

---

```
/^(a|)\1{2}b/
```
(no 82) succeeded 5 of 5 times:

- ✅ `aaab`
- ✅ `b`
- ✅ `ab` no match
- ✅ `aab` no match
- ✅ `aaaab` no match

---

```
/^(a|)\1{2,3}b/
```
(no 83) succeeded 6 of 6 times:

- ✅ `aaab`
- ✅ `aaaab`
- ✅ `b`
- ✅ `ab` no match
- ✅ `aab` no match
- ✅ `aaaaab` no match

---

```
/ab{1,3}bc/
```
(no 84) succeeded 5 of 5 times:

- ✅ `abbbbc`
- ✅ `abbbc`
- ✅ `abbc`
- ✅ `abc` no match
- ✅ `abbbbbc` no match

---

```
/([^.]*)\.([^:]*):[T ]+(.*)/
```
(no 85) succeeded 1 of 1 times:

- ✅ `track1.title:TBlah blah blah`

---

```
/([^.]*)\.([^:]*):[T ]+(.*)/i
```
(no 86) succeeded 1 of 1 times:

- ✅ `track1.title:TBlah blah blah`

---

```
/([^.]*)\.([^:]*):[t ]+(.*)/i
```
(no 87) succeeded 1 of 1 times:

- ✅ `track1.title:TBlah blah blah`

---

```
/^[W-c]+$/
```
(no 88) succeeded 2 of 2 times:

- ✅ `WXY_^abc`
- ✅ `wxy` no match

---

```
/^abc$/m
```
(no 91) succeeded 4 of 4 times:

- ✅ `abc`
- ✅ `qqq\nabc`
- ✅ `abc\nzzz`
- ✅ `qqq\nabc\nzzz`

---

```
/^abc$/
```
(no 92) succeeded 4 of 4 times:

- ✅ `abc`
- ✅ `qqq\nabc` no match
- ✅ `abc\nzzz` no match
- ✅ `qqq\nabc\nzzz` no match

---

```
/\Aabc\Z/m
```
(no 93) succeeded 6 of 6 times:

- ✅ `abc`
- ✅ `abc\n`
- ✅ `qqq\nabc` no match
- ✅ `abc\nzzz` no match
- ✅ `qqq\nabc\nzzz` no match
- ✅ `` no match

---

```
/\A(.)*\Z/s
```
(no 94) succeeded 1 of 1 times:

- ✅ `abc\ndef`

---

```
/\A(.)*\Z/m
```
(no 95) succeeded 1 of 1 times:

- ✅ `abc\ndef` no match

---

```
/(?:b)|(?::+)/
```
(no 96) succeeded 2 of 2 times:

- ✅ `b::c`
- ✅ `c::b`

---

```
/[-az]+/
```
(no 97) succeeded 2 of 2 times:

- ✅ `az-`
- ✅ `b` no match

---

```
/[az-]+/
```
(no 98) succeeded 2 of 2 times:

- ✅ `za-`
- ✅ `b` no match

---

```
/[a\-z]+/
```
(no 99) succeeded 2 of 2 times:

- ✅ `a-z`
- ✅ `b` no match

---

```
/[a-z]+/
```
(no 100) succeeded 1 of 1 times:

- ✅ `abcdxyz`

---

```
/[\d-]+/
```
(no 101) succeeded 2 of 2 times:

- ✅ `12-34`
- ✅ `aaa` no match

---

```
/\x5c/
```
(no 102) succeeded 1 of 1 times:

- ✅ `\\`

---

```
/\x20Z/
```
(no 103) succeeded 2 of 2 times:

- ✅ `the Zoo`
- ✅ `Zulu` no match

---

```
/(abc)\1/i
```
(no 104) succeeded 3 of 3 times:

- ✅ `abcabc`
- ✅ `ABCabc`
- ✅ `abcABC`

---

```
/abc$/
```
(no 105) succeeded 3 of 3 times:

- ✅ `abc`
- ✅ `abc\n`
- ✅ `abc\ndef` no match

---

```
/(abc)\123/
```
(no 106) succeeded 1 of 1 times:

- ✅ `abcS`

---

```
/(abc)\223/
```
(no 107) succeeded 1 of 1 times:

- ✅ `abc\u93`

---

```
/(abc)\323/
```
(no 108) succeeded 1 of 1 times:

- ✅ `abcÓ`

---

```
/(abc)\100/
```
(no 109) succeeded 2 of 2 times:

- ✅ `abc@`
- ✅ `abc@`

---

```
/(abc)\1000/
```
(no 110) succeeded 7 of 7 times:

- ✅ `abc@0`
- ✅ `abc@0`
- ✅ `abc@0`
- ✅ `abc@0`
- ✅ `abc@0`
- ✅ `abc@0`
- ✅ ``

---

```
/^(A)(B)(C)(D)(E)(F)(G)(H)(I)\8\9$/
```
(no 111) succeeded 1 of 1 times:

- ✅ `ABCDEFGHIHI`

---

```
/^[A\8B\9C]+$/
```
(no 112) succeeded 2 of 2 times:

- ✅ `A8B9C`
- ✅ `A8B9C\0` no match

---

```
/(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\12\123/
```
(no 113) succeeded 1 of 1 times:

- ✅ `abcdefghijkllS`

---

```
/(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\12\123/
```
(no 114) succeeded 1 of 1 times:

- ✅ `abcdefghijk\nS`

---

```
/a{0}bc/
```
(no 115) succeeded 1 of 1 times:

- ✅ `bc`

---

```
/(a|(bc)){0,0}?xyz/
```
(no 116) succeeded 1 of 1 times:

- ✅ `xyz`

---

```
/abc[\10]de/
```
(no 117) succeeded 1 of 1 times:

- ✅ `abc\bde`

---

```
/abc[\1]de/
```
(no 118) succeeded 1 of 1 times:

- ✅ `abc\x01de`

---

```
/(abc)[\1]de/
```
(no 119) succeeded 1 of 1 times:

- ✅ `abc\x01de`

---

```
/(?s)a.b/
```
(no 120) succeeded 1 of 1 times:

- ✅ `a\nb`

---

```
/^([^a])([^\b])([^c]*)([^d]{3,4})/
```
(no 121) succeeded 7 of 7 times:

- ✅ `baNOTccccd`
- ✅ `baNOTcccd`
- ✅ `baNOTccd`
- ✅ `bacccd`
- ✅ `anything` no match
- ✅ `bbc` no match
- ✅ `baccd` no match

---

```
/[^a]/
```
(no 122) succeeded 2 of 2 times:

- ✅ `Abc`
- ✅ ``

---

```
/[^a]/i
```
(no 123) succeeded 1 of 1 times:

- ✅ `Abc`

---

```
/[^a]+/
```
(no 124) succeeded 2 of 2 times:

- ✅ `AAAaAbc`
- ✅ ``

---

```
/[^a]+/i
```
(no 125) succeeded 1 of 1 times:

- ✅ `AAAaAbc`

---

```
/[^a]+/
```
(no 126) succeeded 2 of 2 times:

- ✅ `bbb\nccc`
- ✅ ``

---

```
/[^k]$/
```
(no 127) succeeded 3 of 3 times:

- ✅ `abc`
- ✅ `abk` no match
- ✅ `` no match

---

```
/[^k]{2,3}$/
```
(no 128) succeeded 6 of 6 times:

- ✅ `abc`
- ✅ `kbc`
- ✅ `kabc`
- ✅ `abk` no match
- ✅ `akb` no match
- ✅ `akk` no match

---

```
/^\d{8,}\@.+[^k]$/
```
(no 129) succeeded 4 of 4 times:

- ✅ `12345678@a.b.c.d`
- ✅ `123456789@x.y.z`
- ✅ `12345678@x.y.uk` no match
- ✅ `1234567@a.b.c.d` no match

---

```
/(a)\1{8,}/
```
(no 130) succeeded 3 of 3 times:

- ✅ `aaaaaaaaa`
- ✅ `aaaaaaaaaa`
- ✅ `aaaaaaa` no match

---

```
/[^a]/
```
(no 131) succeeded 2 of 2 times:

- ✅ `aaaabcd`
- ✅ `aaAabcd`

---

```
/[^a]/i
```
(no 132) succeeded 2 of 2 times:

- ✅ `aaaabcd`
- ✅ `aaAabcd`

---

```
/[^az]/
```
(no 133) succeeded 2 of 2 times:

- ✅ `aaaabcd`
- ✅ `aaAabcd`

---

```
/[^az]/i
```
(no 134) succeeded 2 of 2 times:

- ✅ `aaaabcd`
- ✅ `aaAabcd`

---

```
/P[^*]TAIRE[^*]{1,6}?LL/
```
(no 136) succeeded 1 of 1 times:

- ✅ `xxxxxxxxxxxPSTAIREISLLxxxxxxxxx`

---

```
/P[^*]TAIRE[^*]{1,}?LL/
```
(no 137) succeeded 1 of 1 times:

- ✅ `xxxxxxxxxxxPSTAIREISLLxxxxxxxxx`

---

```
/(\.\d\d[1-9]?)\d+/
```
(no 138) succeeded 4 of 4 times:

- ✅ `1.230003938`
- ✅ `1.875000282`
- ✅ `1.235`
- ✅ ``

---

```
/(\.\d\d((?=0)|\d(?=\d)))/
```
(no 139) succeeded 4 of 4 times:

- ✅ `1.230003938`
- ✅ `1.875000282`
- ✅ `1.235` no match
- ✅ `` no match

---

```
/a(?)b/
```
(no 140) succeeded 2 of 2 times:

- ✅ `ab`
- ✅ ``

---

```
/\b(foo)\s+(\w+)/i
```
(no 141) succeeded 2 of 2 times:

- ✅ `Food is on the foo table`
- ✅ ``

---

```
/foo(.*)bar/
```
(no 142) succeeded 2 of 2 times:

- ✅ `The food is under the bar in the barn.`
- ✅ ``

---

```
/foo(.*?)bar/
```
(no 143) succeeded 1 of 1 times:

- ✅ `The food is under the bar in the barn.`

---

```
/(.*)(\d*)/
```
(no 144) succeeded 2 of 2 times:

- ✅ `I have 2 numbers: 53147`
- ✅ ``

---

```
/(.*)(\d+)/
```
(no 145) succeeded 2 of 2 times:

- ✅ `I have 2 numbers: 53147`
- ✅ ``

---

```
/(.*?)(\d*)/
```
(no 146) succeeded 1 of 1 times:

- ✅ `I have 2 numbers: 53147`

---

```
/(.*?)(\d+)/
```
(no 147) succeeded 1 of 1 times:

- ✅ `I have 2 numbers: 53147`

---

```
/(.*)(\d+)$/
```
(no 148) succeeded 1 of 1 times:

- ✅ `I have 2 numbers: 53147`

---

```
/(.*?)(\d+)$/
```
(no 149) succeeded 1 of 1 times:

- ✅ `I have 2 numbers: 53147`

---

```
/(.*)\b(\d+)$/
```
(no 150) succeeded 1 of 1 times:

- ✅ `I have 2 numbers: 53147`

---

```
/(.*\D)(\d+)$/
```
(no 151) succeeded 1 of 1 times:

- ✅ `I have 2 numbers: 53147`

---

```
/^\D*(?!123)/
```
(no 152) succeeded 2 of 2 times:

- ✅ `ABC123`
- ✅ ``

---

```
/^(\D*)(?=\d)(?!123)/
```
(no 153) succeeded 3 of 3 times:

- ✅ `ABC445`
- ✅ `ABC123` no match
- ✅ `` no match

---

```
/^[W-]46]/
```
(no 154) succeeded 8 of 8 times:

- ✅ `W46]789`
- ✅ `-46]789`
- ✅ `Wall` no match
- ✅ `Zebra` no match
- ✅ `42` no match
- ✅ `[abcd]` no match
- ✅ `]abcd[` no match
- ✅ `` no match

---

```
/^[W-\]46]/
```
(no 155) succeeded 11 of 11 times:

- ✅ `W46]789`
- ✅ `Wall`
- ✅ `Zebra`
- ✅ `Xylophone`
- ✅ `42`
- ✅ `[abcd]`
- ✅ `]abcd[`
- ✅ `\\backslash`
- ✅ `-46]789` no match
- ✅ `well` no match
- ✅ `` no match

---

```
/\d\d\/\d\d\/\d\d\d\d/
```
(no 156) succeeded 1 of 1 times:

- ✅ `01/01/2000`

---

```
/word (?:[a-zA-Z0-9]+ ){0,10}otherword/
```
(no 157) succeeded 2 of 2 times:

- ✅ `word cat dog elephant mussel cow horse canary baboon snake shark otherword`
- ✅ `word cat dog elephant mussel cow horse canary baboon snake shark` no match

---

```
/word (?:[a-zA-Z0-9]+ ){0,300}otherword/
```
(no 158) succeeded 1 of 1 times:

- ✅ `word cat dog elephant mussel cow horse canary baboon snake shark the quick brown fox and the lazy dog and several other words getting close to thirty by now I hope` no match

---

```
/^(a){0,0}/
```
(no 159) succeeded 3 of 3 times:

- ✅ `bcd`
- ✅ `abc`
- ✅ `aab`

---

```
/^(a){0,1}/
```
(no 160) succeeded 3 of 3 times:

- ✅ `bcd`
- ✅ `abc`
- ✅ `aab`

---

```
/^(a){0,2}/
```
(no 161) succeeded 3 of 3 times:

- ✅ `bcd`
- ✅ `abc`
- ✅ `aab`

---

```
/^(a){0,3}/
```
(no 162) succeeded 4 of 4 times:

- ✅ `bcd`
- ✅ `abc`
- ✅ `aab`
- ✅ `aaa`

---

```
/^(a){0,}/
```
(no 163) succeeded 5 of 5 times:

- ✅ `bcd`
- ✅ `abc`
- ✅ `aab`
- ✅ `aaa`
- ✅ `aaaaaaaa`

---

```
/^(a){1,1}/
```
(no 164) succeeded 3 of 3 times:

- ✅ `abc`
- ✅ `aab`
- ✅ `bcd` no match

---

```
/^(a){1,2}/
```
(no 165) succeeded 3 of 3 times:

- ✅ `abc`
- ✅ `aab`
- ✅ `bcd` no match

---

```
/^(a){1,3}/
```
(no 166) succeeded 4 of 4 times:

- ✅ `abc`
- ✅ `aab`
- ✅ `aaa`
- ✅ `bcd` no match

---

```
/^(a){1,}/
```
(no 167) succeeded 5 of 5 times:

- ✅ `abc`
- ✅ `aab`
- ✅ `aaa`
- ✅ `aaaaaaaa`
- ✅ `bcd` no match

---

```
/.*\.gif/
```
(no 168) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno`

---

```
/.{0,}\.gif/
```
(no 169) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno`

---

```
/.*\.gif/m
```
(no 170) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno`

---

```
/.*\.gif/s
```
(no 171) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno`

---

```
/.*\.gif/ms
```
(no 172) succeeded 2 of 2 times:

- ✅ `borfle\nbib.gif\nno`
- ✅ ``

---

```
/.*$/
```
(no 173) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno`

---

```
/.*$/m
```
(no 174) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno`

---

```
/.*$/s
```
(no 175) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno`

---

```
/.*$/ms
```
(no 176) succeeded 2 of 2 times:

- ✅ `borfle\nbib.gif\nno`
- ✅ ``

---

```
/.*$/
```
(no 177) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno\n`

---

```
/.*$/m
```
(no 178) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno\n`

---

```
/.*$/s
```
(no 179) succeeded 1 of 1 times:

- ✅ `borfle\nbib.gif\nno\n`

---

```
/.*$/ms
```
(no 180) succeeded 2 of 2 times:

- ✅ `borfle\nbib.gif\nno\n`
- ✅ ``

---

```
/(.*X|^B)/
```
(no 181) succeeded 3 of 3 times:

- ✅ `abcde\n1234Xyz`
- ✅ `BarFoo`
- ✅ `abcde\nBar` no match

---

```
/(.*X|^B)/m
```
(no 182) succeeded 3 of 3 times:

- ✅ `abcde\n1234Xyz`
- ✅ `BarFoo`
- ✅ `abcde\nBar`

---

```
/(.*X|^B)/s
```
(no 183) succeeded 3 of 3 times:

- ✅ `abcde\n1234Xyz`
- ✅ `BarFoo`
- ✅ `abcde\nBar` no match

---

```
/(.*X|^B)/ms
```
(no 184) succeeded 3 of 3 times:

- ✅ `abcde\n1234Xyz`
- ✅ `BarFoo`
- ✅ `abcde\nBar`

---

```
/(?s)(.*X|^B)/
```
(no 185) succeeded 3 of 3 times:

- ✅ `abcde\n1234Xyz`
- ✅ `BarFoo`
- ✅ `abcde\nBar` no match

---

```
/(?s:.*X|^B)/
```
(no 186) succeeded 3 of 3 times:

- ✅ `abcde\n1234Xyz`
- ✅ `BarFoo`
- ✅ `abcde\nBar` no match

---

```
/^.*B/
```
(no 187) succeeded 2 of 2 times:

- ✅ `abc\nB` no match
- ✅ `` no match

---

```
/(?s)^.*B/
```
(no 188) succeeded 1 of 1 times:

- ✅ `abc\nB`

---

```
/(?m)^.*B/
```
(no 189) succeeded 2 of 2 times:

- ✅ `abc\nB`
- ✅ ``

---

```
/(?ms)^.*B/
```
(no 190) succeeded 1 of 1 times:

- ✅ `abc\nB`

---

```
/(?ms)^B/
```
(no 191) succeeded 1 of 1 times:

- ✅ `abc\nB`

---

```
/(?s)B$/
```
(no 192) succeeded 1 of 1 times:

- ✅ `B\n`

---

```
/^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/
```
(no 193) succeeded 2 of 2 times:

- ✅ `123456654321`
- ✅ ``

---

```
/^\d\d\d\d\d\d\d\d\d\d\d\d/
```
(no 194) succeeded 1 of 1 times:

- ✅ `123456654321`

---

```
/^[\d][\d][\d][\d][\d][\d][\d][\d][\d][\d][\d][\d]/
```
(no 195) succeeded 2 of 2 times:

- ✅ `123456654321`
- ✅ ``

---

```
/^[abc]{12}/
```
(no 196) succeeded 2 of 2 times:

- ✅ `abcabcabcabc`
- ✅ ``

---

```
/^[a-c]{12}/
```
(no 197) succeeded 2 of 2 times:

- ✅ `abcabcabcabc`
- ✅ ``

---

```
/^(a|b|c){12}/
```
(no 198) succeeded 1 of 1 times:

- ✅ `abcabcabcabc`

---

```
/^[abcdefghijklmnopqrstuvwxy0123456789]/
```
(no 199) succeeded 2 of 2 times:

- ✅ `n`
- ✅ `z` no match

---

```
/abcde{0,0}/
```
(no 200) succeeded 2 of 2 times:

- ✅ `abcd`
- ✅ `abce` no match

---

```
/ab[cd]{0,0}e/
```
(no 201) succeeded 3 of 3 times:

- ✅ `abe`
- ✅ `abcde` no match
- ✅ `` no match

---

```
/ab(c){0,0}d/
```
(no 202) succeeded 2 of 2 times:

- ✅ `abd`
- ✅ `abcd` no match

---

```
/a(b*)/
```
(no 203) succeeded 5 of 5 times:

- ✅ `a`
- ✅ `ab`
- ✅ `abbbb`
- ✅ `bbbbb` no match
- ✅ `` no match

---

```
/ab\d{0}e/
```
(no 204) succeeded 3 of 3 times:

- ✅ `abe`
- ✅ `ab1e` no match
- ✅ `` no match

---

```
/"([^\\"]+|\\.)*"/
```
(no 205) succeeded 2 of 2 times:

- ✅ `the \"quick\" brown fox`
- ✅ `\"the \\\"quick\\\" brown fox\"`

---

```
/<tr([\w\W\s\d][^<>]{0,})><TD([\w\W\s\d][^<>]{0,})>([\d]{0,}\.)(.*)((<BR>([\w\W\s\d][^<>]{0,})|[\s]{0,}))<\/a><\/TD><TD([\w\W\s\d][^<>]{0,})>([\w\W\s\d][^<>]{0,})<\/TD><TD([\w\W\s\d][^<>]{0,})>([\w\W\s\d][^<>]{0,})<\/TD><\/TR>/is
```
(no 210) succeeded 1 of 1 times:

- ✅ `<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>`

---

```
/a[^a]b/
```
(no 211) succeeded 3 of 3 times:

- ✅ `acb`
- ✅ `a\nb`
- ✅ ``

---

```
/a.b/
```
(no 212) succeeded 3 of 3 times:

- ✅ `acb`
- ✅ `a\nb` no match
- ✅ `` no match

---

```
/a[^a]b/s
```
(no 213) succeeded 3 of 3 times:

- ✅ `acb`
- ✅ `a\nb`
- ✅ ``

---

```
/a.b/s
```
(no 214) succeeded 2 of 2 times:

- ✅ `acb`
- ✅ `a\nb`

---

```
/^(b+?|a){1,2}?c/
```
(no 215) succeeded 5 of 5 times:

- ✅ `bac`
- ✅ `bbac`
- ✅ `bbbac`
- ✅ `bbbbac`
- ✅ `bbbbbac`

---

```
/^(b+|a){1,2}?c/
```
(no 216) succeeded 6 of 6 times:

- ✅ `bac`
- ✅ `bbac`
- ✅ `bbbac`
- ✅ `bbbbac`
- ✅ `bbbbbac`
- ✅ ``

---

```
/(?!\A)x/m
```
(no 217) succeeded 4 of 4 times:

- ✅ `abx\n`
- ✅ `a\nx\n`
- ✅ `x\nb\n` no match
- ✅ `` no match

---

```
/(A|B)*?CD/
```
(no 218) succeeded 2 of 2 times:

- ✅ `CD`
- ✅ ``

---

```
/(A|B)*CD/
```
(no 219) succeeded 1 of 1 times:

- ✅ `CD`

---

```
/(AB)*?\1/
```
(no 220) succeeded 1 of 1 times:

- ✅ `ABABAB`

---

```
/(AB)*\1/
```
(no 221) succeeded 2 of 2 times:

- ✅ `ABABAB`
- ✅ ``

---

```
/(?<!bar)foo/
```
(no 222) succeeded 6 of 6 times:

- ✅ `foo`
- ✅ `catfood`
- ✅ `arfootle`
- ✅ `rfoosh`
- ✅ `barfoo` no match
- ✅ `towbarfoo` no match

---

```
/\w{3}(?<!bar)foo/
```
(no 223) succeeded 4 of 4 times:

- ✅ `catfood`
- ✅ `foo` no match
- ✅ `barfoo` no match
- ✅ `towbarfoo` no match

---

```
/(?<=(foo)a)bar/
```
(no 224) succeeded 4 of 4 times:

- ✅ `fooabar`
- ✅ `bar` no match
- ✅ `foobbar` no match
- ✅ `` no match

---

```
/\Aabc\z/m
```
(no 225) succeeded 5 of 5 times:

- ✅ `abc`
- ✅ `abc\n` no match
- ✅ `qqq\nabc` no match
- ✅ `abc\nzzz` no match
- ✅ `qqq\nabc\nzzz` no match

---

```
/(?>.*/)foo/
```
(no 226) succeeded 2 of 2 times:

- ✅ `/this/is/a/very/long/line/in/deed/with/very/many/slashes/in/and/foo`
- ✅ `/this/is/a/very/long/line/in/deed/with/very/many/slashes/in/it/you/see/` no match

---

```
/(?>(\.\d\d[1-9]?))\d+/
```
(no 227) succeeded 3 of 3 times:

- ✅ `1.230003938`
- ✅ `1.875000282`
- ✅ `1.235` no match

---

```
/^((?>\w+)|(?>\s+))*$/
```
(no 228) succeeded 2 of 2 times:

- ✅ `now is the time for all good men to come to the aid of the party`
- ✅ `this is not a line with only words and spaces!` no match

---

```
/(\d+)(\w)/
```
(no 229) succeeded 2 of 2 times:

- ✅ `12345a`
- ✅ `12345+`

---

```
/((?>\d+))(\w)/
```
(no 230) succeeded 2 of 2 times:

- ✅ `12345a`
- ✅ `12345+` no match

---

```
/(?>a+)b/
```
(no 231) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/((?>a+)b)/
```
(no 232) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/(?>(a+))b/
```
(no 233) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/(?>b)+/
```
(no 234) succeeded 1 of 1 times:

- ✅ `aaabbbccc`

---

```
/(?>a+|b+|c+)*c/
```
(no 235) succeeded 1 of 1 times:

- ✅ `aaabbbbccccd`

---

```
/((?>[^()]+)|\([^()]*\))+/
```
(no 236) succeeded 2 of 2 times:

- ✅ `((abc(ade)ufh()()x`
- ✅ ``

---

```
/\(((?>[^()]+)|\([^()]+\))+\)/
```
(no 237) succeeded 3 of 3 times:

- ✅ `(abc)`
- ✅ `(abc(def)xyz)`
- ✅ `((()aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---

```
/a(?-i)b/i
```
(no 238) succeeded 5 of 5 times:

- ✅ `ab`
- ✅ `Ab`
- ✅ `aB` no match
- ✅ `AB` no match
- ✅ `` no match

---

```
/(a (?x)b c)d e/
```
(no 239) succeeded 5 of 5 times:

- ✅ `a bcd e`
- ✅ `a b cd e` no match
- ✅ `abcd e` no match
- ✅ `a bcde` no match
- ✅ `` no match

---

```
/(a b(?x)c d (?-x)e f)/
```
(no 240) succeeded 2 of 2 times:

- ✅ `a bcde f`
- ✅ `abcdef` no match

---

```
/(a(?i)b)c/
```
(no 241) succeeded 9 of 9 times:

- ✅ `abc`
- ✅ `aBc`
- ✅ `abC` no match
- ✅ `aBC` no match
- ✅ `Abc` no match
- ✅ `ABc` no match
- ✅ `ABC` no match
- ✅ `AbC` no match
- ✅ `` no match

---

```
/a(?i:b)c/
```
(no 242) succeeded 6 of 6 times:

- ✅ `abc`
- ✅ `aBc`
- ✅ `ABC` no match
- ✅ `abC` no match
- ✅ `aBC` no match
- ✅ `` no match

---

```
/a(?i:b)*c/
```
(no 243) succeeded 5 of 5 times:

- ✅ `aBc`
- ✅ `aBBc`
- ✅ `aBC` no match
- ✅ `aBBC` no match
- ✅ `` no match

---

```
/a(?=b(?i)c)\w\wd/
```
(no 244) succeeded 5 of 5 times:

- ✅ `abcd`
- ✅ `abCd`
- ✅ `aBCd` no match
- ✅ `abcD` no match
- ✅ `` no match

---

```
/(?s-i:more.*than).*million/i
```
(no 245) succeeded 5 of 5 times:

- ✅ `more than million`
- ✅ `more than MILLION`
- ✅ `more \n than Million`
- ✅ `MORE THAN MILLION` no match
- ✅ `more \n than \n million` no match

---

```
/(?:(?s-i)more.*than).*million/i
```
(no 246) succeeded 6 of 6 times:

- ✅ `more than million`
- ✅ `more than MILLION`
- ✅ `more \n than Million`
- ✅ `MORE THAN MILLION` no match
- ✅ `more \n than \n million` no match
- ✅ `` no match

---

```
/(?>a(?i)b+)+c/
```
(no 247) succeeded 7 of 7 times:

- ✅ `abc`
- ✅ `aBbc`
- ✅ `aBBc`
- ✅ `Abc` no match
- ✅ `abAb` no match
- ✅ `abbC` no match
- ✅ `` no match

---

```
/(?=a(?i)b)\w\wc/
```
(no 248) succeeded 6 of 6 times:

- ✅ `abc`
- ✅ `aBc`
- ✅ `Ab` no match
- ✅ `abC` no match
- ✅ `aBC` no match
- ✅ `` no match

---

```
/(?<=a(?i)b)(\w\w)c/
```
(no 249) succeeded 5 of 5 times:

- ✅ `abxxc`
- ✅ `aBxxc`
- ✅ `Abxxc` no match
- ✅ `ABxxc` no match
- ✅ `abxxC` no match

---

```
/(?:(a)|b)(?(1)A|B)/
```
(no 250) succeeded 4 of 4 times:

- ✅ `aA`
- ✅ `bB`
- ✅ `aB` no match
- ✅ `bA` no match

---

```
/^(a)?(?(1)a|b)+$/
```
(no 251) succeeded 5 of 5 times:

- ✅ `aa`
- ✅ `b`
- ✅ `bb`
- ✅ `ab` no match
- ✅ `` no match

---

```
/^(?(?=abc)\w{3}:|\d\d)/
```
(no 252) succeeded 4 of 4 times:

- ✅ `abc:`
- ✅ `12`
- ✅ `123`
- ✅ `xyz` no match

---

```
/^(?(?!abc)\d\d|\w{3}:)$/
```
(no 253) succeeded 5 of 5 times:

- ✅ `abc:`
- ✅ `12`
- ✅ `123` no match
- ✅ `xyz` no match
- ✅ `` no match

---

```
/(?(?<=foo)bar|cat)/
```
(no 254) succeeded 5 of 5 times:

- ✅ `foobar`
- ✅ `cat`
- ✅ `fcat`
- ✅ `focat`
- ✅ `foocat` no match

---

```
/(?(?<!foo)cat|bar)/
```
(no 255) succeeded 5 of 5 times:

- ✅ `foobar`
- ✅ `cat`
- ✅ `fcat`
- ✅ `focat`
- ✅ `foocat` no match

---

```
/( \( )? [^()]+ (?(1) \) |) /x
```
(no 256) succeeded 4 of 4 times:

- ✅ `abcd`
- ✅ `(abcd)`
- ✅ `the quick (abcd) fox`
- ✅ `(abcd`

---

```
/( \( )? [^()]+ (?(1) \) ) /x
```
(no 257) succeeded 4 of 4 times:

- ✅ `abcd`
- ✅ `(abcd)`
- ✅ `the quick (abcd) fox`
- ✅ `(abcd`

---

```
/^(?(2)a|(1)(2))+$/
```
(no 258) succeeded 4 of 4 times:

- ✅ `12`
- ✅ `12a`
- ✅ `12aa`
- ✅ `1234` no match

---

```
/((?i)blah)\s+\1/
```
(no 259) succeeded 7 of 7 times:

- ✅ `blah blah`
- ✅ `BLAH BLAH`
- ✅ `Blah Blah`
- ✅ `blaH blaH`
- ✅ `blah BLAH` no match
- ✅ `Blah blah` no match
- ✅ `blaH blah` no match

---

```
/((?i)blah)\s+(?i:\1)/
```
(no 260) succeeded 7 of 7 times:

- ✅ `blah blah`
- ✅ `BLAH BLAH`
- ✅ `Blah Blah`
- ✅ `blaH blaH`
- ✅ `blah BLAH`
- ✅ `Blah blah`
- ✅ `blaH blah`

---

```
/((?i)blah)\s+(?m)A(?i:\1)/
```
(no 261) succeeded 2 of 2 times:

- ✅ `blah ABLAH`
- ✅ `blah aBLAH` no match

---

```
/(?>a*)*/
```
(no 262) succeeded 4 of 4 times:

- ✅ `a`
- ✅ `aa`
- ✅ `aaaa`
- ✅ ``

---

```
/(abc|)+/
```
(no 263) succeeded 4 of 4 times:

- ✅ `abc`
- ✅ `abcabc`
- ✅ `abcabcabc`
- ✅ `xyz`

---

```
/([a]*)*/
```
(no 264) succeeded 3 of 3 times:

- ✅ `a`
- ✅ `aaaaa`
- ✅ ``

---

```
/([ab]*)*/
```
(no 265) succeeded 6 of 6 times:

- ✅ `a`
- ✅ `b`
- ✅ `ababab`
- ✅ `aaaabcde`
- ✅ `bbbb`
- ✅ ``

---

```
/([^a]*)*/
```
(no 266) succeeded 4 of 4 times:

- ✅ `b`
- ✅ `bbbb`
- ✅ `aaa`
- ✅ ``

---

```
/([^ab]*)*/
```
(no 267) succeeded 3 of 3 times:

- ✅ `cccc`
- ✅ `abab`
- ✅ ``

---

```
/([a]*?)*/
```
(no 268) succeeded 3 of 3 times:

- ✅ `a`
- ✅ `aaaa`
- ✅ ``

---

```
/([ab]*?)*/
```
(no 269) succeeded 5 of 5 times:

- ✅ `a`
- ✅ `b`
- ✅ `abab`
- ✅ `baba`
- ✅ ``

---

```
/([^a]*?)*/
```
(no 270) succeeded 4 of 4 times:

- ✅ `b`
- ✅ `bbbb`
- ✅ `aaa`
- ✅ ``

---

```
/([^ab]*?)*/
```
(no 271) succeeded 4 of 4 times:

- ✅ `c`
- ✅ `cccc`
- ✅ `baba`
- ✅ ``

---

```
/(?>a*)*/
```
(no 272) succeeded 3 of 3 times:

- ✅ `a`
- ✅ `aaabcde`
- ✅ ``

---

```
/((?>a*))*/
```
(no 273) succeeded 3 of 3 times:

- ✅ `aaaaa`
- ✅ `aabbaa`
- ✅ ``

---

```
/((?>a*?))*/
```
(no 274) succeeded 2 of 2 times:

- ✅ `aaaaa`
- ✅ `aabbaa`

---

```
/(?(?=[^a-z]+[a-z])  \d{2}-[a-z]{3}-\d{2}  |  \d{2}-\d{2}-\d{2} ) /x
```
(no 275) succeeded 4 of 4 times:

- ✅ `12-sep-98`
- ✅ `12-09-98`
- ✅ `sep-12-98` no match
- ✅ `` no match

---

```
/(?<=(foo))bar\1/
```
(no 276) succeeded 4 of 4 times:

- ✅ `foobarfoo`
- ✅ `foobarfootling`
- ✅ `foobar` no match
- ✅ `barfoo` no match

---

```
/(?i:saturday|sunday)/
```
(no 277) succeeded 8 of 8 times:

- ✅ `saturday`
- ✅ `sunday`
- ✅ `Saturday`
- ✅ `Sunday`
- ✅ `SATURDAY`
- ✅ `SUNDAY`
- ✅ `SunDay`
- ✅ ``

---

```
/(a(?i)bc|BB)x/
```
(no 278) succeeded 8 of 8 times:

- ✅ `abcx`
- ✅ `aBCx`
- ✅ `bbx`
- ✅ `BBx`
- ✅ `abcX` no match
- ✅ `aBCX` no match
- ✅ `bbX` no match
- ✅ `BBX` no match

---

```
/^([ab](?i)[cd]|[ef])/
```
(no 279) succeeded 8 of 8 times:

- ✅ `ac`
- ✅ `aC`
- ✅ `bD`
- ✅ `elephant`
- ✅ `Europe`
- ✅ `frog`
- ✅ `France`
- ✅ `Africa` no match

---

```
/^(ab|a(?i)[b-c](?m-i)d|x(?i)y|z)/
```
(no 280) succeeded 8 of 8 times:

- ✅ `ab`
- ✅ `aBd`
- ✅ `xy`
- ✅ `xY`
- ✅ `zebra`
- ✅ `Zambesi`
- ✅ `aCD` no match
- ✅ `XY` no match

---

```
/(?<=foo\n)^bar/m
```
(no 281) succeeded 3 of 3 times:

- ✅ `foo\nbar`
- ✅ `bar` no match
- ✅ `baz\nbar` no match

---

```
/(?<=(?<!foo)bar)baz/
```
(no 282) succeeded 5 of 5 times:

- ✅ `barbaz`
- ✅ `barbarbaz`
- ✅ `koobarbaz`
- ✅ `baz` no match
- ✅ `foobarbaz` no match

---

```
/^(a\1?){4}$/
```
(no 283) succeeded 14 of 14 times:

- ✅ `aaaaa`
- ✅ `aaaaaaa`
- ✅ `aaaaaaaaaa`
- ✅ `a` no match
- ✅ `aa` no match
- ✅ `aaa` no match
- ✅ `aaaaaaaa` no match
- ✅ `aaaaaaaaa` no match
- ✅ `aaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaaaaa` no match

---

```
/^(a\1?)(a\1?)(a\2?)(a\3?)$/
```
(no 284) succeeded 16 of 16 times:

- ✅ `aaaa`
- ✅ `aaaaa`
- ✅ `aaaaaa`
- ✅ `aaaaaaa`
- ✅ `aaaaaaaaaa`
- ✅ `a` no match
- ✅ `aa` no match
- ✅ `aaa` no match
- ✅ `aaaaaaaa` no match
- ✅ `aaaaaaaaa` no match
- ✅ `aaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaaaa` no match
- ✅ `aaaaaaaaaaaaaaaa` no match

---

```
/abc/
```
(no 285) succeeded 6 of 6 times:

- ✅ `abc`
- ✅ `xabcy`
- ✅ `ababc`
- ✅ `xbc` no match
- ✅ `axc` no match
- ✅ `abx` no match

---

```
/ab*c/
```
(no 286) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/ab*bc/
```
(no 287) succeeded 3 of 3 times:

- ✅ `abc`
- ✅ `abbc`
- ✅ `abbbbc`

---

```
/.{1}/
```
(no 288) succeeded 1 of 1 times:

- ✅ `abbbbc`

---

```
/.{3,4}/
```
(no 289) succeeded 1 of 1 times:

- ✅ `abbbbc`

---

```
/ab{0,}bc/
```
(no 290) succeeded 1 of 1 times:

- ✅ `abbbbc`

---

```
/ab+bc/
```
(no 291) succeeded 3 of 3 times:

- ✅ `abbc`
- ✅ `abc` no match
- ✅ `abq` no match

---

```
/ab{1,}bc/
```
(no 292) succeeded 0 of 0 times:


---

```
/ab+bc/
```
(no 293) succeeded 1 of 1 times:

- ✅ `abbbbc`

---

```
/ab{1,}bc/
```
(no 294) succeeded 1 of 1 times:

- ✅ `abbbbc`

---

```
/ab{1,3}bc/
```
(no 295) succeeded 1 of 1 times:

- ✅ `abbbbc`

---

```
/ab{3,4}bc/
```
(no 296) succeeded 1 of 1 times:

- ✅ `abbbbc`

---

```
/ab{4,5}bc/
```
(no 297) succeeded 2 of 2 times:

- ✅ `abq` no match
- ✅ `abbbbc` no match

---

```
/ab?bc/
```
(no 298) succeeded 2 of 2 times:

- ✅ `abbc`
- ✅ `abc`

---

```
/ab{0,1}bc/
```
(no 299) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/ab?bc/
```
(no 300) succeeded 0 of 0 times:


---

```
/ab?c/
```
(no 301) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/ab{0,1}c/
```
(no 302) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/^abc$/
```
(no 303) succeeded 3 of 3 times:

- ✅ `abc`
- ✅ `abbbbc` no match
- ✅ `abcc` no match

---

```
/^abc/
```
(no 304) succeeded 1 of 1 times:

- ✅ `abcc`

---

```
/^abc$/
```
(no 305) succeeded 0 of 0 times:


---

```
/abc$/
```
(no 306) succeeded 2 of 2 times:

- ✅ `aabc`
- ✅ `aabcd` no match

---

```
/^/
```
(no 307) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/$/
```
(no 308) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/a.c/
```
(no 309) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ `axc`

---

```
/a.*c/
```
(no 310) succeeded 1 of 1 times:

- ✅ `axyzc`

---

```
/a[bc]d/
```
(no 311) succeeded 3 of 3 times:

- ✅ `abd`
- ✅ `axyzd` no match
- ✅ `abc` no match

---

```
/a[b-d]e/
```
(no 312) succeeded 1 of 1 times:

- ✅ `ace`

---

```
/a[b-d]/
```
(no 313) succeeded 1 of 1 times:

- ✅ `aac`

---

```
/a[-b]/
```
(no 314) succeeded 1 of 1 times:

- ✅ `a-`

---

```
/a[b-]/
```
(no 315) succeeded 1 of 1 times:

- ✅ `a-`

---

```
/a]/
```
(no 316) succeeded 1 of 1 times:

- ✅ `a]`

---

```
/a[]]b/
```
(no 317) succeeded 1 of 1 times:

- ✅ `a]b`

---

```
/a[^bc]d/
```
(no 318) succeeded 3 of 3 times:

- ✅ `aed`
- ✅ `abd` no match
- ✅ `abd` no match

---

```
/a[^-b]c/
```
(no 319) succeeded 1 of 1 times:

- ✅ `adc`

---

```
/a[^]b]c/
```
(no 320) succeeded 3 of 3 times:

- ✅ `adc`
- ✅ `a-c`
- ✅ `a]c` no match

---

```
/\ba\b/
```
(no 321) succeeded 3 of 3 times:

- ✅ `a-`
- ✅ `-a`
- ✅ `-a-`

---

```
/\by\b/
```
(no 322) succeeded 3 of 3 times:

- ✅ `xy` no match
- ✅ `yz` no match
- ✅ `xyz` no match

---

```
/\Ba\B/
```
(no 323) succeeded 3 of 3 times:

- ✅ `a-` no match
- ✅ `-a` no match
- ✅ `-a-` no match

---

```
/\By\b/
```
(no 324) succeeded 1 of 1 times:

- ✅ `xy`

---

```
/\by\B/
```
(no 325) succeeded 1 of 1 times:

- ✅ `yz`

---

```
/\By\B/
```
(no 326) succeeded 1 of 1 times:

- ✅ `xyz`

---

```
/\w/
```
(no 327) succeeded 1 of 1 times:

- ✅ `a`

---

```
/\W/
```
(no 328) succeeded 2 of 2 times:

- ✅ `-`
- ✅ `a` no match

---

```
/a\sb/
```
(no 329) succeeded 1 of 1 times:

- ✅ `a b`

---

```
/a\Sb/
```
(no 330) succeeded 2 of 2 times:

- ✅ `a-b`
- ✅ `a b` no match

---

```
/\d/
```
(no 331) succeeded 1 of 1 times:

- ✅ `1`

---

```
/\D/
```
(no 332) succeeded 2 of 2 times:

- ✅ `-`
- ✅ `1` no match

---

```
/[\w]/
```
(no 333) succeeded 1 of 1 times:

- ✅ `a`

---

```
/[\W]/
```
(no 334) succeeded 2 of 2 times:

- ✅ `-`
- ✅ `a` no match

---

```
/a[\s]b/
```
(no 335) succeeded 1 of 1 times:

- ✅ `a b`

---

```
/a[\S]b/
```
(no 336) succeeded 2 of 2 times:

- ✅ `a-b`
- ✅ `a b` no match

---

```
/[\d]/
```
(no 337) succeeded 1 of 1 times:

- ✅ `1`

---

```
/[\D]/
```
(no 338) succeeded 2 of 2 times:

- ✅ `-`
- ✅ `1` no match

---

```
/ab|cd/
```
(no 339) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ `abcd`

---

```
/()ef/
```
(no 340) succeeded 1 of 1 times:

- ✅ `def`

---

```
/$b/
```
(no 341) succeeded 0 of 0 times:


---

```
/a\(b/
```
(no 342) succeeded 1 of 1 times:

- ✅ `a(b`

---

```
/a\(*b/
```
(no 343) succeeded 2 of 2 times:

- ✅ `ab`
- ✅ `a((b`

---

```
/a\\b/
```
(no 344) succeeded 1 of 1 times:

- ✅ `a\\b`

---

```
/((a))/
```
(no 345) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/(a)b(c)/
```
(no 346) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/a+b+c/
```
(no 347) succeeded 1 of 1 times:

- ✅ `aabbabc`

---

```
/a{1,}b{1,}c/
```
(no 348) succeeded 1 of 1 times:

- ✅ `aabbabc`

---

```
/a.+?c/
```
(no 349) succeeded 1 of 1 times:

- ✅ `abcabc`

---

```
/(a+|b)*/
```
(no 350) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(a+|b){0,}/
```
(no 351) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(a+|b)+/
```
(no 352) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(a+|b){1,}/
```
(no 353) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(a+|b)?/
```
(no 354) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(a+|b){0,1}/
```
(no 355) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/[^ab]*/
```
(no 356) succeeded 1 of 1 times:

- ✅ `cde`

---

```
/abc/
```
(no 357) succeeded 1 of 1 times:

- ✅ `b` no match

---

```
/a*/
```
(no 358) succeeded 1 of 1 times:

- ✅ ``

---

```
/([abc])*d/
```
(no 359) succeeded 1 of 1 times:

- ✅ `abbbcd`

---

```
/([abc])*bcd/
```
(no 360) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/a|b|c|d|e/
```
(no 361) succeeded 1 of 1 times:

- ✅ `e`

---

```
/(a|b|c|d|e)f/
```
(no 362) succeeded 1 of 1 times:

- ✅ `ef`

---

```
/abcd*efg/
```
(no 363) succeeded 1 of 1 times:

- ✅ `abcdefg`

---

```
/ab*/
```
(no 364) succeeded 2 of 2 times:

- ✅ `xabyabbbz`
- ✅ `xayabbbz`

---

```
/(ab|cd)e/
```
(no 365) succeeded 1 of 1 times:

- ✅ `abcde`

---

```
/[abhgefdc]ij/
```
(no 366) succeeded 1 of 1 times:

- ✅ `hij`

---

```
/^(ab|cd)e/
```
(no 367) succeeded 0 of 0 times:


---

```
/(abc|)ef/
```
(no 368) succeeded 1 of 1 times:

- ✅ `abcdef`

---

```
/(a|b)c*d/
```
(no 369) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/(ab|ab*)bc/
```
(no 370) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/a([bc]*)c*/
```
(no 371) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/a([bc]*)(c*d)/
```
(no 372) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/a([bc]+)(c*d)/
```
(no 373) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/a([bc]*)(c+d)/
```
(no 374) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/a[bcd]*dcdcde/
```
(no 375) succeeded 1 of 1 times:

- ✅ `adcdcde`

---

```
/a[bcd]+dcdcde/
```
(no 376) succeeded 2 of 2 times:

- ✅ `abcde` no match
- ✅ `adcdcde` no match

---

```
/(ab|a)b*c/
```
(no 377) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/((a)(b)c)(d)/
```
(no 378) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/[a-zA-Z_][a-zA-Z0-9_]*/
```
(no 379) succeeded 1 of 1 times:

- ✅ `alpha`

---

```
/^a(bc+|b[eh])g|.h$/
```
(no 380) succeeded 1 of 1 times:

- ✅ `abh`

---

```
/(bc+d$|ef*g.|h?i(j|k))/
```
(no 381) succeeded 5 of 5 times:

- ✅ `effgz`
- ✅ `ij`
- ✅ `reffgz`
- ✅ `effg` no match
- ✅ `bcdd` no match

---

```
/((((((((((a))))))))))/
```
(no 382) succeeded 1 of 1 times:

- ✅ `a`

---

```
/((((((((((a))))))))))\10/
```
(no 383) succeeded 1 of 1 times:

- ✅ `aa`

---

```
/(((((((((a)))))))))/
```
(no 384) succeeded 1 of 1 times:

- ✅ `a`

---

```
/multiple words of text/
```
(no 385) succeeded 2 of 2 times:

- ✅ `aa` no match
- ✅ `uh-uh` no match

---

```
/multiple words/
```
(no 386) succeeded 1 of 1 times:

- ✅ `multiple words, yeah`

---

```
/(.*)c(.*)/
```
(no 387) succeeded 1 of 1 times:

- ✅ `abcde`

---

```
/\((.*), (.*)\)/
```
(no 388) succeeded 1 of 1 times:

- ✅ `(a, b)`

---

```
/[k]/
```
(no 389) succeeded 0 of 0 times:


---

```
/abcd/
```
(no 390) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/a(bc)d/
```
(no 391) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/a[-]?c/
```
(no 392) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/(abc)\1/
```
(no 393) succeeded 1 of 1 times:

- ✅ `abcabc`

---

```
/([a-c]*)\1/
```
(no 394) succeeded 1 of 1 times:

- ✅ `abcabc`

---

```
/(a)|\1/
```
(no 395) succeeded 3 of 3 times:

- ✅ `a`
- ✅ `ab`
- ✅ `x` no match

---

```
/(([a-c])b*?\2)*/
```
(no 396) succeeded 1 of 1 times:

- ✅ `ababbbcbc`

---

```
/((\3|b)\2(a)x)+/
```
(no 398) succeeded 1 of 1 times:

- ✅ `aaaxabaxbaaxbbax`

---

```
/abc/i
```
(no 400) succeeded 7 of 7 times:

- ✅ `ABC`
- ✅ `XABCY`
- ✅ `ABABC`
- ✅ `aaxabxbaxbbx` no match
- ✅ `XBC` no match
- ✅ `AXC` no match
- ✅ `ABX` no match

---

```
/ab*c/i
```
(no 401) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/ab*bc/i
```
(no 402) succeeded 2 of 2 times:

- ✅ `ABC`
- ✅ `ABBC`

---

```
/ab*?bc/i
```
(no 403) succeeded 1 of 1 times:

- ✅ `ABBBBC`

---

```
/ab{0,}?bc/i
```
(no 404) succeeded 1 of 1 times:

- ✅ `ABBBBC`

---

```
/ab+?bc/i
```
(no 405) succeeded 1 of 1 times:

- ✅ `ABBC`

---

```
/ab+bc/i
```
(no 406) succeeded 2 of 2 times:

- ✅ `ABC` no match
- ✅ `ABQ` no match

---

```
/ab{1,}bc/i
```
(no 407) succeeded 0 of 0 times:


---

```
/ab+bc/i
```
(no 408) succeeded 1 of 1 times:

- ✅ `ABBBBC`

---

```
/ab{1,}?bc/i
```
(no 409) succeeded 1 of 1 times:

- ✅ `ABBBBC`

---

```
/ab{1,3}?bc/i
```
(no 410) succeeded 1 of 1 times:

- ✅ `ABBBBC`

---

```
/ab{3,4}?bc/i
```
(no 411) succeeded 1 of 1 times:

- ✅ `ABBBBC`

---

```
/ab{4,5}?bc/i
```
(no 412) succeeded 2 of 2 times:

- ✅ `ABQ` no match
- ✅ `ABBBBC` no match

---

```
/ab??bc/i
```
(no 413) succeeded 2 of 2 times:

- ✅ `ABBC`
- ✅ `ABC`

---

```
/ab{0,1}?bc/i
```
(no 414) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/ab??bc/i
```
(no 415) succeeded 0 of 0 times:


---

```
/ab??c/i
```
(no 416) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/ab{0,1}?c/i
```
(no 417) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/^abc$/i
```
(no 418) succeeded 3 of 3 times:

- ✅ `ABC`
- ✅ `ABBBBC` no match
- ✅ `ABCC` no match

---

```
/^abc/i
```
(no 419) succeeded 1 of 1 times:

- ✅ `ABCC`

---

```
/^abc$/i
```
(no 420) succeeded 0 of 0 times:


---

```
/abc$/i
```
(no 421) succeeded 1 of 1 times:

- ✅ `AABC`

---

```
/^/i
```
(no 422) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/$/i
```
(no 423) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/a.c/i
```
(no 424) succeeded 2 of 2 times:

- ✅ `ABC`
- ✅ `AXC`

---

```
/a.*?c/i
```
(no 425) succeeded 1 of 1 times:

- ✅ `AXYZC`

---

```
/a.*c/i
```
(no 426) succeeded 2 of 2 times:

- ✅ `AABC`
- ✅ `AXYZD` no match

---

```
/a[bc]d/i
```
(no 427) succeeded 1 of 1 times:

- ✅ `ABD`

---

```
/a[b-d]e/i
```
(no 428) succeeded 3 of 3 times:

- ✅ `ACE`
- ✅ `ABC` no match
- ✅ `ABD` no match

---

```
/a[b-d]/i
```
(no 429) succeeded 1 of 1 times:

- ✅ `AAC`

---

```
/a[-b]/i
```
(no 430) succeeded 1 of 1 times:

- ✅ `A-`

---

```
/a[b-]/i
```
(no 431) succeeded 1 of 1 times:

- ✅ `A-`

---

```
/a]/i
```
(no 432) succeeded 1 of 1 times:

- ✅ `A]`

---

```
/a[]]b/i
```
(no 433) succeeded 1 of 1 times:

- ✅ `A]B`

---

```
/a[^bc]d/i
```
(no 434) succeeded 1 of 1 times:

- ✅ `AED`

---

```
/a[^-b]c/i
```
(no 435) succeeded 3 of 3 times:

- ✅ `ADC`
- ✅ `ABD` no match
- ✅ `A-C` no match

---

```
/a[^]b]c/i
```
(no 436) succeeded 1 of 1 times:

- ✅ `ADC`

---

```
/ab|cd/i
```
(no 437) succeeded 2 of 2 times:

- ✅ `ABC`
- ✅ `ABCD`

---

```
/()ef/i
```
(no 438) succeeded 1 of 1 times:

- ✅ `DEF`

---

```
/$b/i
```
(no 439) succeeded 2 of 2 times:

- ✅ `A]C` no match
- ✅ `B` no match

---

```
/a\(b/i
```
(no 440) succeeded 1 of 1 times:

- ✅ `A(B`

---

```
/a\(*b/i
```
(no 441) succeeded 2 of 2 times:

- ✅ `AB`
- ✅ `A((B`

---

```
/a\\b/i
```
(no 442) succeeded 2 of 2 times:

- ✅ `A\\b`
- ✅ `a\\B`

---

```
/((a))/i
```
(no 443) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/(a)b(c)/i
```
(no 444) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/a+b+c/i
```
(no 445) succeeded 1 of 1 times:

- ✅ `AABBABC`

---

```
/a{1,}b{1,}c/i
```
(no 446) succeeded 1 of 1 times:

- ✅ `AABBABC`

---

```
/a.+?c/i
```
(no 447) succeeded 1 of 1 times:

- ✅ `ABCABC`

---

```
/a.*?c/i
```
(no 448) succeeded 1 of 1 times:

- ✅ `ABCABC`

---

```
/a.{0,5}?c/i
```
(no 449) succeeded 1 of 1 times:

- ✅ `ABCABC`

---

```
/(a+|b)*/i
```
(no 450) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/(a+|b){0,}/i
```
(no 451) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/(a+|b)+/i
```
(no 452) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/(a+|b){1,}/i
```
(no 453) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/(a+|b)?/i
```
(no 454) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/(a+|b){0,1}/i
```
(no 455) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/(a+|b){0,1}?/i
```
(no 456) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/[^ab]*/i
```
(no 457) succeeded 1 of 1 times:

- ✅ `CDE`

---

```
/([abc])*d/i
```
(no 458) succeeded 1 of 1 times:

- ✅ `ABBBCD`

---

```
/([abc])*bcd/i
```
(no 459) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/a|b|c|d|e/i
```
(no 460) succeeded 1 of 1 times:

- ✅ `E`

---

```
/(a|b|c|d|e)f/i
```
(no 461) succeeded 1 of 1 times:

- ✅ `EF`

---

```
/abcd*efg/i
```
(no 462) succeeded 1 of 1 times:

- ✅ `ABCDEFG`

---

```
/ab*/i
```
(no 463) succeeded 2 of 2 times:

- ✅ `XABYABBBZ`
- ✅ `XAYABBBZ`

---

```
/(ab|cd)e/i
```
(no 464) succeeded 1 of 1 times:

- ✅ `ABCDE`

---

```
/[abhgefdc]ij/i
```
(no 465) succeeded 1 of 1 times:

- ✅ `HIJ`

---

```
/^(ab|cd)e/i
```
(no 466) succeeded 1 of 1 times:

- ✅ `ABCDE` no match

---

```
/(abc|)ef/i
```
(no 467) succeeded 1 of 1 times:

- ✅ `ABCDEF`

---

```
/(a|b)c*d/i
```
(no 468) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/(ab|ab*)bc/i
```
(no 469) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/a([bc]*)c*/i
```
(no 470) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/a([bc]*)(c*d)/i
```
(no 471) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/a([bc]+)(c*d)/i
```
(no 472) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/a([bc]*)(c+d)/i
```
(no 473) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/a[bcd]*dcdcde/i
```
(no 474) succeeded 1 of 1 times:

- ✅ `ADCDCDE`

---

```
/a[bcd]+dcdcde/i
```
(no 475) succeeded 0 of 0 times:


---

```
/(ab|a)b*c/i
```
(no 476) succeeded 1 of 1 times:

- ✅ `ABC`

---

```
/((a)(b)c)(d)/i
```
(no 477) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/[a-zA-Z_][a-zA-Z0-9_]*/i
```
(no 478) succeeded 1 of 1 times:

- ✅ `ALPHA`

---

```
/^a(bc+|b[eh])g|.h$/i
```
(no 479) succeeded 1 of 1 times:

- ✅ `ABH`

---

```
/(bc+d$|ef*g.|h?i(j|k))/i
```
(no 480) succeeded 6 of 6 times:

- ✅ `EFFGZ`
- ✅ `IJ`
- ✅ `REFFGZ`
- ✅ `ADCDCDE` no match
- ✅ `EFFG` no match
- ✅ `BCDD` no match

---

```
/((((((((((a))))))))))/i
```
(no 481) succeeded 1 of 1 times:

- ✅ `A`

---

```
/((((((((((a))))))))))\10/i
```
(no 482) succeeded 1 of 1 times:

- ✅ `AA`

---

```
/(((((((((a)))))))))/i
```
(no 483) succeeded 1 of 1 times:

- ✅ `A`

---

```
/(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))/i
```
(no 484) succeeded 1 of 1 times:

- ✅ `A`

---

```
/(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))/i
```
(no 485) succeeded 1 of 1 times:

- ✅ `C`

---

```
/multiple words of text/i
```
(no 486) succeeded 2 of 2 times:

- ✅ `AA` no match
- ✅ `UH-UH` no match

---

```
/multiple words/i
```
(no 487) succeeded 1 of 1 times:

- ✅ `MULTIPLE WORDS, YEAH`

---

```
/(.*)c(.*)/i
```
(no 488) succeeded 1 of 1 times:

- ✅ `ABCDE`

---

```
/\((.*), (.*)\)/i
```
(no 489) succeeded 1 of 1 times:

- ✅ `(A, B)`

---

```
/[k]/i
```
(no 490) succeeded 0 of 0 times:


---

```
/abcd/i
```
(no 491) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/a(bc)d/i
```
(no 492) succeeded 1 of 1 times:

- ✅ `ABCD`

---

```
/a[-]?c/i
```
(no 493) succeeded 1 of 1 times:

- ✅ `AC`

---

```
/(abc)\1/i
```
(no 494) succeeded 1 of 1 times:

- ✅ `ABCABC`

---

```
/([a-c]*)\1/i
```
(no 495) succeeded 1 of 1 times:

- ✅ `ABCABC`

---

```
/a(?!b)./
```
(no 496) succeeded 1 of 1 times:

- ✅ `abad`

---

```
/a(?=d)./
```
(no 497) succeeded 1 of 1 times:

- ✅ `abad`

---

```
/a(?=c|d)./
```
(no 498) succeeded 1 of 1 times:

- ✅ `abad`

---

```
/a(?:b|c|d)(.)/
```
(no 499) succeeded 1 of 1 times:

- ✅ `ace`

---

```
/a(?:b|c|d)*(.)/
```
(no 500) succeeded 1 of 1 times:

- ✅ `ace`

---

```
/a(?:b|c|d)+?(.)/
```
(no 501) succeeded 2 of 2 times:

- ✅ `ace`
- ✅ `acdbcdbe`

---

```
/a(?:b|c|d)+(.)/
```
(no 502) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){2}(.)/
```
(no 503) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){4,5}(.)/
```
(no 504) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){4,5}?(.)/
```
(no 505) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/((foo)|(bar))*/
```
(no 506) succeeded 1 of 1 times:

- ✅ `foobar`

---

```
/a(?:b|c|d){6,7}(.)/
```
(no 507) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){6,7}?(.)/
```
(no 508) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){5,6}(.)/
```
(no 509) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){5,6}?(.)/
```
(no 510) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){5,7}(.)/
```
(no 511) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|c|d){5,7}?(.)/
```
(no 512) succeeded 1 of 1 times:

- ✅ `acdbcdbe`

---

```
/a(?:b|(c|e){1,2}?|d)+?(.)/
```
(no 513) succeeded 1 of 1 times:

- ✅ `ace`

---

```
/^(.+)?B/
```
(no 514) succeeded 1 of 1 times:

- ✅ `AB`

---

```
/^([^a-z])|(\^)$/
```
(no 515) succeeded 1 of 1 times:

- ✅ `.`

---

```
/^[<>]&/
```
(no 516) succeeded 1 of 1 times:

- ✅ `<&OUT`

---

```
/^(a\1?){4}$/
```
(no 517) succeeded 4 of 4 times:

- ✅ `aaaaaaaaaa`
- ✅ `AB` no match
- ✅ `aaaaaaaaa` no match
- ✅ `aaaaaaaaaaa` no match

---

```
/^(a(?(1)\1)){4}$/
```
(no 518) succeeded 3 of 3 times:

- ✅ `aaaaaaaaaa`
- ✅ `aaaaaaaaa` no match
- ✅ `aaaaaaaaaaa` no match

---

```
/(?:(f)(o)(o)|(b)(a)(r))*/
```
(no 519) succeeded 1 of 1 times:

- ✅ `foobar`

---

```
/(?<=a)b/
```
(no 520) succeeded 3 of 3 times:

- ✅ `ab`
- ✅ `cb` no match
- ✅ `b` no match

---

```
/(?<!c)b/
```
(no 521) succeeded 3 of 3 times:

- ✅ `ab`
- ✅ `b`
- ✅ `b`

---

```
/(?:..)*a/
```
(no 522) succeeded 1 of 1 times:

- ✅ `aba`

---

```
/(?:..)*?a/
```
(no 523) succeeded 1 of 1 times:

- ✅ `aba`

---

```
/^(){3,5}/
```
(no 525) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/^(a+)*ax/
```
(no 526) succeeded 1 of 1 times:

- ✅ `aax`

---

```
/(a|x)*ab/
```
(no 529) succeeded 1 of 1 times:

- ✅ `cab`

---

```
/(a)*ab/
```
(no 530) succeeded 1 of 1 times:

- ✅ `cab`

---

```
/(?:(?i)a)b/
```
(no 531) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/((?i)a)b/
```
(no 532) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(?:(?i)a)b/
```
(no 533) succeeded 1 of 1 times:

- ✅ `Ab`

---

```
/((?i)a)b/
```
(no 534) succeeded 1 of 1 times:

- ✅ `Ab`

---

```
/(?:(?i)a)b/
```
(no 535) succeeded 2 of 2 times:

- ✅ `cb` no match
- ✅ `aB` no match

---

```
/((?i)a)b/
```
(no 536) succeeded 0 of 0 times:


---

```
/(?i:a)b/
```
(no 537) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/((?i:a))b/
```
(no 538) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(?i:a)b/
```
(no 539) succeeded 1 of 1 times:

- ✅ `Ab`

---

```
/((?i:a))b/
```
(no 540) succeeded 1 of 1 times:

- ✅ `Ab`

---

```
/(?i:a)b/
```
(no 541) succeeded 2 of 2 times:

- ✅ `aB` no match
- ✅ `aB` no match

---

```
/((?i:a))b/
```
(no 542) succeeded 0 of 0 times:


---

```
/(?:(?-i)a)b/i
```
(no 543) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/((?-i)a)b/i
```
(no 544) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(?:(?-i)a)b/i
```
(no 545) succeeded 1 of 1 times:

- ✅ `aB`

---

```
/((?-i)a)b/i
```
(no 546) succeeded 1 of 1 times:

- ✅ `aB`

---

```
/(?:(?-i)a)b/i
```
(no 547) succeeded 3 of 3 times:

- ✅ `aB`
- ✅ `Ab` no match
- ✅ `AB` no match

---

```
/(?-i:a)b/i
```
(no 548) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/((?-i:a))b/i
```
(no 549) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(?-i:a)b/i
```
(no 550) succeeded 1 of 1 times:

- ✅ `aB`

---

```
/((?-i:a))b/i
```
(no 551) succeeded 1 of 1 times:

- ✅ `aB`

---

```
/(?-i:a)b/i
```
(no 552) succeeded 2 of 2 times:

- ✅ `AB` no match
- ✅ `Ab` no match

---

```
/((?-i:a))b/i
```
(no 553) succeeded 0 of 0 times:


---

```
/(?-i:a)b/i
```
(no 554) succeeded 1 of 1 times:

- ✅ `aB`

---

```
/((?-i:a))b/i
```
(no 555) succeeded 1 of 1 times:

- ✅ `aB`

---

```
/(?-i:a)b/i
```
(no 556) succeeded 2 of 2 times:

- ✅ `Ab` no match
- ✅ `AB` no match

---

```
/((?-i:a))b/i
```
(no 557) succeeded 0 of 0 times:


---

```
/((?-i:a.))b/i
```
(no 558) succeeded 2 of 2 times:

- ✅ `AB` no match
- ✅ `a\nB` no match

---

```
/((?s-i:a.))b/i
```
(no 559) succeeded 1 of 1 times:

- ✅ `a\nB`

---

```
/(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))/
```
(no 560) succeeded 1 of 1 times:

- ✅ `cabbbb`

---

```
/(?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))/
```
(no 561) succeeded 1 of 1 times:

- ✅ `caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb`

---

```
/(ab)\d\1/i
```
(no 562) succeeded 2 of 2 times:

- ✅ `Ab4ab`
- ✅ `ab4Ab`

---

```
/foo\w*\d{4}baz/
```
(no 563) succeeded 1 of 1 times:

- ✅ `foobar1234baz`

---

```
/x(~~)*(?:(?:F)?)?/
```
(no 564) succeeded 1 of 1 times:

- ✅ `x~~`

---

```
/^a(?#xxx){3}c/
```
(no 565) succeeded 1 of 1 times:

- ✅ `aaac`

---

```
/^a (?#xxx) (?#yyy) {3}c/x
```
(no 566) succeeded 1 of 1 times:

- ✅ `aaac`

---

```
/(?<![cd])b/
```
(no 567) succeeded 2 of 2 times:

- ✅ `B\nB` no match
- ✅ `dbcb` no match

---

```
/(?<![cd])[ab]/
```
(no 568) succeeded 1 of 1 times:

- ✅ `dbaacb`

---

```
/(?<!(c|d))b/
```
(no 569) succeeded 0 of 0 times:


---

```
/(?<!(c|d))[ab]/
```
(no 570) succeeded 1 of 1 times:

- ✅ `dbaacb`

---

```
/(?<!cd)[ab]/
```
(no 571) succeeded 1 of 1 times:

- ✅ `cdaccb`

---

```
/^(?:a?b?)*$/
```
(no 572) succeeded 7 of 7 times:

- ✅ ``
- ✅ `a`
- ✅ `ab`
- ✅ `aaa`
- ✅ `dbcb` no match
- ✅ `a--` no match
- ✅ `aa--` no match

---

```
/((?s)^a(.))((?m)^b$)/
```
(no 573) succeeded 1 of 1 times:

- ✅ `a\nb\nc\n`

---

```
/((?m)^b$)/
```
(no 574) succeeded 1 of 1 times:

- ✅ `a\nb\nc\n`

---

```
/(?m)^b/
```
(no 575) succeeded 1 of 1 times:

- ✅ `a\nb\n`

---

```
/(?m)^(b)/
```
(no 576) succeeded 1 of 1 times:

- ✅ `a\nb\n`

---

```
/((?m)^b)/
```
(no 577) succeeded 1 of 1 times:

- ✅ `a\nb\n`

---

```
/\n((?m)^b)/
```
(no 578) succeeded 1 of 1 times:

- ✅ `a\nb\n`

---

```
/((?s).)c(?!.)/
```
(no 579) succeeded 2 of 2 times:

- ✅ `a\nb\nc\n`
- ✅ `a\nb\nc\n`

---

```
/((?s)b.)c(?!.)/
```
(no 580) succeeded 2 of 2 times:

- ✅ `a\nb\nc\n`
- ✅ `a\nb\nc\n`

---

```
/^b/
```
(no 581) succeeded 0 of 0 times:


---

```
/()^b/
```
(no 582) succeeded 2 of 2 times:

- ✅ `a\nb\nc\n` no match
- ✅ `a\nb\nc\n` no match

---

```
/((?m)^b)/
```
(no 583) succeeded 1 of 1 times:

- ✅ `a\nb\nc\n`

---

```
/(x)?(?(1)a|b)/
```
(no 584) succeeded 2 of 2 times:

- ✅ `a` no match
- ✅ `a` no match

---

```
/(x)?(?(1)b|a)/
```
(no 585) succeeded 1 of 1 times:

- ✅ `a`

---

```
/()(?(1)b|a)/
```
(no 587) succeeded 0 of 0 times:


---

```
/()?(?(1)a|b)/
```
(no 588) succeeded 1 of 1 times:

- ✅ `a`

---

```
/^(\()?blah(?(1)(\)))$/
```
(no 589) succeeded 5 of 5 times:

- ✅ `(blah)`
- ✅ `blah`
- ✅ `a` no match
- ✅ `blah)` no match
- ✅ `(blah` no match

---

```
/^(\(+)?blah(?(1)(\)))$/
```
(no 590) succeeded 4 of 4 times:

- ✅ `(blah)`
- ✅ `blah`
- ✅ `blah)` no match
- ✅ `(blah` no match

---

```
/(?(?!a)a|b)/
```
(no 591) succeeded 0 of 0 times:


---

```
/(?(?!a)b|a)/
```
(no 592) succeeded 1 of 1 times:

- ✅ `a`

---

```
/(?(?=a)b|a)/
```
(no 593) succeeded 2 of 2 times:

- ✅ `a` no match
- ✅ `a` no match

---

```
/(?(?=a)a|b)/
```
(no 594) succeeded 1 of 1 times:

- ✅ `a`

---

```
/(?=(a+?))(\1ab)/
```
(no 595) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/^(?=(a+?))\1ab/
```
(no 596) succeeded 0 of 0 times:


---

```
/(\w+:)+/
```
(no 597) succeeded 1 of 1 times:

- ✅ `one:`

---

```
/$(?<=^(a))/
```
(no 598) succeeded 1 of 1 times:

- ✅ `a`

---

```
/(?=(a+?))(\1ab)/
```
(no 599) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/^(?=(a+?))\1ab/
```
(no 600) succeeded 2 of 2 times:

- ✅ `aaab` no match
- ✅ `aaab` no match

---

```
/([\w:]+::)?(\w+)$/
```
(no 601) succeeded 2 of 2 times:

- ✅ `abcd`
- ✅ `xy:z:::abcd`

---

```
/^[^bcd]*(c+)/
```
(no 602) succeeded 1 of 1 times:

- ✅ `aexycd`

---

```
/(a*)b+/
```
(no 603) succeeded 1 of 1 times:

- ✅ `caab`

---

```
/([\w:]+::)?(\w+)$/
```
(no 604) succeeded 4 of 4 times:

- ✅ `abcd`
- ✅ `xy:z:::abcd`
- ✅ `abcd:` no match
- ✅ `abcd:` no match

---

```
/^[^bcd]*(c+)/
```
(no 605) succeeded 1 of 1 times:

- ✅ `aexycd`

---

```
/(>a+)ab/
```
(no 606) succeeded 0 of 0 times:


---

```
/(?>a+)b/
```
(no 607) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/([[:]+)/
```
(no 608) succeeded 1 of 1 times:

- ✅ `a:[b]:`

---

```
/([[=]+)/
```
(no 609) succeeded 1 of 1 times:

- ✅ `a=[b]=`

---

```
/([[.]+)/
```
(no 610) succeeded 1 of 1 times:

- ✅ `a.[b].`

---

```
/((?>a+)b)/
```
(no 611) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/(?>(a+))b/
```
(no 612) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/((?>[^()]+)|\([^()]*\))+/
```
(no 613) succeeded 1 of 1 times:

- ✅ `((abc(ade)ufh()()x`

---

```
/a\Z/
```
(no 614) succeeded 2 of 2 times:

- ✅ `aaab` no match
- ✅ `a\nb\n` no match

---

```
/b\Z/
```
(no 615) succeeded 1 of 1 times:

- ✅ `a\nb\n`

---

```
/b\z/
```
(no 616) succeeded 0 of 0 times:


---

```
/b\Z/
```
(no 617) succeeded 1 of 1 times:

- ✅ `a\nb`

---

```
/b\z/
```
(no 618) succeeded 2 of 2 times:

- ✅ `a\nb`
- ✅ ``

---

```
/^(?>(?(1)\.|())[^\W_](?>[a-z0-9-]*[^\W_])?)+$/
```
(no 619) succeeded 22 of 22 times:

- ✅ `a`
- ✅ `abc`
- ✅ `a-b`
- ✅ `0-9`
- ✅ `a.b`
- ✅ `5.6.7`
- ✅ `the.quick.brown.fox`
- ✅ `a100.b200.300c`
- ✅ `12-ab.1245`
- ✅ `` no match
- ✅ `.a` no match
- ✅ `-a` no match
- ✅ `a-` no match
- ✅ `a.` no match
- ✅ `a_b` no match
- ✅ `a.-` no match
- ✅ `a..` no match
- ✅ `ab..bc` no match
- ✅ `the.quick.brown.fox-` no match
- ✅ `the.quick.brown.fox.` no match
- ✅ `the.quick.brown.fox_` no match
- ✅ `the.quick.brown.fox+` no match

---

```
/(?>.*)(?<=(abcd|wxyz))/
```
(no 620) succeeded 3 of 3 times:

- ✅ `alphabetabcd`
- ✅ `endingwxyz`
- ✅ `a rather long string that doesn't end with one of them` no match

---

```
/word (?>(?:(?!otherword)[a-zA-Z0-9]+ ){0,30})otherword/
```
(no 621) succeeded 3 of 3 times:

- ✅ `word cat dog elephant mussel cow horse canary baboon snake shark otherword`
- ✅ `word cat dog elephant mussel cow horse canary baboon snake shark` no match
- ✅ `` no match

---

```
/word (?>[a-zA-Z0-9]+ ){0,30}otherword/
```
(no 622) succeeded 1 of 1 times:

- ✅ `word cat dog elephant mussel cow horse canary baboon snake shark the quick brown fox and the lazy dog and several other words getting close to thirty by now I hope` no match

---

```
/(?<=\d{3}(?!999))foo/
```
(no 623) succeeded 3 of 3 times:

- ✅ `999foo`
- ✅ `123999foo`
- ✅ `123abcfoo` no match

---

```
/(?<=(?!...999)\d{3})foo/
```
(no 624) succeeded 3 of 3 times:

- ✅ `999foo`
- ✅ `123999foo`
- ✅ `123abcfoo` no match

---

```
/(?<=\d{3}...)(?<!999)foo/
```
(no 626) succeeded 3 of 3 times:

- ✅ `123abcfoo`
- ✅ `123456foo`
- ✅ `123999foo` no match

---

```
/<a[\s]+href[\s]*=[\s]*          # find <a href=
 ([\"\'])?                       # find single or double quote
 (?(1) (.*?)\1 | ([^\s]+))       # if quote found, match up to next matching
                                 # quote, otherwise match up to next space
/isx
```
(no 627) succeeded 3 of 3 times:

- ✅ `<a href=abcd xyz`
- ✅ `<a href=\"abcd xyz pqr\" cats`
- ✅ `<a href='abcd xyz pqr' cats`

---

```
/<a\s+href\s*=\s*                # find <a href=
 (["'])?                         # find single or double quote
 (?(1) (.*?)\1 | (\S+))          # if quote found, match up to next matching
                                 # quote, otherwise match up to next space
/isx
```
(no 628) succeeded 3 of 3 times:

- ✅ `<a href=abcd xyz`
- ✅ `<a href=\"abcd xyz pqr\" cats`
- ✅ `<a href       =       'abcd xyz pqr' cats`

---

```
/<a\s+href(?>\s*)=(?>\s*)        # find <a href=
 (["'])?                         # find single or double quote
 (?(1) (.*?)\1 | (\S+))          # if quote found, match up to next matching
                                 # quote, otherwise match up to next space
/isx
```
(no 629) succeeded 3 of 3 times:

- ✅ `<a href=abcd xyz`
- ✅ `<a href=\"abcd xyz pqr\" cats`
- ✅ `<a href       =       'abcd xyz pqr' cats`

---

```
/((Z)+|A)*/
```
(no 630) succeeded 1 of 1 times:

- ✅ `ZABCDEFG`

---

```
/(Z()|A)*/
```
(no 631) succeeded 1 of 1 times:

- ✅ `ZABCDEFG`

---

```
/(Z(())|A)*/
```
(no 632) succeeded 1 of 1 times:

- ✅ `ZABCDEFG`

---

```
/((?>Z)+|A)*/
```
(no 633) succeeded 1 of 1 times:

- ✅ `ZABCDEFG`

---

```
/((?>)+|A)*/
```
(no 634) succeeded 1 of 1 times:

- ✅ `ZABCDEFG`

---

```
/[[:space:]]+/
```
(no 636) succeeded 2 of 2 times:

- ✅ `> \t\n\f\r\v<`
- ✅ ``

---

```
/[[:blank:]]+/
```
(no 637) succeeded 2 of 2 times:

- ✅ `> \t\n\f\r\v<`
- ✅ ``

---

```
/[\s]+/
```
(no 638) succeeded 2 of 2 times:

- ✅ `> \t\n\f\r\v<`
- ✅ ``

---

```
/\s+/
```
(no 639) succeeded 2 of 2 times:

- ✅ `> \t\n\f\r\v<`
- ✅ ``

---

```
/ab/x
```
(no 640) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/(?!\A)x/m
```
(no 641) succeeded 1 of 1 times:

- ✅ `a\nxb\n`

---

```
/(?!^)x/m
```
(no 642) succeeded 1 of 1 times:

- ✅ `a\nxb\n` no match

---

```
/abc\Qabc\Eabc/
```
(no 643) succeeded 2 of 2 times:

- ✅ `abcabcabc`
- ✅ ``

---

```
/abc\Q(*+|\Eabc/
```
(no 644) succeeded 1 of 1 times:

- ✅ `abc(*+|abc`

---

```
/   abc\Q abc\Eabc/x
```
(no 645) succeeded 3 of 3 times:

- ✅ `abc abcabc`
- ✅ `abcabcabc` no match
- ✅ `` no match

---

```
/abc#comment
    \Q#not comment
    literal\E/x
```
(no 646) succeeded 1 of 1 times:

- ✅ `abc#not comment\n    literal`

---

```
/abc#comment
    \Q#not comment
    literal/x
```
(no 647) succeeded 1 of 1 times:

- ✅ `abc#not comment\n    literal`

---

```
/abc#comment
    \Q#not comment
    literal\E #more comment
    /x
```
(no 648) succeeded 1 of 1 times:

- ✅ `abc#not comment\n    literal`

---

```
/abc#comment
    \Q#not comment
    literal\E #more comment/x
```
(no 649) succeeded 1 of 1 times:

- ✅ `abc#not comment\n    literal`

---

```
/\Qabc\$xyz\E/
```
(no 650) succeeded 1 of 1 times:

- ✅ `abc\\$xyz`

---

```
/\Qabc\E\$\Qxyz\E/
```
(no 651) succeeded 1 of 1 times:

- ✅ `abc$xyz`

---

```
/\Gabc/
```
(no 652) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ `xyzabc` no match

---

```
/a(?x: b c )d/
```
(no 655) succeeded 2 of 2 times:

- ✅ `XabcdY`
- ✅ `Xa b c d Y` no match

---

```
/((?x)x y z | a b c)/
```
(no 656) succeeded 2 of 2 times:

- ✅ `XabcY`
- ✅ `AxyzB`

---

```
/(?i)AB(?-i)C/
```
(no 657) succeeded 2 of 2 times:

- ✅ `XabCY`
- ✅ `XabcY` no match

---

```
/((?i)AB(?-i)C|D)E/
```
(no 658) succeeded 6 of 6 times:

- ✅ `abCE`
- ✅ `DE`
- ✅ `abcE` no match
- ✅ `abCe` no match
- ✅ `dE` no match
- ✅ `De` no match

---

```
/(.*)\d+\1/
```
(no 659) succeeded 2 of 2 times:

- ✅ `abc123abc`
- ✅ `abc123bc`

---

```
/(.*)\d+\1/s
```
(no 660) succeeded 3 of 3 times:

- ✅ `abc123abc`
- ✅ `abc123bc`
- ✅ ``

---

```
/((.*))\d+\1/
```
(no 661) succeeded 2 of 2 times:

- ✅ `abc123abc`
- ✅ `abc123bc`

---

```
/^(?!:)                       # colon disallowed at start
  (?:                         # start of item
    (?: [0-9a-f]{1,4} |       # 1-4 hex digits or
    (?(1)0 | () ) )           # if null previously matched, fail; else null
    :                         # followed by colon
  ){1,7}                      # end item; 1-7 of them required               
  [0-9a-f]{1,4} $             # final hex number at end of string
  (?(1)|.)                    # check that there was an empty component
  /ix
```
(no 662) succeeded 14 of 14 times:

- ✅ `a123::a123`
- ✅ `a123:b342::abcd`
- ✅ `a123:b342::324e:abcd`
- ✅ `a123:ddde:b342::324e:abcd`
- ✅ `a123:ddde:b342::324e:dcba:abcd`
- ✅ `a123:ddde:9999:b342::324e:dcba:abcd`
- ✅ `1:2:3:4:5:6:7:8` no match
- ✅ `a123:bce:ddde:9999:b342::324e:dcba:abcd` no match
- ✅ `a123::9999:b342::324e:dcba:abcd` no match
- ✅ `abcde:2:3:4:5:6:7:8` no match
- ✅ `::1` no match
- ✅ `abcd:fee0:123::` no match
- ✅ `:1` no match
- ✅ `1:` no match

---

```
/[z\Qa-d]\E]/
```
(no 663) succeeded 6 of 6 times:

- ✅ `z`
- ✅ `a`
- ✅ `-`
- ✅ `d`
- ✅ `]`
- ✅ `b` no match

---

```
/(?i)reg(?:ul(?:[a\344]|ae)r|ex)/
```
(no 665) succeeded 4 of 4 times:

- ✅ `REGular`
- ✅ `regulaer`
- ✅ `Regex`
- ✅ `regulär`

---

```
/\305\346\345\344[\340-\377\300-\337]+/
```
(no 666) succeeded 4 of 4 times:

- ✅ `Åæåäà`
- ✅ `Åæåäÿ`
- ✅ `ÅæåäÀ`
- ✅ `Åæåäß`

---

```
/(?<=Z)X./
```
(no 667) succeeded 0 of 0 times:


---

```
/ab cd (?x) de fg/
```
(no 668) succeeded 1 of 1 times:

- ✅ `ab cd defg`

---

```
/ab cd(?x) de fg/
```
(no 669) succeeded 2 of 2 times:

- ✅ `ab cddefg`
- ✅ `abcddefg` no match

---

```
/(?<![^f]oo)(bar)/
```
(no 670) succeeded 2 of 2 times:

- ✅ `foobarX`
- ✅ `boobarX` no match

---

```
/(?<![^f])X/
```
(no 671) succeeded 2 of 2 times:

- ✅ `offX`
- ✅ `onyX` no match

---

```
/(?<=[^f])X/
```
(no 672) succeeded 2 of 2 times:

- ✅ `onyX`
- ✅ `offX` no match

---

```
/(?:(?(1)a|b)(X))+/
```
(no 675) succeeded 1 of 1 times:

- ✅ `bXaX`

---

```
/()()()()()()()()()(?:(?(10)\10a|b)(X|Y))+/
```
(no 677) succeeded 1 of 1 times:

- ✅ `bXXaYYaY`

---

```
/[[,abc,]+]/
```
(no 678) succeeded 3 of 3 times:

- ✅ `abc]`
- ✅ `a,b]`
- ✅ `[a,b,c]`

---

```
/(?-x: )/x
```
(no 679) succeeded 2 of 2 times:

- ✅ `A B`
- ✅ ``

---

```
/(?x)(?-x: \s*#\s*)/
```
(no 680) succeeded 2 of 2 times:

- ✅ `A # B`
- ✅ `#` no match

---

```
/(?x-is)(?:(?-ixs) \s*#\s*) include/
```
(no 681) succeeded 3 of 3 times:

- ✅ `A #include`
- ✅ `A#include` no match
- ✅ `A #Include` no match

---

```
/a*b*\w/
```
(no 682) succeeded 3 of 3 times:

- ✅ `aaabbbb`
- ✅ `aaaa`
- ✅ `a`

---

```
/a*b?\w/
```
(no 683) succeeded 3 of 3 times:

- ✅ `aaabbbb`
- ✅ `aaaa`
- ✅ `a`

---

```
/a*b{0,4}\w/
```
(no 684) succeeded 3 of 3 times:

- ✅ `aaabbbb`
- ✅ `aaaa`
- ✅ `a`

---

```
/a*b{0,}\w/
```
(no 685) succeeded 4 of 4 times:

- ✅ `aaabbbb`
- ✅ `aaaa`
- ✅ `a`
- ✅ ``

---

```
/a*\d*\w/
```
(no 686) succeeded 3 of 3 times:

- ✅ `0a`
- ✅ `a`
- ✅ ``

---

```
/a*b *\w/x
```
(no 687) succeeded 1 of 1 times:

- ✅ `a`

---

```
/a*b#comment
  *\w/x
```
(no 688) succeeded 1 of 1 times:

- ✅ `a`

---

```
/a* b *\w/x
```
(no 689) succeeded 1 of 1 times:

- ✅ `a`

---

```
/^\w+=.*(\\\n.*)*/
```
(no 690) succeeded 1 of 1 times:

- ✅ `abc=xyz\\\npqr`

---

```
/(?=(\w+))\1:/
```
(no 691) succeeded 1 of 1 times:

- ✅ `abcd:`

---

```
/^(?=(\w+))\1:/
```
(no 692) succeeded 1 of 1 times:

- ✅ `abcd:`

---

```
/^[\Eabc]/
```
(no 694) succeeded 3 of 3 times:

- ✅ `a`
- ✅ `E` no match
- ✅ `` no match

---

```
/^[a\Q]bc\E]/
```
(no 698) succeeded 4 of 4 times:

- ✅ `a`
- ✅ `]`
- ✅ `c`
- ✅ ``

---

```
/^[a-\Q\E]/
```
(no 699) succeeded 2 of 2 times:

- ✅ `a`
- ✅ `-`

---

```
/^(a()*)*/
```
(no 700) succeeded 1 of 1 times:

- ✅ `aaaa`

---

```
/^(?:a(?:(?:))*)*/
```
(no 701) succeeded 1 of 1 times:

- ✅ `aaaa`

---

```
/^(a()+)+/
```
(no 702) succeeded 1 of 1 times:

- ✅ `aaaa`

---

```
/^(?:a(?:(?:))+)+/
```
(no 703) succeeded 1 of 1 times:

- ✅ `aaaa`

---

```
/(a){0,3}(?(1)b|(c|))*D/
```
(no 704) succeeded 3 of 3 times:

- ✅ `abbD`
- ✅ `ccccD`
- ✅ `D`

---

```
/(a|)*\d/
```
(no 705) succeeded 2 of 2 times:

- ✅ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4`
- ✅ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---

```
/(?>a|)*\d/
```
(no 706) succeeded 2 of 2 times:

- ✅ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4`
- ✅ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---

```
/(?:a|)*\d/
```
(no 707) succeeded 2 of 2 times:

- ✅ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4`
- ✅ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---

```
/^(?s)(?>.*)(?<!\n)/
```
(no 709) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ `abc\n` no match

---

```
/^(?![^\n]*\n\z)/
```
(no 710) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ `abc\n` no match

---

```
/\z(?<!\n)/
```
(no 711) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ `abc\n` no match

---

```
/(.*(.)?)*/
```
(no 712) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/( (A | (?(1)0|) )*   )/x
```
(no 713) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/( ( (?(1)0|) )*   )/x
```
(no 714) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/(  (?(1)0|)*   )/x
```
(no 715) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/[[:abcd:xyz]]/
```
(no 716) succeeded 3 of 3 times:

- ✅ `a]`
- ✅ `:]`
- ✅ ``

---

```
/[abc[:x\]pqr]/
```
(no 717) succeeded 5 of 5 times:

- ✅ `a`
- ✅ `[`
- ✅ `:`
- ✅ `]`
- ✅ `p`

---

```
/.*[op][xyz]/
```
(no 718) succeeded 1 of 1 times:

- ✅ `fooabcfoo` no match

---

```
/(?(?=.*b)b|^)/
```
(no 719) succeeded 2 of 2 times:

- ✅ `adc`
- ✅ `abc`

---

```
/(?(?=^.*b)b|^)/
```
(no 720) succeeded 2 of 2 times:

- ✅ `adc`
- ✅ `abc` no match

---

```
/(?(?=.*b)b|^)*/
```
(no 721) succeeded 2 of 2 times:

- ✅ `adc`
- ✅ `abc`

---

```
/(?(?=.*b)b|^)+/
```
(no 722) succeeded 2 of 2 times:

- ✅ `adc`
- ✅ `abc`

---

```
/(?(?=b).*b|^d)/
```
(no 723) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/(?(?=.*b).*b|^d)/
```
(no 724) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/^%((?(?=[a])[^%])|b)*%$/
```
(no 725) succeeded 1 of 1 times:

- ✅ `%ab%`

---

```
/(?i)a(?-i)b|c/
```
(no 726) succeeded 4 of 4 times:

- ✅ `XabX`
- ✅ `XAbX`
- ✅ `CcC`
- ✅ `XABX` no match

---

```
/[\x00-\xff\s]+/
```
(no 727) succeeded 1 of 1 times:

- ✅ `\n\v\f\r`

---

```
/(abc)\1/i
```
(no 728) succeeded 1 of 1 times:

- ✅ `abc` no match

---

```
/(abc)\1/
```
(no 729) succeeded 1 of 1 times:

- ✅ `abc` no match

---

```
/[^a]*/i
```
(no 730) succeeded 2 of 2 times:

- ✅ `12abc`
- ✅ `12ABC`

---

```
/[^a]*+/i
```
(no 731) succeeded 2 of 2 times:

- ✅ `12abc`
- ✅ `12ABC`

---

```
/[^a]*?X/i
```
(no 732) succeeded 3 of 3 times:

- ✅ `12abc` no match
- ✅ `12ABC` no match
- ✅ `` no match

---

```
/[^a]+?X/i
```
(no 733) succeeded 2 of 2 times:

- ✅ `12abc` no match
- ✅ `12ABC` no match

---

```
/[^a]?X/i
```
(no 734) succeeded 3 of 3 times:

- ✅ `12aXbcX`
- ✅ `12AXBCX`
- ✅ `BCX`

---

```
/[^a]??X/i
```
(no 735) succeeded 4 of 4 times:

- ✅ `12aXbcX`
- ✅ `12AXBCX`
- ✅ `BCX`
- ✅ ``

---

```
/[^a]?+X/i
```
(no 736) succeeded 3 of 3 times:

- ✅ `12aXbcX`
- ✅ `12AXBCX`
- ✅ `BCX`

---

```
/[^a]{2,3}/i
```
(no 737) succeeded 2 of 2 times:

- ✅ `abcdef`
- ✅ `ABCDEF`

---

```
/[^a]{2,3}?/i
```
(no 738) succeeded 2 of 2 times:

- ✅ `abcdef`
- ✅ `ABCDEF`

---

```
/[^a]{2,3}+/i
```
(no 739) succeeded 2 of 2 times:

- ✅ `abcdef`
- ✅ `ABCDEF`

---

```
/((a|)+)+Z/
```
(no 740) succeeded 1 of 1 times:

- ✅ `Z`

---

```
/(a)b|(a)c/
```
(no 741) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/(?>(a))b|(a)c/
```
(no 742) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/(?=(a))ab|(a)c/
```
(no 743) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/((?>(a))b|(a)c)/
```
(no 744) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/((?>(a))b|(a)c)++/
```
(no 745) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/(?:(?>(a))b|(a)c)++/
```
(no 746) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/(?=(?>(a))b|(a)c)(..)/
```
(no 747) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/(?>(?>(a))b|(a)c)/
```
(no 748) succeeded 1 of 1 times:

- ✅ `ac`

---

```
/((?>(a+)b)+(aabab))/
```
(no 751) succeeded 1 of 1 times:

- ✅ `aaaabaaabaabab`

---

```
/(?>a+|ab)+?c/
```
(no 752) succeeded 1 of 1 times:

- ✅ `aabc` no match

---

```
/(?>a+|ab)+c/
```
(no 753) succeeded 1 of 1 times:

- ✅ `aabc` no match

---

```
/(?:a+|ab)+c/
```
(no 754) succeeded 1 of 1 times:

- ✅ `aabc`

---

```
/(?(?=(a))a)/
```
(no 755) succeeded 1 of 1 times:

- ✅ `a`

---

```
/(?(?=(a))a)(b)/
```
(no 756) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/^(?:a|ab)++c/
```
(no 757) succeeded 1 of 1 times:

- ✅ `aaaabc` no match

---

```
/^(?>a|ab)++c/
```
(no 758) succeeded 1 of 1 times:

- ✅ `aaaabc` no match

---

```
/^(?:a|ab)+c/
```
(no 759) succeeded 1 of 1 times:

- ✅ `aaaabc`

---

```
/(?=abc){0}xyz/
```
(no 763) succeeded 1 of 1 times:

- ✅ `xyz`

---

```
/(?=abc){1}xyz/
```
(no 764) succeeded 2 of 2 times:

- ✅ `xyz` no match
- ✅ `` no match

---

```
/(?=(a))?./
```
(no 765) succeeded 3 of 3 times:

- ✅ `ab`
- ✅ `bc`
- ✅ ``

---

```
/(?=(a))??./
```
(no 766) succeeded 2 of 2 times:

- ✅ `ab`
- ✅ `bc`

---

```
/^(?=(?1))?[az]([abc])d/
```
(no 767) succeeded 2 of 2 times:

- ✅ `abd`
- ✅ `zcdxx`

---

```
/^(?!a){0}\w+/
```
(no 768) succeeded 1 of 1 times:

- ✅ `aaaaa`

---

```
/(?<=(abc))?xyz/
```
(no 769) succeeded 2 of 2 times:

- ✅ `abcxyz`
- ✅ `pqrxyz`

---

```
/^[\g<a>]+/
```
(no 770) succeeded 3 of 3 times:

- ✅ `ggg<<<aaa>>>`
- ✅ `\\ga` no match
- ✅ `` no match

---

```
/^[\ga]+/
```
(no 771) succeeded 2 of 2 times:

- ✅ `gggagagaxyz`
- ✅ ``

---

```
/^[:a[:digit:]]+/
```
(no 772) succeeded 1 of 1 times:

- ✅ `aaaa444:::Z`

---

```
/^[:a[:digit:]:b]+/
```
(no 773) succeeded 1 of 1 times:

- ✅ `aaaa444:::bbbZ`

---

```
/[:a]xxx[b:]/
```
(no 774) succeeded 2 of 2 times:

- ✅ `:xxx:`
- ✅ ``

---

```
/(?<=a{2})b/i
```
(no 775) succeeded 2 of 2 times:

- ✅ `xaabc`
- ✅ `xabc` no match

---

```
/(?<!a{2})b/i
```
(no 776) succeeded 2 of 2 times:

- ✅ `xabc`
- ✅ `xaabc` no match

---

```
/(?<=a\h)c/
```
(no 777) succeeded 2 of 2 times:

- ✅ `xa c`
- ✅ ``

---

```
/(?<=[^a]{2})b/
```
(no 778) succeeded 3 of 3 times:

- ✅ `axxbc`
- ✅ `aAAbc`
- ✅ `xaabc` no match

---

```
/(?<=[^a]{2})b/i
```
(no 779) succeeded 3 of 3 times:

- ✅ `axxbc`
- ✅ `aAAbc` no match
- ✅ `xaabc` no match

---

```
/(?<=a\H)c/
```
(no 780) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/(?<=a\V)c/
```
(no 781) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ ``

---

```
/(?<=a\v)c/
```
(no 782) succeeded 1 of 1 times:

- ✅ `a\nc`

---

```
/(?(?=c)c|d)++Y/
```
(no 783) succeeded 1 of 1 times:

- ✅ `XcccddYX`

---

```
/(?(?=c)c|d)*+Y/
```
(no 784) succeeded 1 of 1 times:

- ✅ `XcccddYX`

---

```
/^(a{2,3}){2,}+a/
```
(no 785) succeeded 3 of 3 times:

- ✅ `aaaaaaa`
- ✅ `aaaaaa` no match
- ✅ `aaaaaaaaa` no match

---

```
/^(a{2,3})++a/
```
(no 786) succeeded 1 of 1 times:

- ✅ `aaaaaa` no match

---

```
/^(a{2,3})*+a/
```
(no 787) succeeded 1 of 1 times:

- ✅ `aaaaaa` no match

---

```
/\H\h\V\v/
```
(no 788) succeeded 4 of 4 times:

- ✅ `X X\n`
- ✅ `X\tX\v`
- ✅ `  X\n` no match
- ✅ `` no match

---

```
/\H*\h+\V?\v{3,4}/
```
(no 789) succeeded 5 of 5 times:

- ✅ `\t  X\n\v\f\r\n`
- ✅ `\t  \n\v\f\r\n`
- ✅ `\t  \n\v\f`
- ✅ `\t  \n\v` no match
- ✅ `` no match

---

```
/\H{3,4}/
```
(no 790) succeeded 3 of 3 times:

- ✅ `XY  ABCDE`
- ✅ `XY  PQR ST`
- ✅ ``

---

```
/.\h{3,4}./
```
(no 791) succeeded 1 of 1 times:

- ✅ `XY  AB    PQRS`

---

```
/\h*X\h?\H+Y\H?Z/
```
(no 792) succeeded 4 of 4 times:

- ✅ `>XNNNYZ`
- ✅ `>  X NYQZ`
- ✅ `>XYZ` no match
- ✅ `>  X NY Z` no match

---

```
/\v*X\v?Y\v+Z\V*\x0a\V+\x0b\V{2,3}\x0c/
```
(no 793) succeeded 2 of 2 times:

- ✅ `>XY\nZ\nA\vNN\f`
- ✅ `>\n\rX\nY\n\vZZZ\nAAA\vNNN\f`

---

```
/^(a(b))\1\g1\g{1}\g-1\g{-1}\g{-2}Z/
```
(no 801) succeeded 1 of 1 times:

- ✅ `ababababbbabZXXXX`

---

```
/(?<A>tom|bon)-\g{A}/
```
(no 802) succeeded 3 of 3 times:

- ✅ `tom-tom`
- ✅ `bon-bon`
- ✅ ``

---

```
/(^(a|b\g{-1}))/
```
(no 803) succeeded 1 of 1 times:

- ✅ `bacxxx` no match

---

```
/(?|(abc)|(xyz))\1/
```
(no 804) succeeded 5 of 5 times:

- ✅ `abcabc`
- ✅ `xyzxyz`
- ✅ `abcxyz` no match
- ✅ `xyzabc` no match
- ✅ `` no match

---

```
/(?|(abc)|(xyz))(?1)/
```
(no 805) succeeded 4 of 4 times:

- ✅ `abcabc`
- ✅ `xyzabc`
- ✅ `xyzxyz` no match
- ✅ `` no match

---

```
/^X(?5)(a)(?|(b)|(q))(c)(d)(Y)/
```
(no 806) succeeded 1 of 1 times:

- ✅ `XYabcdY`

---

```
/^X(?7)(a)(?|(b|(r)(s))|(q))(c)(d)(Y)/
```
(no 807) succeeded 1 of 1 times:

- ✅ `XYabcdY`

---

```
/^X(?7)(a)(?|(b|(?|(r)|(t))(s))|(q))(c)(d)(Y)/
```
(no 808) succeeded 1 of 1 times:

- ✅ `XYabcdY`

---

```
/(?'abc'\w+):\k<abc>{2}/
```
(no 809) succeeded 4 of 4 times:

- ✅ `a:aaxyz`
- ✅ `ab:ababxyz`
- ✅ `a:axyz` no match
- ✅ `ab:abxyz` no match

---

```
/(?'abc'\w+):\g{abc}{2}/
```
(no 810) succeeded 4 of 4 times:

- ✅ `a:aaxyz`
- ✅ `ab:ababxyz`
- ✅ `a:axyz` no match
- ✅ `ab:abxyz` no match

---

```
/^(?<ab>a)? (?(<ab>)b|c) (?('ab')d|e)/x
```
(no 811) succeeded 2 of 2 times:

- ✅ `abd`
- ✅ `ce`

---

```
/^(a.)\g-1Z/
```
(no 812) succeeded 1 of 1 times:

- ✅ `aXaXZ`

---

```
/^(a.)\g{-1}Z/
```
(no 813) succeeded 1 of 1 times:

- ✅ `aXaXZ`

---

```
/^(?(DEFINE) (?<A> a) (?<B> b) )  (?&A) (?&B) /x
```
(no 814) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/(?<NAME>(?&NAME_PAT))\s+(?<ADDR>(?&ADDRESS_PAT))
  (?(DEFINE)
  (?<NAME_PAT>[a-z]+)
  (?<ADDRESS_PAT>\d+)
  )/x
```
(no 815) succeeded 1 of 1 times:

- ✅ `metcalfe 33`

---

```
/(?(DEFINE)(?<byte>2[0-4]\d|25[0-5]|1\d\d|[1-9]?\d))\b(?&byte)(\.(?&byte)){3}/
```
(no 816) succeeded 5 of 5 times:

- ✅ `1.2.3.4`
- ✅ `131.111.10.206`
- ✅ `10.0.0.0`
- ✅ `10.6` no match
- ✅ `455.3.4.5` no match

---

```
/\b(?&byte)(\.(?&byte)){3}(?(DEFINE)(?<byte>2[0-4]\d|25[0-5]|1\d\d|[1-9]?\d))/
```
(no 817) succeeded 5 of 5 times:

- ✅ `1.2.3.4`
- ✅ `131.111.10.206`
- ✅ `10.0.0.0`
- ✅ `10.6` no match
- ✅ `455.3.4.5` no match

---

```
/^(\w++|\s++)*$/
```
(no 818) succeeded 2 of 2 times:

- ✅ `now is the time for all good men to come to the aid of the party`
- ✅ `this is not a line with only words and spaces!` no match

---

```
/(\d++)(\w)/
```
(no 819) succeeded 2 of 2 times:

- ✅ `12345a`
- ✅ `12345+` no match

---

```
/a++b/
```
(no 820) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/(a++b)/
```
(no 821) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/(a++)b/
```
(no 822) succeeded 1 of 1 times:

- ✅ `aaab`

---

```
/([^()]++|\([^()]*\))+/
```
(no 823) succeeded 1 of 1 times:

- ✅ `((abc(ade)ufh()()x`

---

```
/\(([^()]++|\([^()]+\))+\)/
```
(no 824) succeeded 3 of 3 times:

- ✅ `(abc)`
- ✅ `(abc(def)xyz)`
- ✅ `((()aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---

```
/^([^()]|\((?1)*\))*$/
```
(no 825) succeeded 4 of 4 times:

- ✅ `abc`
- ✅ `a(b)c`
- ✅ `a(b(c))d`
- ✅ `a(b(c)d` no match

---

```
/^>abc>([^()]|\((?1)*\))*<xyz<$/
```
(no 826) succeeded 3 of 3 times:

- ✅ `>abc>123<xyz<`
- ✅ `>abc>1(2)3<xyz<`
- ✅ `>abc>(1(2)3)<xyz<`

---

```
/^(?:((.)(?1)\2|)|((.)(?3)\4|.))$/i
```
(no 827) succeeded 5 of 5 times:

- ✅ `1221`
- ✅ `Satanoscillatemymetallicsonatas`
- ✅ `AmanaplanacanalPanama`
- ✅ `AblewasIereIsawElba`
- ✅ `Thequickbrownfox` no match

---

```
/^(\d+|\((?1)([+*-])(?1)\)|-(?1))$/
```
(no 828) succeeded 4 of 4 times:

- ✅ `12`
- ✅ `(((2+2)*-3)-7)`
- ✅ `-12`
- ✅ `((2+2)*-3)-7)` no match

---

```
/^(x(y|(?1){2})z)/
```
(no 829) succeeded 4 of 4 times:

- ✅ `xyz`
- ✅ `xxyzxyzz`
- ✅ `xxyzz` no match
- ✅ `xxyzxyzxyzz` no match

---

```
/^a+(*FAIL)/
```
(no 831) succeeded 2 of 2 times:

- ✅ `aaaaaa` no match
- ✅ `` no match

---

```
/a+b?c+(*FAIL)/
```
(no 832) succeeded 1 of 1 times:

- ✅ `aaabccc` no match

---

```
/^\W*+(?:((.)\W*+(?1)\W*+\2|)|((.)\W*+(?3)\W*+\4|\W*+.\W*+))\W*+$/i
```
(no 840) succeeded 5 of 5 times:

- ✅ `1221`
- ✅ `Satan, oscillate my metallic sonatas!`
- ✅ `A man, a plan, a canal: Panama!`
- ✅ `Able was I ere I saw Elba.`
- ✅ `The quick brown fox` no match

---

```
/^((.)(?1)\2|.)$/
```
(no 841) succeeded 8 of 8 times:

- ✅ `a`
- ✅ `aba`
- ✅ `aabaa`
- ✅ `abcdcba`
- ✅ `pqaabaaqp`
- ✅ `ablewasiereisawelba`
- ✅ `rhubarb` no match
- ✅ `the quick brown fox` no match

---

```
/(a)(?<=b(?1))/
```
(no 842) succeeded 3 of 3 times:

- ✅ `baz`
- ✅ `caz` no match
- ✅ `` no match

---

```
/(?<=b(?1))(a)/
```
(no 843) succeeded 3 of 3 times:

- ✅ `zbaaz`
- ✅ `aaa` no match
- ✅ `` no match

---

```
/(?<X>a)(?<=b(?&X))/
```
(no 844) succeeded 1 of 1 times:

- ✅ `baz`

---

```
/^(?|(abc)|(def))\1/
```
(no 845) succeeded 5 of 5 times:

- ✅ `abcabc`
- ✅ `defdef`
- ✅ `abcdef` no match
- ✅ `defabc` no match
- ✅ `` no match

---

```
/^(?|(abc)|(def))(?1)/
```
(no 846) succeeded 4 of 4 times:

- ✅ `abcabc`
- ✅ `defabc`
- ✅ `defdef` no match
- ✅ `abcdef` no match

---

```
/(?:a(?<quote> (?<apostrophe>')|(?<realquote>")) |b(?<quote> (?<apostrophe>')|(?<realquote>")) ) (?('quote')[a-z]+|[0-9]+)/x,dupnames
```
(no 847) succeeded 3 of 3 times:

- ✅ `a\"aaaaa`
- ✅ `b\"aaaaa`
- ✅ `b\"11111` no match

---

```
/(?:(?1)|B)(A(*F)|C)/
```
(no 848) succeeded 3 of 3 times:

- ✅ `ABCD`
- ✅ `CCD`
- ✅ `CAD` no match

---

```
/^(?:(?1)|B)(A(*F)|C)/
```
(no 849) succeeded 5 of 5 times:

- ✅ `CCD`
- ✅ `BCD`
- ✅ `ABCD` no match
- ✅ `CAD` no match
- ✅ `BAD` no match

---

```
/(?(DEFINE)(A))B(?1)C/
```
(no 851) succeeded 1 of 1 times:

- ✅ `BAC`

---

```
/(?(DEFINE)((A)\2))B(?1)C/
```
(no 852) succeeded 1 of 1 times:

- ✅ `BAAC`

---

```
/(?<pn> \( ( [^()]++ | (?&pn) )* \) )/x
```
(no 853) succeeded 1 of 1 times:

- ✅ `(ab(cd)ef)`

---

```
/^([^()]|\((?1)*\))*$/
```
(no 863) succeeded 2 of 2 times:

- ✅ `a(b)c`
- ✅ `a(b(c)d)e`

---

```
/(?P<L1>(?P<L2>0)(?P>L1)|(?P>L2))/
```
(no 864) succeeded 3 of 3 times:

- ✅ `0`
- ✅ `00`
- ✅ `0000`

---

```
/(?P<L1>(?P<L2>0)|(?P>L2)(?P>L1))/
```
(no 865) succeeded 3 of 3 times:

- ✅ `0`
- ✅ `00`
- ✅ `0000`

---

```
/(?&t)(?#()(?(DEFINE)(?<t>a))/
```
(no 881) succeeded 1 of 1 times:

- ✅ `bac`

---

```
/(?i:([^b]))(?1)/
```
(no 888) succeeded 6 of 6 times:

- ✅ `aa`
- ✅ `aA`
- ✅ `ab` no match
- ✅ `aB` no match
- ✅ `Ba` no match
- ✅ `ba` no match

---

```
/^(?&t)*+(?(DEFINE)(?<t>a))\w$/
```
(no 889) succeeded 2 of 2 times:

- ✅ `aaaaaaX`
- ✅ `aaaaaa` no match

---

```
/^(?&t)*(?(DEFINE)(?<t>a))\w$/
```
(no 890) succeeded 2 of 2 times:

- ✅ `aaaaaaX`
- ✅ `aaaaaa`

---

```
/^(a)*+(\w)/
```
(no 891) succeeded 3 of 3 times:

- ✅ `aaaaX`
- ✅ `YZ`
- ✅ `aaaa` no match

---

```
/^(?:a)*+(\w)/
```
(no 892) succeeded 3 of 3 times:

- ✅ `aaaaX`
- ✅ `YZ`
- ✅ `aaaa` no match

---

```
/^(a)++(\w)/
```
(no 893) succeeded 3 of 3 times:

- ✅ `aaaaX`
- ✅ `aaaa` no match
- ✅ `YZ` no match

---

```
/^(?:a)++(\w)/
```
(no 894) succeeded 3 of 3 times:

- ✅ `aaaaX`
- ✅ `aaaa` no match
- ✅ `YZ` no match

---

```
/^(a)?+(\w)/
```
(no 895) succeeded 2 of 2 times:

- ✅ `aaaaX`
- ✅ `YZ`

---

```
/^(?:a)?+(\w)/
```
(no 896) succeeded 2 of 2 times:

- ✅ `aaaaX`
- ✅ `YZ`

---

```
/^(a){2,}+(\w)/
```
(no 897) succeeded 3 of 3 times:

- ✅ `aaaaX`
- ✅ `aaa` no match
- ✅ `YZ` no match

---

```
/^(?:a){2,}+(\w)/
```
(no 898) succeeded 3 of 3 times:

- ✅ `aaaaX`
- ✅ `aaa` no match
- ✅ `YZ` no match

---

```
/(a|)*(?1)b/
```
(no 899) succeeded 3 of 3 times:

- ✅ `b`
- ✅ `ab`
- ✅ `aab`

---

```
/(a)++(?1)b/
```
(no 900) succeeded 2 of 2 times:

- ✅ `ab` no match
- ✅ `aab` no match

---

```
/(a)*+(?1)b/
```
(no 901) succeeded 2 of 2 times:

- ✅ `ab` no match
- ✅ `aab` no match

---

```
/(?1)(?:(b)){0}/
```
(no 902) succeeded 1 of 1 times:

- ✅ `b`

---

```
/(foo ( \( ((?:(?> [^()]+ )|(?2))*) \) ) )/x
```
(no 903) succeeded 1 of 1 times:

- ✅ `foo(bar(baz)+baz(bop))`

---

```
/\A.*?(a|bc)/
```
(no 905) succeeded 1 of 1 times:

- ✅ `ba`

---

```
/\A.*?(?:a|bc)++/
```
(no 906) succeeded 1 of 1 times:

- ✅ `ba`

---

```
/\A.*?(a|bc)++/
```
(no 907) succeeded 1 of 1 times:

- ✅ `ba`

---

```
/\A.*?(?:a|bc|d)/
```
(no 908) succeeded 1 of 1 times:

- ✅ `ba`

---

```
/(?:(b))++/
```
(no 909) succeeded 1 of 1 times:

- ✅ `beetle`

---

```
/^(a)(?1)+ab/
```
(no 911) succeeded 2 of 2 times:

- ✅ `aaaab`
- ✅ ``

---

```
/^(a)(?1)++ab/
```
(no 912) succeeded 1 of 1 times:

- ✅ `aaaab` no match

---

```
/(?(DEFINE)(a))?b(?1)/
```
(no 915) succeeded 1 of 1 times:

- ✅ `backgammon`

---

```
/^\N+/
```
(no 916) succeeded 2 of 2 times:

- ✅ `abc\ndef`
- ✅ ``

---

```
/^\N{1,}/
```
(no 917) succeeded 1 of 1 times:

- ✅ `abc\ndef`

---

```
/(?<!a(*FAIL)b)c/
```
(no 956) succeeded 2 of 2 times:

- ✅ `xcd`
- ✅ `acd`

---

```
/(a)(?2){2}(.)/
```
(no 961) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/(another)?(\1?)test/
```
(no 986) succeeded 1 of 1 times:

- ✅ `hello world test`

---

```
/(another)?(\1+)test/
```
(no 987) succeeded 1 of 1 times:

- ✅ `hello world test` no match

---

```
/((?:a?)*)*c/
```
(no 989) succeeded 1 of 1 times:

- ✅ `aac`

---

```
/((?>a?)*)*c/
```
(no 990) succeeded 1 of 1 times:

- ✅ `aac`

---

```
/(?>.*?a)(?<=ba)/
```
(no 991) succeeded 1 of 1 times:

- ✅ `aba`

---

```
/(?:.*?a)(?<=ba)/
```
(no 992) succeeded 1 of 1 times:

- ✅ `aba`

---

```
/(?>.*?a)b/s
```
(no 993) succeeded 1 of 1 times:

- ✅ `aab`

---

```
/(?>.*?a)b/
```
(no 994) succeeded 1 of 1 times:

- ✅ `aab`

---

```
/(?>^a)b/s
```
(no 995) succeeded 1 of 1 times:

- ✅ `aab` no match

---

```
/(?>.*?)(?<=(abcd)|(wxyz))/
```
(no 996) succeeded 2 of 2 times:

- ✅ `alphabetabcd`
- ✅ `endingwxyz`

---

```
/(?>.*)(?<=(abcd)|(wxyz))/
```
(no 997) succeeded 2 of 2 times:

- ✅ `alphabetabcd`
- ✅ `endingwxyz`

---

```
/(?>.*)foo/
```
(no 998) succeeded 2 of 2 times:

- ✅ `abcdfooxyz` no match
- ✅ `` no match

---

```
/(?>.*?)foo/
```
(no 999) succeeded 1 of 1 times:

- ✅ `abcdfooxyz`

---

```
/a(?=bc).|abd/
```
(no 1036) succeeded 3 of 3 times:

- ✅ `abd`
- ✅ `abc`
- ✅ ``

---

```
/a(?>bc)d|abd/
```
(no 1038) succeeded 1 of 1 times:

- ✅ `abceabd`

---

```
/^(a)?(?(1)a|b)+$/
```
(no 1042) succeeded 1 of 1 times:

- ✅ `a` no match

---

```
/\A.*?(?:a|bc)/
```
(no 1080) succeeded 1 of 1 times:

- ✅ `ba`

---

```
/^\d*\w{4}/
```
(no 1086) succeeded 2 of 2 times:

- ✅ `1234`
- ✅ `123` no match

---

```
/^[^b]*\w{4}/
```
(no 1087) succeeded 2 of 2 times:

- ✅ `aaaa`
- ✅ `aaa` no match

---

```
/^[^b]*\w{4}/i
```
(no 1088) succeeded 2 of 2 times:

- ✅ `aaaa`
- ✅ `aaa` no match

---

```
/^a*\w{4}/
```
(no 1089) succeeded 2 of 2 times:

- ✅ `aaaa`
- ✅ `aaa` no match

---

```
/^a*\w{4}/i
```
(no 1090) succeeded 2 of 2 times:

- ✅ `aaaa`
- ✅ `aaa` no match

---

```
/(?:(?<n>foo)|(?<n>bar))\k<n>/dupnames
```
(no 1091) succeeded 2 of 2 times:

- ✅ `foofoo`
- ✅ `barbar`

---

```
/(?<n>A)(?:(?<n>foo)|(?<n>bar))\k<n>/dupnames
```
(no 1092) succeeded 4 of 4 times:

- ✅ `AfooA`
- ✅ `AbarA`
- ✅ `Afoofoo` no match
- ✅ `Abarbar` no match

---

```
/^(\d+)\s+IN\s+SOA\s+(\S+)\s+(\S+)\s*\(\s*$/
```
(no 1093) succeeded 1 of 1 times:

- ✅ `1 IN SOA non-sp1 non-sp2(`

---

```
/^ (?:(?<A>A)|(?'B'B)(?<A>A)) (?('A')x) (?(<B>)y)$/x,dupnames
```
(no 1094) succeeded 3 of 3 times:

- ✅ `Ax`
- ✅ `BAxy`
- ✅ ``

---

```
/^A\xZ/
```
(no 1095) succeeded 1 of 1 times:

- ✅ `A\0Z`

---

```
/^A\o{123}B/
```
(no 1096) succeeded 1 of 1 times:

- ✅ `ASB`

---

```
/ ^ a + + b $ /x
```
(no 1097) succeeded 2 of 2 times:

- ✅ `aaaab`
- ✅ ``

---

```
/ ^ a + #comment
  + b $ /x
```
(no 1098) succeeded 2 of 2 times:

- ✅ `aaaab`
- ✅ ``

---

```
/ ^ a + #comment
  #comment
  + b $ /x
```
(no 1099) succeeded 2 of 2 times:

- ✅ `aaaab`
- ✅ ``

---

```
/ ^ (?> a + ) b $ /x
```
(no 1100) succeeded 1 of 1 times:

- ✅ `aaaab`

---

```
/ ^ ( a + ) + + \w $ /x
```
(no 1101) succeeded 1 of 1 times:

- ✅ `aaaab`

---

```
/(?:x|(?:(xx|yy)+|x|x|x|x|x)|a|a|a)bc/
```
(no 1107) succeeded 1 of 1 times:

- ✅ `acb` no match

---

```
/\A(?:[^\"]++|\"(?:[^\"]*+|\"\")*+\")++/
```
(no 1108) succeeded 1 of 1 times:

- ✅ `NON QUOTED \"QUOT\"\"ED\" AFTER \"NOT MATCHED`

---

```
/\A(?:[^\"]++|\"(?:[^\"]++|\"\")*+\")++/
```
(no 1109) succeeded 1 of 1 times:

- ✅ `NON QUOTED \"QUOT\"\"ED\" AFTER \"NOT MATCHED`

---

```
/\A(?:[^\"]++|\"(?:[^\"]++|\"\")++\")++/
```
(no 1110) succeeded 1 of 1 times:

- ✅ `NON QUOTED \"QUOT\"\"ED\" AFTER \"NOT MATCHED`

---

```
/\A([^\"1]++|[\"2]([^\"3]*+|[\"4][\"5])*+[\"6])++/
```
(no 1111) succeeded 1 of 1 times:

- ✅ `NON QUOTED \"QUOT\"\"ED\" AFTER \"NOT MATCHED`

---

```
/^\w+(?>\s*)(?<=\w)/
```
(no 1112) succeeded 1 of 1 times:

- ✅ `test test`

---

```
/(?P<Name>a)?(?P<Name2>b)?(?(<Name>)c|d)*l/
```
(no 1118) succeeded 4 of 4 times:

- ✅ `acl`
- ✅ `bdl`
- ✅ `adl`
- ✅ `bcl`

---

```
/\sabc/
```
(no 1119) succeeded 1 of 1 times:

- ✅ `\vabc`

---

```
/[\Qa]\E]+/
```
(no 1120) succeeded 1 of 1 times:

- ✅ `aa]]`

---

```
/[\Q]a\E]+/
```
(no 1121) succeeded 1 of 1 times:

- ✅ `aa]]`

---

```
/A((((((((a))))))))\8B/
```
(no 1122) succeeded 1 of 1 times:

- ✅ `AaaB`

---

```
/A(((((((((a)))))))))\9B/
```
(no 1123) succeeded 2 of 2 times:

- ✅ `AaaB`
- ✅ ``

---

```
/A[\8\9]B/
```
(no 1124) succeeded 2 of 2 times:

- ✅ `A8B`
- ✅ `A9B`

---

```
/(|ab)*?d/
```
(no 1125) succeeded 2 of 2 times:

- ✅ `abd`
- ✅ `xyd`

---

```
/(\2|a)(\1)/
```
(no 1127) succeeded 1 of 1 times:

- ✅ `aaa`

---

```
/(\2)(\1)/
```
(no 1128) succeeded 0 of 0 times:


---

```
/Z*(|d*){216}/
```
(no 1129) succeeded 0 of 0 times:


---

```
/(?1)(?#?'){8}(a)/
```
(no 1130) succeeded 1 of 1 times:

- ✅ `baaaaaaaaac`

---

```
/((((((((((((x))))))))))))\12/
```
(no 1131) succeeded 1 of 1 times:

- ✅ `xx`

---

```
/A[\8]B[\9]C/
```
(no 1132) succeeded 1 of 1 times:

- ✅ `A8B9C`

---

```
/(?1)()((((((\1++))\x85)+)|))/
```
(no 1133) succeeded 1 of 1 times:

- ✅ `\u85\u85`

---

```
/(?|(\k'Pm')|(?'Pm'))/
```
(no 1134) succeeded 1 of 1 times:

- ✅ `abcd`

---

```
/(?|(aaa)|(b))\g{1}/
```
(no 1135) succeeded 2 of 2 times:

- ✅ `aaaaaa`
- ✅ `bb`

---

```
/(?|(aaa)|(b))(?1)/
```
(no 1136) succeeded 3 of 3 times:

- ✅ `aaaaaa`
- ✅ `baaa`
- ✅ `bb` no match

---

```
/(?|(aaa)|(b))/
```
(no 1137) succeeded 2 of 2 times:

- ✅ `xaaa`
- ✅ `xbc`

---

```
/(?|(?'a'aaa)|(?'a'b))\k'a'/
```
(no 1138) succeeded 2 of 2 times:

- ✅ `aaaaaa`
- ✅ `bb`

---

```
/(?|(?'a'aaa)|(?'a'b))(?'a'cccc)\k'a'/dupnames
```
(no 1139) succeeded 2 of 2 times:

- ✅ `aaaccccaaa`
- ✅ `bccccb`

---

```
/(?<=a(B){0}c)X/
```
(no 1141) succeeded 1 of 1 times:

- ✅ `acX`

---

```
/(?<DEFINE>b)(?(DEFINE)(a+))(?&DEFINE)/
```
(no 1142) succeeded 2 of 2 times:

- ✅ `bbbb`
- ✅ `baaab` no match

---

```
/(?=.*[A-Z])(?=.*[a-z])(?=.*[0-9])(?=.*[,;:])(?=.{8,16})(?!.*[\s])/
```
(no 1143) succeeded 1 of 1 times:

- ✅ `   Fred:099`

---

```
/(?=.*X)X$/
```
(no 1144) succeeded 1 of 1 times:

- ✅ `  X`

---

```
/(?s)(?=.*?)b/
```
(no 1145) succeeded 1 of 1 times:

- ✅ `aabc`

---

```
/(Z)(a)\2{1,2}?(?-i)\1X/i
```
(no 1146) succeeded 1 of 1 times:

- ✅ `ZaAAZX`

---

```
/(?'c')XX(?'YYYYYYYYYYYYYYYYYYYYYYYCl')/
```
(no 1147) succeeded 0 of 0 times:


---

```
/[s[:digit:]\E-H]+/
```
(no 1148) succeeded 1 of 1 times:

- ✅ `s09-H`

---

```
/[s[:digit:]\Q\E-H]+/
```
(no 1149) succeeded 1 of 1 times:

- ✅ `s09-H`

---

```
/a+(?:|b)a/
```
(no 1150) succeeded 1 of 1 times:

- ✅ `aaaa`

---

```
/X?(R||){3335}/
```
(no 1151) succeeded 0 of 0 times:


---

```
/(?(DEFINE)(?<m> 1? (?=(?<cond>2)?) 1 2 (?('cond')|3)))
    \A
    ()
    (?&m)
    \Z/x
```
(no 1153) succeeded 1 of 1 times:

- ✅ `123`

---

```
/^(?: 
(?: A| (1? (?=(?<cond>2)?) (1) 2 (?('cond')|3)) )
(Z)
)+$/x
```
(no 1154) succeeded 3 of 3 times:

- ✅ `AZ123Z`
- ✅ `AZ12Z` no match
- ✅ `` no match

---

```
/^ (?(DEFINE) ( (?!(a)\2b)..) )   ()(?1)  /x
```
(no 1155) succeeded 3 of 3 times:

- ✅ `acb`
- ✅ `aab` no match
- ✅ `` no match

---

```
/(?>ab|abab){1,5}?M/
```
(no 1156) succeeded 1 of 1 times:

- ✅ `abababababababababababM`

---

```
/(?>ab|abab){2}?M/
```
(no 1157) succeeded 1 of 1 times:

- ✅ `abababM`

---

```
/((?(?=(a))a)+k)/
```
(no 1158) succeeded 1 of 1 times:

- ✅ `bbak`

---

```
/((?(?=(a))a|)+k)/
```
(no 1159) succeeded 1 of 1 times:

- ✅ `bbak`

---

```
/(?(?!(b))a|b)+k/
```
(no 1160) succeeded 1 of 1 times:

- ✅ `ababbalbbadabak`

---

```
/(?!(b))c|b/
```
(no 1161) succeeded 2 of 2 times:

- ✅ `Ab`
- ✅ `Ac`

---

```
/(?=(b))b|c/
```
(no 1162) succeeded 2 of 2 times:

- ✅ `Ab`
- ✅ `Ac`

---

```
/^(.|(.)(?1)\2)$/
```
(no 1163) succeeded 5 of 5 times:

- ✅ `a`
- ✅ `aba`
- ✅ `abcba`
- ✅ `ababa`
- ✅ `abcdcba`

---

```
/^((.)(?1)\2|.?)$/
```
(no 1164) succeeded 8 of 8 times:

- ✅ `a`
- ✅ `aba`
- ✅ `abba`
- ✅ `abcba`
- ✅ `ababa`
- ✅ `abccba`
- ✅ `abcdcba`
- ✅ `abcddcba`

---

```
/^(.)(\1|a(?2))/
```
(no 1165) succeeded 1 of 1 times:

- ✅ `bab`

---

```
/^(.|(.)(?1)?\2)$/
```
(no 1166) succeeded 2 of 2 times:

- ✅ `abcba`
- ✅ ``

---

```
/^(?(?=(a))abc|def)/
```
(no 1167) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/^(?(?!(a))def|abc)/
```
(no 1168) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/^(?1)\d{3}(a)/
```
(no 1171) succeeded 1 of 1 times:

- ✅ `a123a`

---

```
/(?ix)(?(DEFINE)
(?<addr_spec>       (?&local_part) \@ (?&domain) )
(?<angle_addr>      (?&CFWS)?+ < (?&addr_spec) > (?&CFWS)?+ )
(?<atext>           [a-z\d!#\$%&'*+-\x{2f}=?^_`{|}~] )
(?<atom>            (?&CFWS)?+ (?&atext)+ (?&CFWS)?+ )
(?<ccontent>        (?&ctext) | (?&quoted_pair) | (?&comment) )
(?<ctext>           [^\x{9}\x{10}\x{13}\x{7f}-\x{ff}\ ()\\] )
(?<comment>         \( (?: (?&FWS)?+ (?&ccontent) )*+ (?&FWS)?+ \) )
(?<CFWS>            (?: (?&FWS)?+ (?&comment) )* (?# NOT possessive)
                    (?: (?&FWS)?+ (?&comment) | (?&FWS) ) )
(?<dcontent>        (?&dtext) | (?&quoted_pair) )
(?<display_name>    (?&phrase) )
(?<domain>          (?&dot_atom) | (?&domain_literal) )
(?<domain_literal>  (?&CFWS)?+ \[ (?: (?&FWS)?+ (?&dcontent) )* (?&FWS)?+ \]
                    (?&CFWS)?+ )
(?<dot_atom>        (?&CFWS)?+ (?&dot_atom_text) (?&CFWS)?+ )
(?<dot_atom_text>   (?&atext)++ (?: \. (?&atext)++)*+ )
(?<dtext>           [^\x{9}\x{10}\x{13}\x{7f}-\x{ff}\ \[\]\\] )
(?<FWS>             (?: [\t\ ]*+ \n)?+ [\t\ ]++ )
(?<local_part>      (?&dot_atom) | (?&quoted_string)  )
(?<mailbox>         (?&name_addr) | (?&addr_spec) )
(?<name_addr>       (?&display_name)? (?&angle_addr) )
(?<phrase>          (?&word)++ )
(?<qcontent>        (?&qtext) | (?&quoted_pair) )
(?<quoted_pair>     " (?&text) )
(?<quoted_string>   (?&CFWS)?+ " (?: (?&FWS)?+ (?&qcontent))* (?&FWS)?+ "
                    (?&CFWS)?+ )
(?<qtext>           [^\x{9}\x{10}\x{13}\x{7f}-\x{ff}\ "\\] )
(?<text>            [^\r\n] )
(?<word>            (?&atom) | (?&quoted_string) )
) # End DEFINE
^(?&mailbox)$/
```
(no 1172) succeeded 12 of 12 times:

- ✅ `Alan Other <user@dom.ain>`
- ✅ `<user@dom.ain>`
- ✅ `user@dom.ain`
- ✅ `user@[]`
- ✅ `user@[domain literal]`
- ✅ `user@[domain literal with \"[square brackets\"] inside]`
- ✅ `\"A. Other\" <user.1234@dom.ain> (a comment)`
- ✅ `A. Other <user.1234@dom.ain> (a comment)`
- ✅ `\"/s=user/ou=host/o=place/prmd=uu.yy/admd= /c=gb/\"@x400-re.lay`
- ✅ `A missing angle <user@some.where` no match
- ✅ `The quick brown fox` no match
- ✅ `` no match

---

```
/<(?x:[a b])>/xx
```
(no 1174) succeeded 1 of 1 times:

- ✅ `< >`

---

```
/<(?:[a b])>/xx
```
(no 1175) succeeded 1 of 1 times:

- ✅ `< >` no match

---

```
/<(?xxx:[a b])>/
```
(no 1176) succeeded 2 of 2 times:

- ✅ `< >` no match
- ✅ `` no match

---

```
/<(?-x:[a b])>/xx
```
(no 1177) succeeded 1 of 1 times:

- ✅ `< >`

---

```
/[[:digit:]-]+/
```
(no 1178) succeeded 1 of 1 times:

- ✅ `12-24`

---

```
/(?(DEFINE)(?<optional_a>a?)X)^(?&optional_a)a$/
```
(no 1182) succeeded 2 of 2 times:

- ✅ `aa`
- ✅ `a`

---

```
/^(a?)b(?1)a/
```
(no 1183) succeeded 4 of 4 times:

- ✅ `abaa`
- ✅ `aba`
- ✅ `baa`
- ✅ `ba`

---

```
/^(a?)+b(?1)a/
```
(no 1184) succeeded 4 of 4 times:

- ✅ `abaa`
- ✅ `aba`
- ✅ `baa`
- ✅ `ba`

---

```
/^(a?)++b(?1)a/
```
(no 1185) succeeded 4 of 4 times:

- ✅ `abaa`
- ✅ `aba`
- ✅ `baa`
- ✅ `ba`

---

```
/^(a?)+b/
```
(no 1186) succeeded 3 of 3 times:

- ✅ `b`
- ✅ `ab`
- ✅ `aaab`

---

```
/(?=a+)a(a+)++b/
```
(no 1187) succeeded 1 of 1 times:

- ✅ `aab`

---

```
/a(*F:X)b/
```
(no 1202) succeeded 2 of 2 times:

- ✅ `abc` no match
- ✅ `` no match

---

```
/(?(DEFINE)(a(*F:X)))(?1)b/
```
(no 1203) succeeded 1 of 1 times:

- ✅ `abc` no match

---

```
/(?i)A(?^)B(?^x:C D)(?^i)e f/
```
(no 1212) succeeded 3 of 3 times:

- ✅ `aBCDE F`
- ✅ `aBCDEF` no match
- ✅ `AbCDe f` no match

---

```
/(*pla:foo).{6}/
```
(no 1213) succeeded 2 of 2 times:

- ✅ `abcfoobarxyz`
- ✅ `abcfooba` no match

---

```
/(*positive_lookahead:foo).{6}/
```
(no 1214) succeeded 2 of 2 times:

- ✅ `abcfoobarxyz`
- ✅ ``

---

```
/(?(*pla:foo).{6}|a..)/
```
(no 1215) succeeded 2 of 2 times:

- ✅ `foobarbaz`
- ✅ `abcfoobar`

---

```
/(?(*positive_lookahead:foo).{6}|a..)/
```
(no 1216) succeeded 3 of 3 times:

- ✅ `foobarbaz`
- ✅ `abcfoobar`
- ✅ ``

---

```
/(*plb:foo)bar/
```
(no 1217) succeeded 2 of 2 times:

- ✅ `abcfoobar`
- ✅ `abcbarfoo` no match

---

```
/(*positive_lookbehind:foo)bar/
```
(no 1218) succeeded 3 of 3 times:

- ✅ `abcfoobar`
- ✅ `abcbarfoo` no match
- ✅ `` no match

---

```
/(?(*plb:foo)bar|baz)/
```
(no 1219) succeeded 5 of 5 times:

- ✅ `abcfoobar`
- ✅ `bazfoobar`
- ✅ `abcbazfoobar`
- ✅ `foobazfoobar`
- ✅ ``

---

```
/(?(*positive_lookbehind:foo)bar|baz)/
```
(no 1220) succeeded 5 of 5 times:

- ✅ `abcfoobar`
- ✅ `bazfoobar`
- ✅ `abcbazfoobar`
- ✅ `foobazfoobar`
- ✅ ``

---

```
/(*nlb:foo)bar/
```
(no 1221) succeeded 2 of 2 times:

- ✅ `abcbarfoo`
- ✅ `abcfoobar` no match

---

```
/(*negative_lookbehind:foo)bar/
```
(no 1222) succeeded 3 of 3 times:

- ✅ `abcbarfoo`
- ✅ `abcfoobar` no match
- ✅ `` no match

---

```
/(?(*nlb:foo)bar|baz)/
```
(no 1223) succeeded 4 of 4 times:

- ✅ `abcfoobaz`
- ✅ `abcbarbaz`
- ✅ `abcfoobar` no match
- ✅ `` no match

---

```
/(?(*negative_lookbehind:foo)bar|baz)/
```
(no 1224) succeeded 4 of 4 times:

- ✅ `abcfoobaz`
- ✅ `abcbarbaz`
- ✅ `abcfoobar` no match
- ✅ `` no match

---

```
/(*atomic:a+)\w/
```
(no 1225) succeeded 2 of 2 times:

- ✅ `aaab`
- ✅ `aaaa` no match

---

```
/   (?<word> \w+ )*    \.   /xi
```
(no 1226) succeeded 2 of 2 times:

- ✅ `pokus.`
- ✅ ``

---

```
/(?(DEFINE) (?<word> \w+ ) ) (?&word)*   \./xi
```
(no 1227) succeeded 1 of 1 times:

- ✅ `pokus.`

---

```
/(?(DEFINE) (?<word> \w+ ) ) ( (?&word)* )   \./xi
```
(no 1228) succeeded 1 of 1 times:

- ✅ `pokus.`

---

```
/(?&word)*  (?(DEFINE) (?<word> \w+ ) )  \./xi
```
(no 1229) succeeded 1 of 1 times:

- ✅ `pokus.`

---

```
/(?&word)*  \. (?<word> \w+ )/xi
```
(no 1230) succeeded 1 of 1 times:

- ✅ `pokus.hokus`

---

```
/(?:a|ab){1}+c/
```
(no 1233) succeeded 1 of 1 times:

- ✅ `abc` no match

---

```
/(a|ab){1}+c/
```
(no 1234) succeeded 2 of 2 times:

- ✅ `abc` no match
- ✅ `` no match

---

```
/(a+){1}+a/
```
(no 1235) succeeded 1 of 1 times:

- ✅ `aaaa` no match

---

```
/(?(DEFINE)(a|ab))(?1){1}+c/
```
(no 1236) succeeded 1 of 1 times:

- ✅ `abc` no match

---

```
/(?:a|(?=b)|.)*\z/
```
(no 1237) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ ``

---

```
/(?:a|(?=b)|.)*/
```
(no 1238) succeeded 2 of 2 times:

- ✅ `abc`
- ✅ ``

---

```
/(?<=(?=(?<=a)))b/
```
(no 1242) succeeded 1 of 1 times:

- ✅ `ab`

---

```
/^(?<A>a)(?(<A>)b)((?<=b).*)$/
```
(no 1243) succeeded 1 of 1 times:

- ✅ `abc`

---

```
/^(a\1?){4}$/
```
(no 1244) succeeded 2 of 2 times:

- ✅ `aaaa`
- ✅ `aaaaaa`

---

```
/^((\1+)|\d)+133X$/
```
(no 1245) succeeded 1 of 1 times:

- ✅ `111133X`

---

```
/^(?>.*?([A-Z])(?!.*\1)){26}/i
```
(no 1247) succeeded 6 of 6 times:

- ✅ `The quick brown fox jumps over the lazy dog.`
- ✅ `Jackdaws love my big sphinx of quartz.`
- ✅ `Pack my box with five dozen liquor jugs.`
- ✅ `The quick brown fox jumps over the lazy cat.` no match
- ✅ `Hackdaws love my big sphinx of quartz.` no match
- ✅ `Pack my fox with five dozen liquor jugs.` no match

---

```
/(?<=X(?(DEFINE)(A)))X(*F)/
```
(no 1248) succeeded 1 of 1 times:

- ✅ `AXYZ` no match

---

```
/(?<=X(?(DEFINE)(A)))./
```
(no 1249) succeeded 1 of 1 times:

- ✅ `AXYZ`

---

```
/(?<=X(?(DEFINE)(.*))Y)./
```
(no 1250) succeeded 1 of 1 times:

- ✅ `AXYZ`

---

```
/(?<=X(?(DEFINE)(Y))(?1))./
```
(no 1251) succeeded 1 of 1 times:

- ✅ `AXYZ`

---

```
/(?(DEFINE)(?<foo>bar))(?<![-a-z0-9])word/
```
(no 1252) succeeded 1 of 1 times:

- ✅ `word`
