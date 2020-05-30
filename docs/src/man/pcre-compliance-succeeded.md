# PCRE Compliance
3034 successful tests on (success = 1252, failed = 43, skipped = 14, unsupported = 251).success patterns.



---
```
/the quick brown fox/
```
(#1) succeeded 4 times:

- ☑ `the quick brown fox`
- ☑ `What do you know about the quick brown fox?`
- ☑ `The quick brown FOX` no match
- ☑ `What do you know about THE QUICK BROWN FOX?` no match

---
```
/The quick brown fox/i
```
(#2) succeeded 4 times:

- ☑ `the quick brown fox`
- ☑ `The quick brown FOX`
- ☑ `What do you know about the quick brown fox?`
- ☑ `What do you know about THE QUICK BROWN FOX?`

---
```
/abcd\t\n\r\f\a\e\071\x3b\$\\\?caxyz/
```
(#3) succeeded 0 times:

- ☐ `abcd	\n9;$\?caxyz`

---
```
/a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz/
```
(#4) succeeded 36 times:

- ☑ `abxyzpqrrrabbxyyyypqAzz`
- ☑ `abxyzpqrrrabbxyyyypqAzz`
- ☑ `aabxyzpqrrrabbxyyyypqAzz`
- ☑ `aaabxyzpqrrrabbxyyyypqAzz`
- ☑ `aaaabxyzpqrrrabbxyyyypqAzz`
- ☑ `abcxyzpqrrrabbxyyyypqAzz`
- ☑ `aabcxyzpqrrrabbxyyyypqAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypqAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypqqAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypqqqAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypqqqqAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypqqqqqAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypqqqqqqAzz`
- ☑ `aaaabcxyzpqrrrabbxyyyypqAzz`
- ☑ `abxyzzpqrrrabbxyyyypqAzz`
- ☑ `aabxyzzzpqrrrabbxyyyypqAzz`
- ☑ `aaabxyzzzzpqrrrabbxyyyypqAzz`
- ☑ `aaaabxyzzzzpqrrrabbxyyyypqAzz`
- ☑ `abcxyzzpqrrrabbxyyyypqAzz`
- ☑ `aabcxyzzzpqrrrabbxyyyypqAzz`
- ☑ `aaabcxyzzzzpqrrrabbxyyyypqAzz`
- ☑ `aaaabcxyzzzzpqrrrabbxyyyypqAzz`
- ☑ `aaaabcxyzzzzpqrrrabbbxyyyypqAzz`
- ☑ `aaaabcxyzzzzpqrrrabbbxyyyyypqAzz`
- ☑ `aaabcxyzpqrrrabbxyyyypABzz`
- ☑ `aaabcxyzpqrrrabbxyyyypABBzz`
- ☑ `>>>aaabxyzpqrrrabbxyyyypqAzz`
- ☑ `>aaaabxyzpqrrrabbxyyyypqAzz`
- ☑ `>>>>abcxyzpqrrrabbxyyyypqAzz`
- ☑ `abxyzpqrrabbxyyyypqAzz` no match
- ☑ `abxyzpqrrrrabbxyyyypqAzz` no match
- ☑ `abxyzpqrrrabxyyyypqAzz` no match
- ☑ `aaaabcxyzzzzpqrrrabbbxyyyyyypqAzz` no match
- ☑ `aaaabcxyzzzzpqrrrabbbxyyypqAzz` no match
- ☑ `aaabcxyzpqrrrabbxyyyypqqqqqqqAzz` no match

---
```
/^(abc){1,2}zz/
```
(#5) succeeded 5 times:

- ☑ `abczz`
- ☑ `abcabczz`
- ☑ `zz` no match
- ☑ `abcabcabczz` no match
- ☑ `>>abczz` no match

---
```
/^(b+?|a){1,2}?c/
```
(#6) succeeded 10 times:

- ☑ `bc`
- ☑ `bbc`
- ☑ `bbbc`
- ☑ `bac`
- ☑ `bbac`
- ☑ `aac`
- ☑ `abbbbbbbbbbbc`
- ☑ `bbbbbbbbbbbac`
- ☑ `aaac` no match
- ☑ `abbbbbbbbbbbac` no match

---
```
/^(b+|a){1,2}c/
```
(#7) succeeded 10 times:

- ☑ `bc`
- ☑ `bbc`
- ☑ `bbbc`
- ☑ `bac`
- ☑ `bbac`
- ☑ `aac`
- ☑ `abbbbbbbbbbbc`
- ☑ `bbbbbbbbbbbac`
- ☑ `aaac` no match
- ☑ `abbbbbbbbbbbac` no match

---
```
/^(ba|b*){1,2}?bc/
```
(#8) succeeded 5 times:

- ☑ `babc`
- ☑ `bbabc`
- ☑ `bababc`
- ☑ `bababbc` no match
- ☑ `babababc` no match

---
```
/^\ca\cA\c[;\c:/
```
(#9) succeeded 1 times:

- ☑ `;z`

---
```
/^[ab\]cde]/
```
(#10) succeeded 9 times:

- ☑ `athing`
- ☑ `bthing`
- ☑ `]thing`
- ☑ `cthing`
- ☑ `dthing`
- ☑ `ething`
- ☑ `fthing` no match
- ☑ `[thing` no match
- ☑ `\thing` no match

---
```
/^[]cde]/
```
(#11) succeeded 6 times:

- ☑ `]thing`
- ☑ `cthing`
- ☑ `dthing`
- ☑ `ething`
- ☑ `athing` no match
- ☑ `fthing` no match

---
```
/^[^ab\]cde]/
```
(#12) succeeded 9 times:

- ☑ `fthing`
- ☑ `[thing`
- ☑ `\thing`
- ☑ `athing` no match
- ☑ `bthing` no match
- ☑ `]thing` no match
- ☑ `cthing` no match
- ☑ `dthing` no match
- ☑ `ething` no match

---
```
/^[^]cde]/
```
(#13) succeeded 6 times:

- ☑ `athing`
- ☑ `fthing`
- ☑ `]thing` no match
- ☑ `cthing` no match
- ☑ `dthing` no match
- ☑ `ething` no match

---
```
/^\�/
```
(#14) succeeded -1 times:


---
```
/^�/
```
(#15) succeeded 0 times:

- ☐ `�`

---
```
/^[0-9]+$/
```
(#16) succeeded 13 times:

- ☑ `0`
- ☑ `1`
- ☑ `2`
- ☑ `3`
- ☑ `4`
- ☑ `5`
- ☑ `6`
- ☑ `7`
- ☑ `8`
- ☑ `9`
- ☑ `10`
- ☑ `100`
- ☑ `abc` no match

---
```
/^.*nter/
```
(#17) succeeded 3 times:

- ☑ `enter`
- ☑ `inter`
- ☑ `uponter`

---
```
/^xxx[0-9]+$/
```
(#18) succeeded 3 times:

- ☑ `xxx0`
- ☑ `xxx1234`
- ☑ `xxx` no match

---
```
/^.+[0-9][0-9][0-9]$/
```
(#19) succeeded 5 times:

- ☑ `x123`
- ☑ `x1234`
- ☑ `xx123`
- ☑ `123456`
- ☑ `123` no match

---
```
/^.+?[0-9][0-9][0-9]$/
```
(#20) succeeded 5 times:

- ☑ `x123`
- ☑ `x1234`
- ☑ `xx123`
- ☑ `123456`
- ☑ `123` no match

---
```
/^([^!]+)!(.+)=apquxz\.ixr\.zzz\.ac\.uk$/
```
(#21) succeeded 5 times:

- ☑ `abc!pqr=apquxz.ixr.zzz.ac.uk`
- ☑ `!pqr=apquxz.ixr.zzz.ac.uk` no match
- ☑ `abc!=apquxz.ixr.zzz.ac.uk` no match
- ☑ `abc!pqr=apquxz:ixr.zzz.ac.uk` no match
- ☑ `abc!pqr=apquxz.ixr.zzz.ac.ukk` no match

---
```
/:/
```
(#22) succeeded 2 times:

- ☑ `Well, we need a colon: somewhere`
- ☑ `Fail without a colon` no match

---
```
/([\da-f:]+)$/i
```
(#23) succeeded 12 times:

- ☑ `0abc`
- ☑ `abc`
- ☑ `fed`
- ☑ `E`
- ☑ `::`
- ☑ `5f03:12C0::932e`
- ☑ `fed def`
- ☑ `Any old stuff`
- ☑ `0zzz` no match
- ☑ `gzzz` no match
- ☑ `fed ` no match
- ☑ `Any old rubbish` no match

---
```
/^.*\.(\d{1,3})\.(\d{1,3})\.(\d{1,3})$/
```
(#24) succeeded 5 times:

- ☑ `.1.2.3`
- ☑ `A.12.123.0`
- ☑ `.1.2.3333` no match
- ☑ `1.2.3` no match
- ☑ `1234.2.3` no match

---
```
/^(\d+)\s+IN\s+SOA\s+(\S+)\s+(\S+)\s*\(\s*$/
```
(#25) succeeded 3 times:

- ☑ `1 IN SOA non-sp1 non-sp2(`
- ☑ `1    IN    SOA    non-sp1    non-sp2   (`
- ☑ `1IN SOA non-sp1 non-sp2(` no match

---
```
/^[a-zA-Z\d][a-zA-Z\d\-]*(\.[a-zA-Z\d][a-zA-z\d\-]*)*\.$/
```
(#26) succeeded 7 times:

- ☑ `a.`
- ☑ `Z.`
- ☑ `2.`
- ☑ `ab-c.pq-r.`
- ☑ `sxk.zzz.ac.uk.`
- ☑ `x-.y-.`
- ☑ `-abc.peq.` no match

---
```
/^\*\.[a-z]([a-z\-\d]*[a-z\d]+)?(\.[a-z]([a-z\-\d]*[a-z\d]+)?)*$/
```
(#27) succeeded 8 times:

- ☑ `*.a`
- ☑ `*.b0-a`
- ☑ `*.c3-b.c`
- ☑ `*.c-a.b-c`
- ☑ `*.0` no match
- ☑ `*.a-` no match
- ☑ `*.a-b.c-` no match
- ☑ `*.c-a.0-c` no match

---
```
/^(?=ab(de))(abd)(e)/
```
(#28) succeeded 1 times:

- ☑ `abde`

---
```
/^(?!(ab)de|x)(abd)(f)/
```
(#29) succeeded 1 times:

- ☑ `abdf`

---
```
/^(?=(ab(cd)))(ab)/
```
(#30) succeeded 1 times:

- ☑ `abcd`

---
```
/^[\da-f](\.[\da-f])*$/i
```
(#31) succeeded 3 times:

- ☑ `a.b.c.d`
- ☑ `A.B.C.D`
- ☑ `a.b.c.1.2.3.C`

---
```
/^\".*\"\s*(;.*)?$/
```
(#32) succeeded 4 times:

- ☑ `"1234"`
- ☑ `"abcd" ;`
- ☑ `"" ; rhubarb`
- ☑ `"1234" : things` no match

---
```
/^$/
```
(#33) succeeded 1 times:

- ☐ `\`
- ☑ `A non-empty line` no match

---
```
/   ^    a   (?# begins with a)  b\sc (?# then b c) $ (?# then end)/x
```
(#34) succeeded 3 times:

- ☑ `ab c`
- ☑ `abc` no match
- ☑ `ab cde` no match

---
```
/(?x)   ^    a   (?# begins with a)  b\sc (?# then b c) $ (?# then end)/
```
(#35) succeeded 3 times:

- ☑ `ab c`
- ☑ `abc` no match
- ☑ `ab cde` no match

---
```
/^   a\ b[c ]d       $/x
```
(#36) succeeded 4 times:

- ☑ `a bcd`
- ☑ `a b d`
- ☑ `abcd` no match
- ☑ `ab d` no match

---
```
/^(a(b(c)))(d(e(f)))(h(i(j)))(k(l(m)))$/
```
(#37) succeeded 1 times:

- ☑ `abcdefhijklm`

---
```
/^(?:a(b(c)))(?:d(e(f)))(?:h(i(j)))(?:k(l(m)))$/
```
(#38) succeeded 1 times:

- ☑ `abcdefhijklm`

---
```
/^[\w][\W][\s][\S][\d][\D][\b][\n][\c]][\022]/
```
(#39) succeeded 0 times:

- ☐ `a+ Z0+\n`

---
```
/^[.^$|()*+?{,}]+/
```
(#40) succeeded 1 times:

- ☑ `.^$(*+)|{?,?}`

---
```
/^a*\w/
```
(#41) succeeded 8 times:

- ☑ `z`
- ☑ `az`
- ☑ `aaaz`
- ☑ `a`
- ☑ `aa`
- ☑ `aaaa`
- ☑ `a+`
- ☑ `aa+`

---
```
/^a*?\w/
```
(#42) succeeded 8 times:

- ☑ `z`
- ☑ `az`
- ☑ `aaaz`
- ☑ `a`
- ☑ `aa`
- ☑ `aaaa`
- ☑ `a+`
- ☑ `aa+`

---
```
/^a+\w/
```
(#43) succeeded 5 times:

- ☑ `az`
- ☑ `aaaz`
- ☑ `aa`
- ☑ `aaaa`
- ☑ `aa+`

---
```
/^a+?\w/
```
(#44) succeeded 5 times:

- ☑ `az`
- ☑ `aaaz`
- ☑ `aa`
- ☑ `aaaa`
- ☑ `aa+`

---
```
/^\d{8}\w{2,}/
```
(#45) succeeded 4 times:

- ☑ `1234567890`
- ☑ `12345678ab`
- ☑ `12345678__`
- ☑ `1234567` no match

---
```
/^[aeiou\d]{4,5}$/
```
(#46) succeeded 5 times:

- ☑ `uoie`
- ☑ `1234`
- ☑ `12345`
- ☑ `aaaaa`
- ☑ `123456` no match

---
```
/^[aeiou\d]{4,5}?/
```
(#47) succeeded 5 times:

- ☑ `uoie`
- ☑ `1234`
- ☑ `12345`
- ☑ `aaaaa`
- ☑ `123456`

---
```
/\A(abc|def)=(\1){2,3}\Z/
```
(#48) succeeded 3 times:

- ☑ `abc=abcabc`
- ☑ `def=defdefdef`
- ☑ `abc=defdef` no match

---
```
/^(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\11*(\3\4)\1(?#)2$/
```
(#49) succeeded 2 times:

- ☑ `abcdefghijkcda2`
- ☑ `abcdefghijkkkkcda2`

---
```
/(cat(a(ract|tonic)|erpillar)) \1()2(3)/
```
(#50) succeeded 3 times:

- ☑ `cataract cataract23`
- ☑ `catatonic catatonic23`
- ☑ `caterpillar caterpillar23`

---
```
/^From +([^ ]+) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]/
```
(#51) succeeded 1 times:

- ☑ `From abcd  Mon Sep 01 12:33:02 1997`

---
```
/^From\s+\S+\s+([a-zA-Z]{3}\s+){2}\d{1,2}\s+\d\d:\d\d/
```
(#52) succeeded 3 times:

- ☑ `From abcd  Mon Sep 01 12:33:02 1997`
- ☑ `From abcd  Mon Sep  1 12:33:02 1997`
- ☑ `From abcd  Sep 01 12:33:02 1997` no match

---
```
/^12.34/s
```
(#53) succeeded 2 times:

- ☑ `12\n34`
- ☑ `1234`

---
```
/\w+(?=\t)/
```
(#54) succeeded 1 times:

- ☑ `the quick brown	 fox`

---
```
/foo(?!bar)(.*)/
```
(#55) succeeded 1 times:

- ☑ `foobar is foolish see?`

---
```
/(?:(?!foo)...|^.{0,2})bar(.*)/
```
(#56) succeeded 4 times:

- ☑ `foobar crowbar etc`
- ☑ `barrel`
- ☑ `2barrel`
- ☑ `A barrel`

---
```
/^(\D*)(?=\d)(?!123)/
```
(#57) succeeded 2 times:

- ☑ `abc456`
- ☑ `abc123` no match

---
```
/^1234(?# test newlines
  inside)/
```
(#58) succeeded 1 times:

- ☑ `1234`

---
```
/^1234 #comment in extended re
  /x
```
(#59) succeeded 1 times:

- ☑ `1234`

---
```
/#rhubarb
  abcd/x
```
(#60) succeeded 1 times:

- ☑ `abcd`

---
```
/^abcd#rhubarb/x
```
(#61) succeeded 1 times:

- ☑ `abcd`

---
```
/^(a)\1{2,3}(.)/
```
(#62) succeeded 4 times:

- ☑ `aaab`
- ☑ `aaaab`
- ☑ `aaaaab`
- ☑ `aaaaaab`

---
```
/(?!^)abc/
```
(#63) succeeded 2 times:

- ☑ `the abc`
- ☑ `abc` no match

---
```
/(?=^)abc/
```
(#64) succeeded 2 times:

- ☑ `abc`
- ☑ `the abc` no match

---
```
/^[ab]{1,3}(ab*|b)/
```
(#65) succeeded 1 times:

- ☑ `aabbbbb`

---
```
/^[ab]{1,3}?(ab*|b)/
```
(#66) succeeded 1 times:

- ☑ `aabbbbb`

---
```
/^[ab]{1,3}?(ab*?|b)/
```
(#67) succeeded 1 times:

- ☑ `aabbbbb`

---
```
/^[ab]{1,3}(ab*?|b)/
```
(#68) succeeded 1 times:

- ☑ `aabbbbb`

---
```
/  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*                          # optional leading comment
(?:    (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
" (?:                      # opening quote...
[^\\\x80-\xff\n\015"]                #   Anything except backslash and quote
|                     #    or
\\ [^\x80-\xff]           #   Escaped something (something != CR)
)* "  # closing quote
)                    # initial word
(?:  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  \.  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*   (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
" (?:                      # opening quote...
[^\\\x80-\xff\n\015"]                #   Anything except backslash and quote
|                     #    or
\\ [^\x80-\xff]           #   Escaped something (something != CR)
)* "  # closing quote
)  )* # further okay, if led by a period
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  @  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*    (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                           # initial subdomain
(?:                                  #
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  \.                        # if led by a period...
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*   (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                     #   ...further okay
)*
# address
|                     #  or
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
" (?:                      # opening quote...
[^\\\x80-\xff\n\015"]                #   Anything except backslash and quote
|                     #    or
\\ [^\x80-\xff]           #   Escaped something (something != CR)
)* "  # closing quote
)             # one word, optionally followed by....
(?:
[^()<>@,;:".\\\[\]\x80-\xff\000-\010\012-\037]  |  # atom and space parts, or...
\(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)       |  # comments, or...

" (?:                      # opening quote...
[^\\\x80-\xff\n\015"]                #   Anything except backslash and quote
|                     #    or
\\ [^\x80-\xff]           #   Escaped something (something != CR)
)* "  # closing quote
# quoted strings
)*
<  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*                     # leading <
(?:  @  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*    (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                           # initial subdomain
(?:                                  #
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  \.                        # if led by a period...
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*   (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                     #   ...further okay
)*

(?:  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  ,  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  @  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*    (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                           # initial subdomain
(?:                                  #
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  \.                        # if led by a period...
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*   (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                     #   ...further okay
)*
)* # further okay, if led by comma
:                                # closing colon
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  )? #       optional route
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
" (?:                      # opening quote...
[^\\\x80-\xff\n\015"]                #   Anything except backslash and quote
|                     #    or
\\ [^\x80-\xff]           #   Escaped something (something != CR)
)* "  # closing quote
)                    # initial word
(?:  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  \.  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*   (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
" (?:                      # opening quote...
[^\\\x80-\xff\n\015"]                #   Anything except backslash and quote
|                     #    or
\\ [^\x80-\xff]           #   Escaped something (something != CR)
)* "  # closing quote
)  )* # further okay, if led by a period
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  @  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*    (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                           # initial subdomain
(?:                                  #
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  \.                        # if led by a period...
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*   (?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|   \[                         # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*    #    stuff
\]                        #           ]
)                     #   ...further okay
)*
#       address spec
(?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*  > #                  trailing >
# name and address
)  (?: [\040\t] |  \(
(?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  |  \( (?:  [^\\\x80-\xff\n\015()]  |  \\ [^\x80-\xff]  )* \)  )*
\)  )*                       # optional trailing comment
/x
```
(#69) succeeded -1 times:


---
```
/[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional leading comment
(?:
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
# Atom
|                       #  or
"                                     # "
[^\\\x80-\xff\n\015"] *                            #   normal
(?:  \\ [^\x80-\xff]  [^\\\x80-\xff\n\015"] * )*        #   ( special normal* )*
"                                     #        "
# Quoted string
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
\.
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
# Atom
|                       #  or
"                                     # "
[^\\\x80-\xff\n\015"] *                            #   normal
(?:  \\ [^\x80-\xff]  [^\\\x80-\xff\n\015"] * )*        #   ( special normal* )*
"                                     #        "
# Quoted string
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# additional words
)*
@
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
(?:
\.
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
)*
# address
|                             #  or
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
# Atom
|                       #  or
"                                     # "
[^\\\x80-\xff\n\015"] *                            #   normal
(?:  \\ [^\x80-\xff]  [^\\\x80-\xff\n\015"] * )*        #   ( special normal* )*
"                                     #        "
# Quoted string
)
# leading word
[^()<>@,;:".\\\[\]\x80-\xff\000-\010\012-\037] *               # "normal" atoms and or spaces
(?:
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
|
"                                     # "
[^\\\x80-\xff\n\015"] *                            #   normal
(?:  \\ [^\x80-\xff]  [^\\\x80-\xff\n\015"] * )*        #   ( special normal* )*
"                                     #        "
) # "special" comment or quoted string
[^()<>@,;:".\\\[\]\x80-\xff\000-\010\012-\037] *            #  more "normal"
)*
<
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# <
(?:
@
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
(?:
\.
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
)*
(?: ,
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
@
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
(?:
\.
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
)*
)*  # additional domains
:
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
)?     #       optional route
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
# Atom
|                       #  or
"                                     # "
[^\\\x80-\xff\n\015"] *                            #   normal
(?:  \\ [^\x80-\xff]  [^\\\x80-\xff\n\015"] * )*        #   ( special normal* )*
"                                     #        "
# Quoted string
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
\.
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
# Atom
|                       #  or
"                                     # "
[^\\\x80-\xff\n\015"] *                            #   normal
(?:  \\ [^\x80-\xff]  [^\\\x80-\xff\n\015"] * )*        #   ( special normal* )*
"                                     #        "
# Quoted string
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# additional words
)*
@
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
(?:
\.
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
(?:
[^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]+    # some number of atom characters...
(?![^(\040)<>@,;:".\\\[\]\000-\037\x80-\xff]) # ..not followed by something that could be part of an atom
|
\[                            # [
(?: [^\\\x80-\xff\n\015\[\]] |  \\ [^\x80-\xff]  )*     #    stuff
\]                           #           ]
)
[\040\t]*                    # Nab whitespace.
(?:
\(                              #  (
[^\\\x80-\xff\n\015()] *                             #     normal*
(?:                                 #       (
(?:  \\ [^\x80-\xff]  |
\(                            #  (
[^\\\x80-\xff\n\015()] *                            #     normal*
(?:  \\ [^\x80-\xff]   [^\\\x80-\xff\n\015()] * )*        #     (special normal*)*
\)                           #                       )
)    #         special
[^\\\x80-\xff\n\015()] *                         #         normal*
)*                                  #            )*
\)                             #                )
[\040\t]* )*    # If comment found, allow more spaces.
# optional trailing comments
)*
#       address spec
>                    #                 >
# name and address
)
/x
```
(#70) succeeded -1 times:


---
```
/abc\0def\00pqr\000xyz\0000AB/
```
(#71) succeeded 0 times:

- ☐ `abc def pqr xyz 0AB`
- ☐ `abc456 abc def pqr xyz 0ABCDE`

---
```
/abc\x0def\x00pqr\x000xyz\x0000AB/
```
(#72) succeeded 2 times:

- ☑ `abcef pqr 0xyz 00AB`
- ☑ `abc456 abcef pqr 0xyz 00ABCDE`

---
```
/^[\000-\037]/
```
(#73) succeeded 3 times:

- ☑ ` A`
- ☑ `B`
- ☑ `C`

---
```
/\0*/
```
(#74) succeeded 1 times:

- ☑ `    `

---
```
/A\x0{2,3}Z/
```
(#75) succeeded 4 times:

- ☑ `The A  Z`
- ☑ `An A   Z`
- ☑ `A Z` no match
- ☑ `A    Z` no match

---
```
/^(cow|)\1(bell)/
```
(#76) succeeded 3 times:

- ☑ `cowcowbell`
- ☑ `bell`
- ☑ `cowbell` no match

---
```
/^\s/
```
(#77) succeeded 6 times:

- ☑ ` abc`
- ☑ `abc`
- ☑ `\nabc`
- ☑ `abc`
- ☑ `	abc`
- ☑ `abc` no match

---
```
/^a	b
      c/x
```
(#78) succeeded 1 times:

- ☑ `abc`

---
```
/^(a|)\1*b/
```
(#79) succeeded 4 times:

- ☑ `ab`
- ☑ `aaaab`
- ☑ `b`
- ☑ `acb` no match

---
```
/^(a|)\1+b/
```
(#80) succeeded 4 times:

- ☑ `aab`
- ☑ `aaaab`
- ☑ `b`
- ☑ `ab` no match

---
```
/^(a|)\1?b/
```
(#81) succeeded 4 times:

- ☑ `ab`
- ☑ `aab`
- ☑ `b`
- ☑ `acb` no match

---
```
/^(a|)\1{2}b/
```
(#82) succeeded 5 times:

- ☑ `aaab`
- ☑ `b`
- ☑ `ab` no match
- ☑ `aab` no match
- ☑ `aaaab` no match

---
```
/^(a|)\1{2,3}b/
```
(#83) succeeded 6 times:

- ☑ `aaab`
- ☑ `aaaab`
- ☑ `b`
- ☑ `ab` no match
- ☑ `aab` no match
- ☑ `aaaaab` no match

---
```
/ab{1,3}bc/
```
(#84) succeeded 5 times:

- ☑ `abbbbc`
- ☑ `abbbc`
- ☑ `abbc`
- ☑ `abc` no match
- ☑ `abbbbbc` no match

---
```
/([^.]*)\.([^:]*):[T ]+(.*)/
```
(#85) succeeded 1 times:

- ☑ `track1.title:TBlah blah blah`

---
```
/([^.]*)\.([^:]*):[T ]+(.*)/i
```
(#86) succeeded 1 times:

- ☑ `track1.title:TBlah blah blah`

---
```
/([^.]*)\.([^:]*):[t ]+(.*)/i
```
(#87) succeeded 1 times:

- ☑ `track1.title:TBlah blah blah`

---
```
/^[W-c]+$/
```
(#88) succeeded 2 times:

- ☑ `WXY_^abc`
- ☑ `wxy` no match

---
```
/^[W-c]+$/i
```
(#89) succeeded -1 times:


---
```
/^[\x3f-\x5F]+$/i
```
(#90) succeeded -1 times:


---
```
/^abc$/m
```
(#91) succeeded 4 times:

- ☑ `abc`
- ☑ `qqq\nabc`
- ☑ `abc\nzzz`
- ☑ `qqq\nabc\nzzz`

---
```
/^abc$/
```
(#92) succeeded 4 times:

- ☑ `abc`
- ☑ `qqq\nabc` no match
- ☑ `abc\nzzz` no match
- ☑ `qqq\nabc\nzzz` no match

---
```
/\Aabc\Z/m
```
(#93) succeeded 6 times:

- ☑ `abc`
- ☑ `abc\n`
- ☑ `qqq\nabc` no match
- ☑ `abc\nzzz` no match
- ☑ `qqq\nabc\nzzz` no match
- ☑ `` no match

---
```
/\A(.)*\Z/s
```
(#94) succeeded 1 times:

- ☑ `abc\ndef`

---
```
/\A(.)*\Z/m
```
(#95) succeeded 1 times:

- ☑ `abc\ndef` no match

---
```
/(?:b)|(?::+)/
```
(#96) succeeded 2 times:

- ☑ `b::c`
- ☑ `c::b`

---
```
/[-az]+/
```
(#97) succeeded 2 times:

- ☑ `az-`
- ☑ `b` no match

---
```
/[az-]+/
```
(#98) succeeded 2 times:

- ☑ `za-`
- ☑ `b` no match

---
```
/[a\-z]+/
```
(#99) succeeded 2 times:

- ☑ `a-z`
- ☑ `b` no match

---
```
/[a-z]+/
```
(#100) succeeded 1 times:

- ☑ `abcdxyz`

---
```
/[\d-]+/
```
(#101) succeeded 2 times:

- ☑ `12-34`
- ☑ `aaa` no match

---
```
/\x5c/
```
(#102) succeeded 1 times:

- ☑ `\`

---
```
/\x20Z/
```
(#103) succeeded 2 times:

- ☑ `the Zoo`
- ☑ `Zulu` no match

---
```
/(abc)\1/i
```
(#104) succeeded -1 times:


---
```
/abc$/
```
(#105) succeeded 3 times:

- ☑ `abc`
- ☑ `abc\n`
- ☑ `abc\ndef` no match

---
```
/(abc)\123/
```
(#106) succeeded 1 times:

- ☑ `abcS`

---
```
/(abc)\223/
```
(#107) succeeded 1 times:

- ☑ `abc`

---
```
/(abc)\323/
```
(#108) succeeded 1 times:

- ☑ `abcÓ`

---
```
/(abc)\100/
```
(#109) succeeded 2 times:

- ☑ `abc@`
- ☑ `abc@`

---
```
/(abc)\1000/
```
(#110) succeeded 6 times:

- ☑ `abc@0`
- ☑ `abc@0`
- ☑ `abc@0`
- ☑ `abc@0`
- ☑ `abc@0`
- ☐ `abc@60`
- ☑ ``

---
```
/^(A)(B)(C)(D)(E)(F)(G)(H)(I)\8\9$/
```
(#111) succeeded 1 times:

- ☑ `ABCDEFGHIHI`

---
```
/^[A\8B\9C]+$/
```
(#112) succeeded 2 times:

- ☑ `A8B9C`
- ☑ `A8B9C ` no match

---
```
/(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\12\123/
```
(#113) succeeded 1 times:

- ☑ `abcdefghijkllS`

---
```
/(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\12\123/
```
(#114) succeeded 0 times:

- ☐ `abcdefghijk12S`

---
```
/a{0}bc/
```
(#115) succeeded 1 times:

- ☑ `bc`

---
```
/(a|(bc)){0,0}?xyz/
```
(#116) succeeded 1 times:

- ☑ `xyz`

---
```
/abc[\10]de/
```
(#117) succeeded 0 times:

- ☐ `abcde`

---
```
/abc[\1]de/
```
(#118) succeeded 0 times:

- ☐ `abc1de`

---
```
/(abc)[\1]de/
```
(#119) succeeded 0 times:

- ☐ `abc1de`

---
```
/(?s)a.b/
```
(#120) succeeded 1 times:

- ☑ `a\nb`

---
```
/^([^a])([^\b])([^c]*)([^d]{3,4})/
```
(#121) succeeded 7 times:

- ☑ `baNOTccccd`
- ☑ `baNOTcccd`
- ☑ `baNOTccd`
- ☑ `bacccd`
- ☑ `anything` no match
- ☑ `bbc` no match
- ☑ `baccd` no match

---
```
/[^a]/
```
(#122) succeeded 2 times:

- ☑ `Abc`
- ☑ ``

---
```
/[^a]/i
```
(#123) succeeded 1 times:

- ☑ `Abc`

---
```
/[^a]+/
```
(#124) succeeded 2 times:

- ☑ `AAAaAbc`
- ☑ ``

---
```
/[^a]+/i
```
(#125) succeeded 1 times:

- ☑ `AAAaAbc`

---
```
/[^a]+/
```
(#126) succeeded 2 times:

- ☑ `bbb\nccc`
- ☑ ``

---
```
/[^k]$/
```
(#127) succeeded 3 times:

- ☑ `abc`
- ☑ `abk` no match
- ☑ `` no match

---
```
/[^k]{2,3}$/
```
(#128) succeeded 6 times:

- ☑ `abc`
- ☑ `kbc`
- ☑ `kabc`
- ☑ `abk` no match
- ☑ `akb` no match
- ☑ `akk` no match

---
```
/^\d{8,}\@.+[^k]$/
```
(#129) succeeded 4 times:

- ☑ `12345678@a.b.c.d`
- ☑ `123456789@x.y.z`
- ☑ `12345678@x.y.uk` no match
- ☑ `1234567@a.b.c.d` no match

---
```
/(a)\1{8,}/
```
(#130) succeeded 3 times:

- ☑ `aaaaaaaaa`
- ☑ `aaaaaaaaaa`
- ☑ `aaaaaaa` no match

---
```
/[^a]/
```
(#131) succeeded 2 times:

- ☑ `aaaabcd`
- ☑ `aaAabcd`

---
```
/[^a]/i
```
(#132) succeeded 2 times:

- ☑ `aaaabcd`
- ☑ `aaAabcd`

---
```
/[^az]/
```
(#133) succeeded 2 times:

- ☑ `aaaabcd`
- ☑ `aaAabcd`

---
```
/[^az]/i
```
(#134) succeeded 2 times:

- ☑ `aaaabcd`
- ☑ `aaAabcd`

---
```
/\000\001\002\003\004\005\006\007\010\011\012\013\014\015\016\017\020\021\022\023\024\025\026\027\030\031\032\033\034\035\036\037\040\041\042\043\044\045\046\047\050\051\052\053\054\055\056\057\060\061\062\063\064\065\066\067\070\071\072\073\074\075\076\077\100\101\102\103\104\105\106\107\110\111\112\113\114\115\116\117\120\121\122\123\124\125\126\127\130\131\132\133\134\135\136\137\140\141\142\143\144\145\146\147\150\151\152\153\154\155\156\157\160\161\162\163\164\165\166\167\170\171\172\173\174\175\176\177\200\201\202\203\204\205\206\207\210\211\212\213\214\215\216\217\220\221\222\223\224\225\226\227\230\231\232\233\234\235\236\237\240\241\242\243\244\245\246\247\250\251\252\253\254\255\256\257\260\261\262\263\264\265\266\267\270\271\272\273\274\275\276\277\300\301\302\303\304\305\306\307\310\311\312\313\314\315\316\317\320\321\322\323\324\325\326\327\330\331\332\333\334\335\336\337\340\341\342\343\344\345\346\347\350\351\352\353\354\355\356\357\360\361\362\363\364\365\366\367\370\371\372\373\374\375\376\377/
```
(#135) succeeded -1 times:


---
```
/P[^*]TAIRE[^*]{1,6}?LL/
```
(#136) succeeded 1 times:

- ☑ `xxxxxxxxxxxPSTAIREISLLxxxxxxxxx`

---
```
/P[^*]TAIRE[^*]{1,}?LL/
```
(#137) succeeded 1 times:

- ☑ `xxxxxxxxxxxPSTAIREISLLxxxxxxxxx`

---
```
/(\.\d\d[1-9]?)\d+/
```
(#138) succeeded 4 times:

- ☑ `1.230003938`
- ☑ `1.875000282`
- ☑ `1.235`
- ☑ ``

---
```
/(\.\d\d((?=0)|\d(?=\d)))/
```
(#139) succeeded 4 times:

- ☑ `1.230003938`
- ☑ `1.875000282`
- ☑ `1.235` no match
- ☑ `` no match

---
```
/a(?)b/
```
(#140) succeeded -1 times:


---
```
/\b(foo)\s+(\w+)/i
```
(#141) succeeded 2 times:

- ☑ `Food is on the foo table`
- ☑ ``

---
```
/foo(.*)bar/
```
(#142) succeeded 2 times:

- ☑ `The food is under the bar in the barn.`
- ☑ ``

---
```
/foo(.*?)bar/
```
(#143) succeeded 1 times:

- ☑ `The food is under the bar in the barn.`

---
```
/(.*)(\d*)/
```
(#144) succeeded 2 times:

- ☑ `I have 2 numbers: 53147`
- ☑ ``

---
```
/(.*)(\d+)/
```
(#145) succeeded 2 times:

- ☑ `I have 2 numbers: 53147`
- ☑ ``

---
```
/(.*?)(\d*)/
```
(#146) succeeded 1 times:

- ☑ `I have 2 numbers: 53147`

---
```
/(.*?)(\d+)/
```
(#147) succeeded 1 times:

- ☑ `I have 2 numbers: 53147`

---
```
/(.*)(\d+)$/
```
(#148) succeeded 1 times:

- ☑ `I have 2 numbers: 53147`

---
```
/(.*?)(\d+)$/
```
(#149) succeeded 1 times:

- ☑ `I have 2 numbers: 53147`

---
```
/(.*)\b(\d+)$/
```
(#150) succeeded 1 times:

- ☑ `I have 2 numbers: 53147`

---
```
/(.*\D)(\d+)$/
```
(#151) succeeded 1 times:

- ☑ `I have 2 numbers: 53147`

---
```
/^\D*(?!123)/
```
(#152) succeeded 2 times:

- ☑ `ABC123`
- ☑ ``

---
```
/^(\D*)(?=\d)(?!123)/
```
(#153) succeeded 3 times:

- ☑ `ABC445`
- ☑ `ABC123` no match
- ☑ `` no match

---
```
/^[W-]46]/
```
(#154) succeeded 8 times:

- ☑ `W46]789`
- ☑ `-46]789`
- ☑ `Wall` no match
- ☑ `Zebra` no match
- ☑ `42` no match
- ☑ `[abcd]` no match
- ☑ `]abcd[` no match
- ☑ `` no match

---
```
/^[W-\]46]/
```
(#155) succeeded 11 times:

- ☑ `W46]789`
- ☑ `Wall`
- ☑ `Zebra`
- ☑ `Xylophone`
- ☑ `42`
- ☑ `[abcd]`
- ☑ `]abcd[`
- ☑ `\backslash`
- ☑ `-46]789` no match
- ☑ `well` no match
- ☑ `` no match

---
```
/\d\d\/\d\d\/\d\d\d\d/
```
(#156) succeeded 1 times:

- ☑ `01/01/2000`

---
```
/word (?:[a-zA-Z0-9]+ ){0,10}otherword/
```
(#157) succeeded 2 times:

- ☑ `word cat dog elephant mussel cow horse canary baboon snake shark otherword`
- ☑ `word cat dog elephant mussel cow horse canary baboon snake shark` no match

---
```
/word (?:[a-zA-Z0-9]+ ){0,300}otherword/
```
(#158) succeeded 1 times:

- ☑ `word cat dog elephant mussel cow horse canary baboon snake shark the quick brown fox and the lazy dog and several other words getting close to thirty by now I hope` no match

---
```
/^(a){0,0}/
```
(#159) succeeded 3 times:

- ☑ `bcd`
- ☑ `abc`
- ☑ `aab`

---
```
/^(a){0,1}/
```
(#160) succeeded 3 times:

- ☑ `bcd`
- ☑ `abc`
- ☑ `aab`

---
```
/^(a){0,2}/
```
(#161) succeeded 3 times:

- ☑ `bcd`
- ☑ `abc`
- ☑ `aab`

---
```
/^(a){0,3}/
```
(#162) succeeded 4 times:

- ☑ `bcd`
- ☑ `abc`
- ☑ `aab`
- ☑ `aaa`

---
```
/^(a){0,}/
```
(#163) succeeded 5 times:

- ☑ `bcd`
- ☑ `abc`
- ☑ `aab`
- ☑ `aaa`
- ☑ `aaaaaaaa`

---
```
/^(a){1,1}/
```
(#164) succeeded 3 times:

- ☑ `abc`
- ☑ `aab`
- ☑ `bcd` no match

---
```
/^(a){1,2}/
```
(#165) succeeded 3 times:

- ☑ `abc`
- ☑ `aab`
- ☑ `bcd` no match

---
```
/^(a){1,3}/
```
(#166) succeeded 4 times:

- ☑ `abc`
- ☑ `aab`
- ☑ `aaa`
- ☑ `bcd` no match

---
```
/^(a){1,}/
```
(#167) succeeded 5 times:

- ☑ `abc`
- ☑ `aab`
- ☑ `aaa`
- ☑ `aaaaaaaa`
- ☑ `bcd` no match

---
```
/.*\.gif/
```
(#168) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno`

---
```
/.{0,}\.gif/
```
(#169) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno`

---
```
/.*\.gif/m
```
(#170) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno`

---
```
/.*\.gif/s
```
(#171) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno`

---
```
/.*\.gif/ms
```
(#172) succeeded 2 times:

- ☑ `borfle\nbib.gif\nno`
- ☑ ``

---
```
/.*$/
```
(#173) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno`

---
```
/.*$/m
```
(#174) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno`

---
```
/.*$/s
```
(#175) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno`

---
```
/.*$/ms
```
(#176) succeeded 2 times:

- ☑ `borfle\nbib.gif\nno`
- ☑ ``

---
```
/.*$/
```
(#177) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno\n`

---
```
/.*$/m
```
(#178) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno\n`

---
```
/.*$/s
```
(#179) succeeded 1 times:

- ☑ `borfle\nbib.gif\nno\n`

---
```
/.*$/ms
```
(#180) succeeded 2 times:

- ☑ `borfle\nbib.gif\nno\n`
- ☑ ``

---
```
/(.*X|^B)/
```
(#181) succeeded 3 times:

- ☑ `abcde\n1234Xyz`
- ☑ `BarFoo`
- ☑ `abcde\nBar` no match

---
```
/(.*X|^B)/m
```
(#182) succeeded 3 times:

- ☑ `abcde\n1234Xyz`
- ☑ `BarFoo`
- ☑ `abcde\nBar`

---
```
/(.*X|^B)/s
```
(#183) succeeded 3 times:

- ☑ `abcde\n1234Xyz`
- ☑ `BarFoo`
- ☑ `abcde\nBar` no match

---
```
/(.*X|^B)/ms
```
(#184) succeeded 3 times:

- ☑ `abcde\n1234Xyz`
- ☑ `BarFoo`
- ☑ `abcde\nBar`

---
```
/(?s)(.*X|^B)/
```
(#185) succeeded 3 times:

- ☑ `abcde\n1234Xyz`
- ☑ `BarFoo`
- ☑ `abcde\nBar` no match

---
```
/(?s:.*X|^B)/
```
(#186) succeeded 3 times:

- ☑ `abcde\n1234Xyz`
- ☑ `BarFoo`
- ☑ `abcde\nBar` no match

---
```
/^.*B/
```
(#187) succeeded 2 times:

- ☑ `abc\nB` no match
- ☑ `` no match

---
```
/(?s)^.*B/
```
(#188) succeeded 1 times:

- ☑ `abc\nB`

---
```
/(?m)^.*B/
```
(#189) succeeded 2 times:

- ☑ `abc\nB`
- ☑ ``

---
```
/(?ms)^.*B/
```
(#190) succeeded 1 times:

- ☑ `abc\nB`

---
```
/(?ms)^B/
```
(#191) succeeded 1 times:

- ☑ `abc\nB`

---
```
/(?s)B$/
```
(#192) succeeded 1 times:

- ☑ `B\n`

---
```
/^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]/
```
(#193) succeeded 2 times:

- ☑ `123456654321`
- ☑ ``

---
```
/^\d\d\d\d\d\d\d\d\d\d\d\d/
```
(#194) succeeded 1 times:

- ☑ `123456654321`

---
```
/^[\d][\d][\d][\d][\d][\d][\d][\d][\d][\d][\d][\d]/
```
(#195) succeeded 2 times:

- ☑ `123456654321`
- ☑ ``

---
```
/^[abc]{12}/
```
(#196) succeeded 2 times:

- ☑ `abcabcabcabc`
- ☑ ``

---
```
/^[a-c]{12}/
```
(#197) succeeded 2 times:

- ☑ `abcabcabcabc`
- ☑ ``

---
```
/^(a|b|c){12}/
```
(#198) succeeded 1 times:

- ☑ `abcabcabcabc`

---
```
/^[abcdefghijklmnopqrstuvwxy0123456789]/
```
(#199) succeeded 2 times:

- ☑ `n`
- ☑ `z` no match

---
```
/abcde{0,0}/
```
(#200) succeeded 2 times:

- ☑ `abcd`
- ☑ `abce` no match

---
```
/ab[cd]{0,0}e/
```
(#201) succeeded 3 times:

- ☑ `abe`
- ☑ `abcde` no match
- ☑ `` no match

---
```
/ab(c){0,0}d/
```
(#202) succeeded 2 times:

- ☑ `abd`
- ☑ `abcd` no match

---
```
/a(b*)/
```
(#203) succeeded 5 times:

- ☑ `a`
- ☑ `ab`
- ☑ `abbbb`
- ☑ `bbbbb` no match
- ☑ `` no match

---
```
/ab\d{0}e/
```
(#204) succeeded 3 times:

- ☑ `abe`
- ☑ `ab1e` no match
- ☑ `` no match

---
```
/"([^\\"]+|\\.)*"/
```
(#205) succeeded 1 times:

- ☑ `the "quick" brown fox`
- ☐ `"the \"quick\" brown fox"`

---
```
/.*?/g,aftertext
```
(#206) succeeded -1 times:


---
```
/\b/g,aftertext
```
(#207) succeeded -1 times:


---
```
/\b/g,aftertext
```
(#208) succeeded -1 times:


---
```
//g
```
(#209) succeeded -1 times:


---
```
/<tr([\w\W\s\d][^<>]{0,})><TD([\w\W\s\d][^<>]{0,})>([\d]{0,}\.)(.*)((<BR>([\w\W\s\d][^<>]{0,})|[\s]{0,}))<\/a><\/TD><TD([\w\W\s\d][^<>]{0,})>([\w\W\s\d][^<>]{0,})<\/TD><TD([\w\W\s\d][^<>]{0,})>([\w\W\s\d][^<>]{0,})<\/TD><\/TR>/is
```
(#210) succeeded 1 times:

- ☑ `<TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>`

---
```
/a[^a]b/
```
(#211) succeeded 3 times:

- ☑ `acb`
- ☑ `a\nb`
- ☑ ``

---
```
/a.b/
```
(#212) succeeded 3 times:

- ☑ `acb`
- ☑ `a\nb` no match
- ☑ `` no match

---
```
/a[^a]b/s
```
(#213) succeeded 3 times:

- ☑ `acb`
- ☑ `a\nb`
- ☑ ``

---
```
/a.b/s
```
(#214) succeeded 2 times:

- ☑ `acb`
- ☑ `a\nb`

---
```
/^(b+?|a){1,2}?c/
```
(#215) succeeded 5 times:

- ☑ `bac`
- ☑ `bbac`
- ☑ `bbbac`
- ☑ `bbbbac`
- ☑ `bbbbbac`

---
```
/^(b+|a){1,2}?c/
```
(#216) succeeded 6 times:

- ☑ `bac`
- ☑ `bbac`
- ☑ `bbbac`
- ☑ `bbbbac`
- ☑ `bbbbbac`
- ☑ ``

---
```
/(?!\A)x/m
```
(#217) succeeded 4 times:

- ☑ `abx\n`
- ☑ `a\nx\n`
- ☑ `x\nb\n` no match
- ☑ `` no match

---
```
/(A|B)*?CD/
```
(#218) succeeded 2 times:

- ☑ `CD`
- ☑ ``

---
```
/(A|B)*CD/
```
(#219) succeeded 1 times:

- ☑ `CD`

---
```
/(AB)*?\1/
```
(#220) succeeded 1 times:

- ☑ `ABABAB`

---
```
/(AB)*\1/
```
(#221) succeeded 2 times:

- ☑ `ABABAB`
- ☑ ``

---
```
/(?<!bar)foo/
```
(#222) succeeded 6 times:

- ☑ `foo`
- ☑ `catfood`
- ☑ `arfootle`
- ☑ `rfoosh`
- ☑ `barfoo` no match
- ☑ `towbarfoo` no match

---
```
/\w{3}(?<!bar)foo/
```
(#223) succeeded 4 times:

- ☑ `catfood`
- ☑ `foo` no match
- ☑ `barfoo` no match
- ☑ `towbarfoo` no match

---
```
/(?<=(foo)a)bar/
```
(#224) succeeded 4 times:

- ☑ `fooabar`
- ☑ `bar` no match
- ☑ `foobbar` no match
- ☑ `` no match

---
```
/\Aabc\z/m
```
(#225) succeeded 5 times:

- ☑ `abc`
- ☑ `abc\n` no match
- ☑ `qqq\nabc` no match
- ☑ `abc\nzzz` no match
- ☑ `qqq\nabc\nzzz` no match

---
```
/(?>.*/)foo/
```
(#226) succeeded 2 times:

- ☑ `/this/is/a/very/long/line/in/deed/with/very/many/slashes/in/and/foo`
- ☑ `/this/is/a/very/long/line/in/deed/with/very/many/slashes/in/it/you/see/` no match

---
```
/(?>(\.\d\d[1-9]?))\d+/
```
(#227) succeeded 3 times:

- ☑ `1.230003938`
- ☑ `1.875000282`
- ☑ `1.235` no match

---
```
/^((?>\w+)|(?>\s+))*$/
```
(#228) succeeded 2 times:

- ☑ `now is the time for all good men to come to the aid of the party`
- ☑ `this is not a line with only words and spaces!` no match
- ☐ `` no match

---
```
/(\d+)(\w)/
```
(#229) succeeded 2 times:

- ☑ `12345a`
- ☑ `12345+`

---
```
/((?>\d+))(\w)/
```
(#230) succeeded 2 times:

- ☑ `12345a`
- ☑ `12345+` no match

---
```
/(?>a+)b/
```
(#231) succeeded 1 times:

- ☑ `aaab`

---
```
/((?>a+)b)/
```
(#232) succeeded 1 times:

- ☑ `aaab`

---
```
/(?>(a+))b/
```
(#233) succeeded 1 times:

- ☑ `aaab`

---
```
/(?>b)+/
```
(#234) succeeded 1 times:

- ☑ `aaabbbccc`

---
```
/(?>a+|b+|c+)*c/
```
(#235) succeeded 1 times:

- ☑ `aaabbbbccccd`

---
```
/((?>[^()]+)|\([^()]*\))+/
```
(#236) succeeded 2 times:

- ☑ `((abc(ade)ufh()()x`
- ☑ ``

---
```
/\(((?>[^()]+)|\([^()]+\))+\)/
```
(#237) succeeded 3 times:

- ☑ `(abc)`
- ☑ `(abc(def)xyz)`
- ☑ `((()aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---
```
/a(?-i)b/i
```
(#238) succeeded 5 times:

- ☑ `ab`
- ☑ `Ab`
- ☑ `aB` no match
- ☑ `AB` no match
- ☑ `` no match

---
```
/(a (?x)b c)d e/
```
(#239) succeeded 5 times:

- ☑ `a bcd e`
- ☑ `a b cd e` no match
- ☑ `abcd e` no match
- ☑ `a bcde` no match
- ☑ `` no match

---
```
/(a b(?x)c d (?-x)e f)/
```
(#240) succeeded 2 times:

- ☑ `a bcde f`
- ☑ `abcdef` no match

---
```
/(a(?i)b)c/
```
(#241) succeeded 9 times:

- ☑ `abc`
- ☑ `aBc`
- ☑ `abC` no match
- ☑ `aBC` no match
- ☑ `Abc` no match
- ☑ `ABc` no match
- ☑ `ABC` no match
- ☑ `AbC` no match
- ☑ `` no match

---
```
/a(?i:b)c/
```
(#242) succeeded 6 times:

- ☑ `abc`
- ☑ `aBc`
- ☑ `ABC` no match
- ☑ `abC` no match
- ☑ `aBC` no match
- ☑ `` no match

---
```
/a(?i:b)*c/
```
(#243) succeeded 5 times:

- ☑ `aBc`
- ☑ `aBBc`
- ☑ `aBC` no match
- ☑ `aBBC` no match
- ☑ `` no match

---
```
/a(?=b(?i)c)\w\wd/
```
(#244) succeeded 5 times:

- ☑ `abcd`
- ☑ `abCd`
- ☑ `aBCd` no match
- ☑ `abcD` no match
- ☑ `` no match

---
```
/(?s-i:more.*than).*million/i
```
(#245) succeeded 5 times:

- ☑ `more than million`
- ☑ `more than MILLION`
- ☑ `more \n than Million`
- ☑ `MORE THAN MILLION` no match
- ☑ `more \n than \n million` no match

---
```
/(?:(?s-i)more.*than).*million/i
```
(#246) succeeded 6 times:

- ☑ `more than million`
- ☑ `more than MILLION`
- ☑ `more \n than Million`
- ☑ `MORE THAN MILLION` no match
- ☑ `more \n than \n million` no match
- ☑ `` no match

---
```
/(?>a(?i)b+)+c/
```
(#247) succeeded 7 times:

- ☑ `abc`
- ☑ `aBbc`
- ☑ `aBBc`
- ☑ `Abc` no match
- ☑ `abAb` no match
- ☑ `abbC` no match
- ☑ `` no match

---
```
/(?=a(?i)b)\w\wc/
```
(#248) succeeded 6 times:

- ☑ `abc`
- ☑ `aBc`
- ☑ `Ab` no match
- ☑ `abC` no match
- ☑ `aBC` no match
- ☑ `` no match

---
```
/(?<=a(?i)b)(\w\w)c/
```
(#249) succeeded 5 times:

- ☑ `abxxc`
- ☑ `aBxxc`
- ☑ `Abxxc` no match
- ☑ `ABxxc` no match
- ☑ `abxxC` no match

---
```
/(?:(a)|b)(?(1)A|B)/
```
(#250) succeeded 4 times:

- ☑ `aA`
- ☑ `bB`
- ☑ `aB` no match
- ☑ `bA` no match

---
```
/^(a)?(?(1)a|b)+$/
```
(#251) succeeded 5 times:

- ☑ `aa`
- ☑ `b`
- ☑ `bb`
- ☑ `ab` no match
- ☑ `` no match

---
```
/^(?(?=abc)\w{3}:|\d\d)/
```
(#252) succeeded 4 times:

- ☑ `abc:`
- ☑ `12`
- ☑ `123`
- ☑ `xyz` no match

---
```
/^(?(?!abc)\d\d|\w{3}:)$/
```
(#253) succeeded 5 times:

- ☑ `abc:`
- ☑ `12`
- ☑ `123` no match
- ☑ `xyz` no match
- ☑ `` no match

---
```
/(?(?<=foo)bar|cat)/
```
(#254) succeeded 5 times:

- ☑ `foobar`
- ☑ `cat`
- ☑ `fcat`
- ☑ `focat`
- ☑ `foocat` no match

---
```
/(?(?<!foo)cat|bar)/
```
(#255) succeeded 5 times:

- ☑ `foobar`
- ☑ `cat`
- ☑ `fcat`
- ☑ `focat`
- ☑ `foocat` no match

---
```
/( \( )? [^()]+ (?(1) \) |) /x
```
(#256) succeeded 4 times:

- ☑ `abcd`
- ☑ `(abcd)`
- ☑ `the quick (abcd) fox`
- ☑ `(abcd`

---
```
/( \( )? [^()]+ (?(1) \) ) /x
```
(#257) succeeded 4 times:

- ☑ `abcd`
- ☑ `(abcd)`
- ☑ `the quick (abcd) fox`
- ☑ `(abcd`

---
```
/^(?(2)a|(1)(2))+$/
```
(#258) succeeded 4 times:

- ☑ `12`
- ☑ `12a`
- ☑ `12aa`
- ☑ `1234` no match

---
```
/((?i)blah)\s+\1/
```
(#259) succeeded 7 times:

- ☑ `blah blah`
- ☑ `BLAH BLAH`
- ☑ `Blah Blah`
- ☑ `blaH blaH`
- ☑ `blah BLAH` no match
- ☑ `Blah blah` no match
- ☑ `blaH blah` no match

---
```
/((?i)blah)\s+(?i:\1)/
```
(#260) succeeded -1 times:


---
```
/((?i)blah)\s+(?m)A(?i:\1)/
```
(#261) succeeded -1 times:


---
```
/(?>a*)*/
```
(#262) succeeded 4 times:

- ☑ `a`
- ☑ `aa`
- ☑ `aaaa`
- ☑ ``

---
```
/(abc|)+/
```
(#263) succeeded 4 times:

- ☑ `abc`
- ☑ `abcabc`
- ☑ `abcabcabc`
- ☑ `xyz`

---
```
/([a]*)*/
```
(#264) succeeded 3 times:

- ☑ `a`
- ☑ `aaaaa`
- ☑ ``

---
```
/([ab]*)*/
```
(#265) succeeded 6 times:

- ☑ `a`
- ☑ `b`
- ☑ `ababab`
- ☑ `aaaabcde`
- ☑ `bbbb`
- ☑ ``

---
```
/([^a]*)*/
```
(#266) succeeded 4 times:

- ☑ `b`
- ☑ `bbbb`
- ☑ `aaa`
- ☑ ``

---
```
/([^ab]*)*/
```
(#267) succeeded 3 times:

- ☑ `cccc`
- ☑ `abab`
- ☑ ``

---
```
/([a]*?)*/
```
(#268) succeeded 3 times:

- ☑ `a`
- ☑ `aaaa`
- ☑ ``

---
```
/([ab]*?)*/
```
(#269) succeeded 5 times:

- ☑ `a`
- ☑ `b`
- ☑ `abab`
- ☑ `baba`
- ☑ ``

---
```
/([^a]*?)*/
```
(#270) succeeded 4 times:

- ☑ `b`
- ☑ `bbbb`
- ☑ `aaa`
- ☑ ``

---
```
/([^ab]*?)*/
```
(#271) succeeded 4 times:

- ☑ `c`
- ☑ `cccc`
- ☑ `baba`
- ☑ ``

---
```
/(?>a*)*/
```
(#272) succeeded 3 times:

- ☑ `a`
- ☑ `aaabcde`
- ☑ ``

---
```
/((?>a*))*/
```
(#273) succeeded 3 times:

- ☑ `aaaaa`
- ☑ `aabbaa`
- ☑ ``

---
```
/((?>a*?))*/
```
(#274) succeeded 2 times:

- ☑ `aaaaa`
- ☑ `aabbaa`

---
```
/(?(?=[^a-z]+[a-z])  \d{2}-[a-z]{3}-\d{2}  |  \d{2}-\d{2}-\d{2} ) /x
```
(#275) succeeded 4 times:

- ☑ `12-sep-98`
- ☑ `12-09-98`
- ☑ `sep-12-98` no match
- ☑ `` no match

---
```
/(?<=(foo))bar\1/
```
(#276) succeeded 4 times:

- ☑ `foobarfoo`
- ☑ `foobarfootling`
- ☑ `foobar` no match
- ☑ `barfoo` no match

---
```
/(?i:saturday|sunday)/
```
(#277) succeeded 8 times:

- ☑ `saturday`
- ☑ `sunday`
- ☑ `Saturday`
- ☑ `Sunday`
- ☑ `SATURDAY`
- ☑ `SUNDAY`
- ☑ `SunDay`
- ☑ ``

---
```
/(a(?i)bc|BB)x/
```
(#278) succeeded 8 times:

- ☑ `abcx`
- ☑ `aBCx`
- ☑ `bbx`
- ☑ `BBx`
- ☑ `abcX` no match
- ☑ `aBCX` no match
- ☑ `bbX` no match
- ☑ `BBX` no match

---
```
/^([ab](?i)[cd]|[ef])/
```
(#279) succeeded 8 times:

- ☑ `ac`
- ☑ `aC`
- ☑ `bD`
- ☑ `elephant`
- ☑ `Europe`
- ☑ `frog`
- ☑ `France`
- ☑ `Africa` no match

---
```
/^(ab|a(?i)[b-c](?m-i)d|x(?i)y|z)/
```
(#280) succeeded 8 times:

- ☑ `ab`
- ☑ `aBd`
- ☑ `xy`
- ☑ `xY`
- ☑ `zebra`
- ☑ `Zambesi`
- ☑ `aCD` no match
- ☑ `XY` no match

---
```
/(?<=foo\n)^bar/m
```
(#281) succeeded 3 times:

- ☑ `foo\nbar`
- ☑ `bar` no match
- ☑ `baz\nbar` no match

---
```
/(?<=(?<!foo)bar)baz/
```
(#282) succeeded 5 times:

- ☑ `barbaz`
- ☑ `barbarbaz`
- ☑ `koobarbaz`
- ☑ `baz` no match
- ☑ `foobarbaz` no match

---
```
/^(a\1?){4}$/
```
(#283) succeeded 14 times:

- ☑ `aaaaa`
- ☑ `aaaaaaa`
- ☑ `aaaaaaaaaa`
- ☑ `a` no match
- ☑ `aa` no match
- ☑ `aaa` no match
- ☑ `aaaaaaaa` no match
- ☑ `aaaaaaaaa` no match
- ☑ `aaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaaaaa` no match

---
```
/^(a\1?)(a\1?)(a\2?)(a\3?)$/
```
(#284) succeeded 16 times:

- ☑ `aaaa`
- ☑ `aaaaa`
- ☑ `aaaaaa`
- ☑ `aaaaaaa`
- ☑ `aaaaaaaaaa`
- ☑ `a` no match
- ☑ `aa` no match
- ☑ `aaa` no match
- ☑ `aaaaaaaa` no match
- ☑ `aaaaaaaaa` no match
- ☑ `aaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaaaa` no match
- ☑ `aaaaaaaaaaaaaaaa` no match

---
```
/abc/
```
(#285) succeeded 6 times:

- ☑ `abc`
- ☑ `xabcy`
- ☑ `ababc`
- ☑ `xbc` no match
- ☑ `axc` no match
- ☑ `abx` no match

---
```
/ab*c/
```
(#286) succeeded 1 times:

- ☑ `abc`

---
```
/ab*bc/
```
(#287) succeeded 3 times:

- ☑ `abc`
- ☑ `abbc`
- ☑ `abbbbc`

---
```
/.{1}/
```
(#288) succeeded 1 times:

- ☑ `abbbbc`

---
```
/.{3,4}/
```
(#289) succeeded 1 times:

- ☑ `abbbbc`

---
```
/ab{0,}bc/
```
(#290) succeeded 1 times:

- ☑ `abbbbc`

---
```
/ab+bc/
```
(#291) succeeded 3 times:

- ☑ `abbc`
- ☑ `abc` no match
- ☑ `abq` no match

---
```
/ab{1,}bc/
```
(#292) succeeded 0 times:


---
```
/ab+bc/
```
(#293) succeeded 1 times:

- ☑ `abbbbc`

---
```
/ab{1,}bc/
```
(#294) succeeded 1 times:

- ☑ `abbbbc`

---
```
/ab{1,3}bc/
```
(#295) succeeded 1 times:

- ☑ `abbbbc`

---
```
/ab{3,4}bc/
```
(#296) succeeded 1 times:

- ☑ `abbbbc`

---
```
/ab{4,5}bc/
```
(#297) succeeded 2 times:

- ☑ `abq` no match
- ☑ `abbbbc` no match

---
```
/ab?bc/
```
(#298) succeeded 2 times:

- ☑ `abbc`
- ☑ `abc`

---
```
/ab{0,1}bc/
```
(#299) succeeded 1 times:

- ☑ `abc`

---
```
/ab?bc/
```
(#300) succeeded 0 times:


---
```
/ab?c/
```
(#301) succeeded 1 times:

- ☑ `abc`

---
```
/ab{0,1}c/
```
(#302) succeeded 1 times:

- ☑ `abc`

---
```
/^abc$/
```
(#303) succeeded 3 times:

- ☑ `abc`
- ☑ `abbbbc` no match
- ☑ `abcc` no match

---
```
/^abc/
```
(#304) succeeded 1 times:

- ☑ `abcc`

---
```
/^abc$/
```
(#305) succeeded 0 times:


---
```
/abc$/
```
(#306) succeeded 2 times:

- ☑ `aabc`
- ☑ `aabcd` no match

---
```
/^/
```
(#307) succeeded 1 times:

- ☑ `abc`

---
```
/$/
```
(#308) succeeded 1 times:

- ☑ `abc`

---
```
/a.c/
```
(#309) succeeded 2 times:

- ☑ `abc`
- ☑ `axc`

---
```
/a.*c/
```
(#310) succeeded 1 times:

- ☑ `axyzc`

---
```
/a[bc]d/
```
(#311) succeeded 3 times:

- ☑ `abd`
- ☑ `axyzd` no match
- ☑ `abc` no match

---
```
/a[b-d]e/
```
(#312) succeeded 1 times:

- ☑ `ace`

---
```
/a[b-d]/
```
(#313) succeeded 1 times:

- ☑ `aac`

---
```
/a[-b]/
```
(#314) succeeded 1 times:

- ☑ `a-`

---
```
/a[b-]/
```
(#315) succeeded 1 times:

- ☑ `a-`

---
```
/a]/
```
(#316) succeeded 1 times:

- ☑ `a]`

---
```
/a[]]b/
```
(#317) succeeded 1 times:

- ☑ `a]b`

---
```
/a[^bc]d/
```
(#318) succeeded 3 times:

- ☑ `aed`
- ☑ `abd` no match
- ☑ `abd` no match

---
```
/a[^-b]c/
```
(#319) succeeded 1 times:

- ☑ `adc`

---
```
/a[^]b]c/
```
(#320) succeeded 3 times:

- ☑ `adc`
- ☑ `a-c`
- ☑ `a]c` no match

---
```
/\ba\b/
```
(#321) succeeded 3 times:

- ☑ `a-`
- ☑ `-a`
- ☑ `-a-`

---
```
/\by\b/
```
(#322) succeeded 3 times:

- ☑ `xy` no match
- ☑ `yz` no match
- ☑ `xyz` no match

---
```
/\Ba\B/
```
(#323) succeeded 3 times:

- ☑ `a-` no match
- ☑ `-a` no match
- ☑ `-a-` no match

---
```
/\By\b/
```
(#324) succeeded 1 times:

- ☑ `xy`

---
```
/\by\B/
```
(#325) succeeded 1 times:

- ☑ `yz`

---
```
/\By\B/
```
(#326) succeeded 1 times:

- ☑ `xyz`

---
```
/\w/
```
(#327) succeeded 1 times:

- ☑ `a`

---
```
/\W/
```
(#328) succeeded 2 times:

- ☑ `-`
- ☑ `a` no match

---
```
/a\sb/
```
(#329) succeeded 1 times:

- ☑ `a b`

---
```
/a\Sb/
```
(#330) succeeded 2 times:

- ☑ `a-b`
- ☑ `a b` no match

---
```
/\d/
```
(#331) succeeded 1 times:

- ☑ `1`

---
```
/\D/
```
(#332) succeeded 2 times:

- ☑ `-`
- ☑ `1` no match

---
```
/[\w]/
```
(#333) succeeded 1 times:

- ☑ `a`

---
```
/[\W]/
```
(#334) succeeded 2 times:

- ☑ `-`
- ☑ `a` no match

---
```
/a[\s]b/
```
(#335) succeeded 1 times:

- ☑ `a b`

---
```
/a[\S]b/
```
(#336) succeeded 2 times:

- ☑ `a-b`
- ☑ `a b` no match

---
```
/[\d]/
```
(#337) succeeded 1 times:

- ☑ `1`

---
```
/[\D]/
```
(#338) succeeded 2 times:

- ☑ `-`
- ☑ `1` no match

---
```
/ab|cd/
```
(#339) succeeded 2 times:

- ☑ `abc`
- ☑ `abcd`

---
```
/()ef/
```
(#340) succeeded 1 times:

- ☑ `def`

---
```
/$b/
```
(#341) succeeded 0 times:


---
```
/a\(b/
```
(#342) succeeded 1 times:

- ☑ `a(b`

---
```
/a\(*b/
```
(#343) succeeded 2 times:

- ☑ `ab`
- ☑ `a((b`

---
```
/a\\b/
```
(#344) succeeded 0 times:

- ☐ `a\b`

---
```
/((a))/
```
(#345) succeeded 1 times:

- ☑ `abc`

---
```
/(a)b(c)/
```
(#346) succeeded 1 times:

- ☑ `abc`

---
```
/a+b+c/
```
(#347) succeeded 1 times:

- ☑ `aabbabc`

---
```
/a{1,}b{1,}c/
```
(#348) succeeded 1 times:

- ☑ `aabbabc`

---
```
/a.+?c/
```
(#349) succeeded 1 times:

- ☑ `abcabc`

---
```
/(a+|b)*/
```
(#350) succeeded 1 times:

- ☑ `ab`

---
```
/(a+|b){0,}/
```
(#351) succeeded 1 times:

- ☑ `ab`

---
```
/(a+|b)+/
```
(#352) succeeded 1 times:

- ☑ `ab`

---
```
/(a+|b){1,}/
```
(#353) succeeded 1 times:

- ☑ `ab`

---
```
/(a+|b)?/
```
(#354) succeeded 1 times:

- ☑ `ab`

---
```
/(a+|b){0,1}/
```
(#355) succeeded 1 times:

- ☑ `ab`

---
```
/[^ab]*/
```
(#356) succeeded 1 times:

- ☑ `cde`

---
```
/abc/
```
(#357) succeeded 1 times:

- ☑ `b` no match

---
```
/a*/
```
(#358) succeeded 1 times:

- ☑ `\`

---
```
/([abc])*d/
```
(#359) succeeded 1 times:

- ☑ `abbbcd`

---
```
/([abc])*bcd/
```
(#360) succeeded 1 times:

- ☑ `abcd`

---
```
/a|b|c|d|e/
```
(#361) succeeded 1 times:

- ☑ `e`

---
```
/(a|b|c|d|e)f/
```
(#362) succeeded 1 times:

- ☑ `ef`

---
```
/abcd*efg/
```
(#363) succeeded 1 times:

- ☑ `abcdefg`

---
```
/ab*/
```
(#364) succeeded 2 times:

- ☑ `xabyabbbz`
- ☑ `xayabbbz`

---
```
/(ab|cd)e/
```
(#365) succeeded 1 times:

- ☑ `abcde`

---
```
/[abhgefdc]ij/
```
(#366) succeeded 1 times:

- ☑ `hij`

---
```
/^(ab|cd)e/
```
(#367) succeeded 0 times:


---
```
/(abc|)ef/
```
(#368) succeeded 1 times:

- ☑ `abcdef`

---
```
/(a|b)c*d/
```
(#369) succeeded 1 times:

- ☑ `abcd`

---
```
/(ab|ab*)bc/
```
(#370) succeeded 1 times:

- ☑ `abc`

---
```
/a([bc]*)c*/
```
(#371) succeeded 1 times:

- ☑ `abc`

---
```
/a([bc]*)(c*d)/
```
(#372) succeeded 1 times:

- ☑ `abcd`

---
```
/a([bc]+)(c*d)/
```
(#373) succeeded 1 times:

- ☑ `abcd`

---
```
/a([bc]*)(c+d)/
```
(#374) succeeded 1 times:

- ☑ `abcd`

---
```
/a[bcd]*dcdcde/
```
(#375) succeeded 1 times:

- ☑ `adcdcde`

---
```
/a[bcd]+dcdcde/
```
(#376) succeeded 2 times:

- ☑ `abcde` no match
- ☑ `adcdcde` no match

---
```
/(ab|a)b*c/
```
(#377) succeeded 1 times:

- ☑ `abc`

---
```
/((a)(b)c)(d)/
```
(#378) succeeded 1 times:

- ☑ `abcd`

---
```
/[a-zA-Z_][a-zA-Z0-9_]*/
```
(#379) succeeded 1 times:

- ☑ `alpha`

---
```
/^a(bc+|b[eh])g|.h$/
```
(#380) succeeded 1 times:

- ☑ `abh`

---
```
/(bc+d$|ef*g.|h?i(j|k))/
```
(#381) succeeded 5 times:

- ☑ `effgz`
- ☑ `ij`
- ☑ `reffgz`
- ☑ `effg` no match
- ☑ `bcdd` no match

---
```
/((((((((((a))))))))))/
```
(#382) succeeded 1 times:

- ☑ `a`

---
```
/((((((((((a))))))))))\10/
```
(#383) succeeded 1 times:

- ☑ `aa`

---
```
/(((((((((a)))))))))/
```
(#384) succeeded 1 times:

- ☑ `a`

---
```
/multiple words of text/
```
(#385) succeeded 2 times:

- ☑ `aa` no match
- ☑ `uh-uh` no match

---
```
/multiple words/
```
(#386) succeeded 1 times:

- ☑ `multiple words, yeah`

---
```
/(.*)c(.*)/
```
(#387) succeeded 1 times:

- ☑ `abcde`

---
```
/\((.*), (.*)\)/
```
(#388) succeeded 1 times:

- ☑ `(a, b)`

---
```
/[k]/
```
(#389) succeeded 0 times:


---
```
/abcd/
```
(#390) succeeded 1 times:

- ☑ `abcd`

---
```
/a(bc)d/
```
(#391) succeeded 1 times:

- ☑ `abcd`

---
```
/a[-]?c/
```
(#392) succeeded 1 times:

- ☑ `ac`

---
```
/(abc)\1/
```
(#393) succeeded 1 times:

- ☑ `abcabc`

---
```
/([a-c]*)\1/
```
(#394) succeeded 1 times:

- ☑ `abcabc`

---
```
/(a)|\1/
```
(#395) succeeded 3 times:

- ☑ `a`
- ☑ `ab`
- ☑ `x` no match

---
```
/(([a-c])b*?\2)*/
```
(#396) succeeded 1 times:

- ☑ `ababbbcbc`

---
```
/(([a-c])b*?\2){3}/
```
(#397) succeeded 0 times:

- ☐ `ababbbcbc`

---
```
/((\3|b)\2(a)x)+/
```
(#398) succeeded 1 times:

- ☑ `aaaxabaxbaaxbbax`

---
```
/((\3|b)\2(a)){2,}/
```
(#399) succeeded 0 times:

- ☐ `bbaababbabaaaaabbaaaabba`

---
```
/abc/i
```
(#400) succeeded 7 times:

- ☑ `ABC`
- ☑ `XABCY`
- ☑ `ABABC`
- ☑ `aaxabxbaxbbx` no match
- ☑ `XBC` no match
- ☑ `AXC` no match
- ☑ `ABX` no match

---
```
/ab*c/i
```
(#401) succeeded 1 times:

- ☑ `ABC`

---
```
/ab*bc/i
```
(#402) succeeded 2 times:

- ☑ `ABC`
- ☑ `ABBC`

---
```
/ab*?bc/i
```
(#403) succeeded 1 times:

- ☑ `ABBBBC`

---
```
/ab{0,}?bc/i
```
(#404) succeeded 1 times:

- ☑ `ABBBBC`

---
```
/ab+?bc/i
```
(#405) succeeded 1 times:

- ☑ `ABBC`

---
```
/ab+bc/i
```
(#406) succeeded 2 times:

- ☑ `ABC` no match
- ☑ `ABQ` no match

---
```
/ab{1,}bc/i
```
(#407) succeeded 0 times:


---
```
/ab+bc/i
```
(#408) succeeded 1 times:

- ☑ `ABBBBC`

---
```
/ab{1,}?bc/i
```
(#409) succeeded 1 times:

- ☑ `ABBBBC`

---
```
/ab{1,3}?bc/i
```
(#410) succeeded 1 times:

- ☑ `ABBBBC`

---
```
/ab{3,4}?bc/i
```
(#411) succeeded 1 times:

- ☑ `ABBBBC`

---
```
/ab{4,5}?bc/i
```
(#412) succeeded 2 times:

- ☑ `ABQ` no match
- ☑ `ABBBBC` no match

---
```
/ab??bc/i
```
(#413) succeeded 2 times:

- ☑ `ABBC`
- ☑ `ABC`

---
```
/ab{0,1}?bc/i
```
(#414) succeeded 1 times:

- ☑ `ABC`

---
```
/ab??bc/i
```
(#415) succeeded 0 times:


---
```
/ab??c/i
```
(#416) succeeded 1 times:

- ☑ `ABC`

---
```
/ab{0,1}?c/i
```
(#417) succeeded 1 times:

- ☑ `ABC`

---
```
/^abc$/i
```
(#418) succeeded 3 times:

- ☑ `ABC`
- ☑ `ABBBBC` no match
- ☑ `ABCC` no match

---
```
/^abc/i
```
(#419) succeeded 1 times:

- ☑ `ABCC`

---
```
/^abc$/i
```
(#420) succeeded 0 times:


---
```
/abc$/i
```
(#421) succeeded 1 times:

- ☑ `AABC`

---
```
/^/i
```
(#422) succeeded 1 times:

- ☑ `ABC`

---
```
/$/i
```
(#423) succeeded 1 times:

- ☑ `ABC`

---
```
/a.c/i
```
(#424) succeeded 2 times:

- ☑ `ABC`
- ☑ `AXC`

---
```
/a.*?c/i
```
(#425) succeeded 1 times:

- ☑ `AXYZC`

---
```
/a.*c/i
```
(#426) succeeded 2 times:

- ☑ `AABC`
- ☑ `AXYZD` no match

---
```
/a[bc]d/i
```
(#427) succeeded 1 times:

- ☑ `ABD`

---
```
/a[b-d]e/i
```
(#428) succeeded 3 times:

- ☑ `ACE`
- ☑ `ABC` no match
- ☑ `ABD` no match

---
```
/a[b-d]/i
```
(#429) succeeded 1 times:

- ☑ `AAC`

---
```
/a[-b]/i
```
(#430) succeeded 1 times:

- ☑ `A-`

---
```
/a[b-]/i
```
(#431) succeeded 1 times:

- ☑ `A-`

---
```
/a]/i
```
(#432) succeeded 1 times:

- ☑ `A]`

---
```
/a[]]b/i
```
(#433) succeeded 1 times:

- ☑ `A]B`

---
```
/a[^bc]d/i
```
(#434) succeeded 1 times:

- ☑ `AED`

---
```
/a[^-b]c/i
```
(#435) succeeded 3 times:

- ☑ `ADC`
- ☑ `ABD` no match
- ☑ `A-C` no match

---
```
/a[^]b]c/i
```
(#436) succeeded 1 times:

- ☑ `ADC`

---
```
/ab|cd/i
```
(#437) succeeded 2 times:

- ☑ `ABC`
- ☑ `ABCD`

---
```
/()ef/i
```
(#438) succeeded 1 times:

- ☑ `DEF`

---
```
/$b/i
```
(#439) succeeded 2 times:

- ☑ `A]C` no match
- ☑ `B` no match

---
```
/a\(b/i
```
(#440) succeeded 1 times:

- ☑ `A(B`

---
```
/a\(*b/i
```
(#441) succeeded 2 times:

- ☑ `AB`
- ☑ `A((B`

---
```
/a\\b/i
```
(#442) succeeded 0 times:

- ☐ `A\b`
- ☐ `a\B`

---
```
/((a))/i
```
(#443) succeeded 1 times:

- ☑ `ABC`

---
```
/(a)b(c)/i
```
(#444) succeeded 1 times:

- ☑ `ABC`

---
```
/a+b+c/i
```
(#445) succeeded 1 times:

- ☑ `AABBABC`

---
```
/a{1,}b{1,}c/i
```
(#446) succeeded 1 times:

- ☑ `AABBABC`

---
```
/a.+?c/i
```
(#447) succeeded 1 times:

- ☑ `ABCABC`

---
```
/a.*?c/i
```
(#448) succeeded 1 times:

- ☑ `ABCABC`

---
```
/a.{0,5}?c/i
```
(#449) succeeded 1 times:

- ☑ `ABCABC`

---
```
/(a+|b)*/i
```
(#450) succeeded 1 times:

- ☑ `AB`

---
```
/(a+|b){0,}/i
```
(#451) succeeded 1 times:

- ☑ `AB`

---
```
/(a+|b)+/i
```
(#452) succeeded 1 times:

- ☑ `AB`

---
```
/(a+|b){1,}/i
```
(#453) succeeded 1 times:

- ☑ `AB`

---
```
/(a+|b)?/i
```
(#454) succeeded 1 times:

- ☑ `AB`

---
```
/(a+|b){0,1}/i
```
(#455) succeeded 1 times:

- ☑ `AB`

---
```
/(a+|b){0,1}?/i
```
(#456) succeeded 1 times:

- ☑ `AB`

---
```
/[^ab]*/i
```
(#457) succeeded 1 times:

- ☑ `CDE`

---
```
/([abc])*d/i
```
(#458) succeeded 1 times:

- ☑ `ABBBCD`

---
```
/([abc])*bcd/i
```
(#459) succeeded 1 times:

- ☑ `ABCD`

---
```
/a|b|c|d|e/i
```
(#460) succeeded 1 times:

- ☑ `E`

---
```
/(a|b|c|d|e)f/i
```
(#461) succeeded 1 times:

- ☑ `EF`

---
```
/abcd*efg/i
```
(#462) succeeded 1 times:

- ☑ `ABCDEFG`

---
```
/ab*/i
```
(#463) succeeded 2 times:

- ☑ `XABYABBBZ`
- ☑ `XAYABBBZ`

---
```
/(ab|cd)e/i
```
(#464) succeeded 1 times:

- ☑ `ABCDE`

---
```
/[abhgefdc]ij/i
```
(#465) succeeded 1 times:

- ☑ `HIJ`

---
```
/^(ab|cd)e/i
```
(#466) succeeded 1 times:

- ☑ `ABCDE` no match

---
```
/(abc|)ef/i
```
(#467) succeeded 1 times:

- ☑ `ABCDEF`

---
```
/(a|b)c*d/i
```
(#468) succeeded 1 times:

- ☑ `ABCD`

---
```
/(ab|ab*)bc/i
```
(#469) succeeded 1 times:

- ☑ `ABC`

---
```
/a([bc]*)c*/i
```
(#470) succeeded 1 times:

- ☑ `ABC`

---
```
/a([bc]*)(c*d)/i
```
(#471) succeeded 1 times:

- ☑ `ABCD`

---
```
/a([bc]+)(c*d)/i
```
(#472) succeeded 1 times:

- ☑ `ABCD`

---
```
/a([bc]*)(c+d)/i
```
(#473) succeeded 1 times:

- ☑ `ABCD`

---
```
/a[bcd]*dcdcde/i
```
(#474) succeeded 1 times:

- ☑ `ADCDCDE`

---
```
/a[bcd]+dcdcde/i
```
(#475) succeeded 0 times:


---
```
/(ab|a)b*c/i
```
(#476) succeeded 1 times:

- ☑ `ABC`

---
```
/((a)(b)c)(d)/i
```
(#477) succeeded 1 times:

- ☑ `ABCD`

---
```
/[a-zA-Z_][a-zA-Z0-9_]*/i
```
(#478) succeeded 1 times:

- ☑ `ALPHA`

---
```
/^a(bc+|b[eh])g|.h$/i
```
(#479) succeeded 1 times:

- ☑ `ABH`

---
```
/(bc+d$|ef*g.|h?i(j|k))/i
```
(#480) succeeded 6 times:

- ☑ `EFFGZ`
- ☑ `IJ`
- ☑ `REFFGZ`
- ☑ `ADCDCDE` no match
- ☑ `EFFG` no match
- ☑ `BCDD` no match

---
```
/((((((((((a))))))))))/i
```
(#481) succeeded 1 times:

- ☑ `A`

---
```
/((((((((((a))))))))))\10/i
```
(#482) succeeded 1 times:

- ☑ `AA`

---
```
/(((((((((a)))))))))/i
```
(#483) succeeded 1 times:

- ☑ `A`

---
```
/(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))/i
```
(#484) succeeded 1 times:

- ☑ `A`

---
```
/(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))/i
```
(#485) succeeded 1 times:

- ☑ `C`

---
```
/multiple words of text/i
```
(#486) succeeded 2 times:

- ☑ `AA` no match
- ☑ `UH-UH` no match

---
```
/multiple words/i
```
(#487) succeeded 1 times:

- ☑ `MULTIPLE WORDS, YEAH`

---
```
/(.*)c(.*)/i
```
(#488) succeeded 1 times:

- ☑ `ABCDE`

---
```
/\((.*), (.*)\)/i
```
(#489) succeeded 1 times:

- ☑ `(A, B)`

---
```
/[k]/i
```
(#490) succeeded 0 times:


---
```
/abcd/i
```
(#491) succeeded 1 times:

- ☑ `ABCD`

---
```
/a(bc)d/i
```
(#492) succeeded 1 times:

- ☑ `ABCD`

---
```
/a[-]?c/i
```
(#493) succeeded 1 times:

- ☑ `AC`

---
```
/(abc)\1/i
```
(#494) succeeded 1 times:

- ☑ `ABCABC`

---
```
/([a-c]*)\1/i
```
(#495) succeeded 1 times:

- ☑ `ABCABC`

---
```
/a(?!b)./
```
(#496) succeeded 1 times:

- ☑ `abad`

---
```
/a(?=d)./
```
(#497) succeeded 1 times:

- ☑ `abad`

---
```
/a(?=c|d)./
```
(#498) succeeded 1 times:

- ☑ `abad`

---
```
/a(?:b|c|d)(.)/
```
(#499) succeeded 1 times:

- ☑ `ace`

---
```
/a(?:b|c|d)*(.)/
```
(#500) succeeded 1 times:

- ☑ `ace`

---
```
/a(?:b|c|d)+?(.)/
```
(#501) succeeded 2 times:

- ☑ `ace`
- ☑ `acdbcdbe`

---
```
/a(?:b|c|d)+(.)/
```
(#502) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){2}(.)/
```
(#503) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){4,5}(.)/
```
(#504) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){4,5}?(.)/
```
(#505) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/((foo)|(bar))*/
```
(#506) succeeded 1 times:

- ☑ `foobar`

---
```
/a(?:b|c|d){6,7}(.)/
```
(#507) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){6,7}?(.)/
```
(#508) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){5,6}(.)/
```
(#509) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){5,6}?(.)/
```
(#510) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){5,7}(.)/
```
(#511) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|c|d){5,7}?(.)/
```
(#512) succeeded 1 times:

- ☑ `acdbcdbe`

---
```
/a(?:b|(c|e){1,2}?|d)+?(.)/
```
(#513) succeeded 1 times:

- ☑ `ace`

---
```
/^(.+)?B/
```
(#514) succeeded 1 times:

- ☑ `AB`

---
```
/^([^a-z])|(\^)$/
```
(#515) succeeded 1 times:

- ☑ `.`

---
```
/^[<>]&/
```
(#516) succeeded 1 times:

- ☑ `<&OUT`

---
```
/^(a\1?){4}$/
```
(#517) succeeded 4 times:

- ☑ `aaaaaaaaaa`
- ☑ `AB` no match
- ☑ `aaaaaaaaa` no match
- ☑ `aaaaaaaaaaa` no match

---
```
/^(a(?(1)\1)){4}$/
```
(#518) succeeded 3 times:

- ☑ `aaaaaaaaaa`
- ☑ `aaaaaaaaa` no match
- ☑ `aaaaaaaaaaa` no match

---
```
/(?:(f)(o)(o)|(b)(a)(r))*/
```
(#519) succeeded 1 times:

- ☑ `foobar`

---
```
/(?<=a)b/
```
(#520) succeeded 3 times:

- ☑ `ab`
- ☑ `cb` no match
- ☑ `b` no match

---
```
/(?<!c)b/
```
(#521) succeeded 3 times:

- ☑ `ab`
- ☑ `b`
- ☑ `b`

---
```
/(?:..)*a/
```
(#522) succeeded 1 times:

- ☑ `aba`

---
```
/(?:..)*?a/
```
(#523) succeeded 1 times:

- ☑ `aba`

---
```
/^(?:b|a(?=(.)))*\1/
```
(#524) succeeded 0 times:

- ☐ `abc`

---
```
/^(){3,5}/
```
(#525) succeeded 1 times:

- ☑ `abc`

---
```
/^(a+)*ax/
```
(#526) succeeded 1 times:

- ☑ `aax`

---
```
/^((a|b)+)*ax/
```
(#527) succeeded 0 times:

- ☐ `aax`

---
```
/^((a|bc)+)*ax/
```
(#528) succeeded 0 times:

- ☐ `aax`

---
```
/(a|x)*ab/
```
(#529) succeeded 1 times:

- ☑ `cab`

---
```
/(a)*ab/
```
(#530) succeeded 1 times:

- ☑ `cab`

---
```
/(?:(?i)a)b/
```
(#531) succeeded 1 times:

- ☑ `ab`

---
```
/((?i)a)b/
```
(#532) succeeded 1 times:

- ☑ `ab`

---
```
/(?:(?i)a)b/
```
(#533) succeeded 1 times:

- ☑ `Ab`

---
```
/((?i)a)b/
```
(#534) succeeded 1 times:

- ☑ `Ab`

---
```
/(?:(?i)a)b/
```
(#535) succeeded 2 times:

- ☑ `cb` no match
- ☑ `aB` no match

---
```
/((?i)a)b/
```
(#536) succeeded 0 times:


---
```
/(?i:a)b/
```
(#537) succeeded 1 times:

- ☑ `ab`

---
```
/((?i:a))b/
```
(#538) succeeded 1 times:

- ☑ `ab`

---
```
/(?i:a)b/
```
(#539) succeeded 1 times:

- ☑ `Ab`

---
```
/((?i:a))b/
```
(#540) succeeded 1 times:

- ☑ `Ab`

---
```
/(?i:a)b/
```
(#541) succeeded 2 times:

- ☑ `aB` no match
- ☑ `aB` no match

---
```
/((?i:a))b/
```
(#542) succeeded 0 times:


---
```
/(?:(?-i)a)b/i
```
(#543) succeeded 1 times:

- ☑ `ab`

---
```
/((?-i)a)b/i
```
(#544) succeeded 1 times:

- ☑ `ab`

---
```
/(?:(?-i)a)b/i
```
(#545) succeeded 1 times:

- ☑ `aB`

---
```
/((?-i)a)b/i
```
(#546) succeeded 1 times:

- ☑ `aB`

---
```
/(?:(?-i)a)b/i
```
(#547) succeeded 3 times:

- ☑ `aB`
- ☑ `Ab` no match
- ☑ `AB` no match

---
```
/(?-i:a)b/i
```
(#548) succeeded 1 times:

- ☑ `ab`

---
```
/((?-i:a))b/i
```
(#549) succeeded 1 times:

- ☑ `ab`

---
```
/(?-i:a)b/i
```
(#550) succeeded 1 times:

- ☑ `aB`

---
```
/((?-i:a))b/i
```
(#551) succeeded 1 times:

- ☑ `aB`

---
```
/(?-i:a)b/i
```
(#552) succeeded 2 times:

- ☑ `AB` no match
- ☑ `Ab` no match

---
```
/((?-i:a))b/i
```
(#553) succeeded 0 times:


---
```
/(?-i:a)b/i
```
(#554) succeeded 1 times:

- ☑ `aB`

---
```
/((?-i:a))b/i
```
(#555) succeeded 1 times:

- ☑ `aB`

---
```
/(?-i:a)b/i
```
(#556) succeeded 2 times:

- ☑ `Ab` no match
- ☑ `AB` no match

---
```
/((?-i:a))b/i
```
(#557) succeeded 0 times:


---
```
/((?-i:a.))b/i
```
(#558) succeeded 2 times:

- ☑ `AB` no match
- ☑ `a\nB` no match

---
```
/((?s-i:a.))b/i
```
(#559) succeeded 1 times:

- ☑ `a\nB`

---
```
/(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))/
```
(#560) succeeded 1 times:

- ☑ `cabbbb`

---
```
/(?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))/
```
(#561) succeeded 1 times:

- ☑ `caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb`

---
```
/(ab)\d\1/i
```
(#562) succeeded -1 times:


---
```
/foo\w*\d{4}baz/
```
(#563) succeeded 1 times:

- ☑ `foobar1234baz`

---
```
/x(~~)*(?:(?:F)?)?/
```
(#564) succeeded 1 times:

- ☑ `x~~`

---
```
/^a(?#xxx){3}c/
```
(#565) succeeded 1 times:

- ☑ `aaac`

---
```
/^a (?#xxx) (?#yyy) {3}c/x
```
(#566) succeeded 1 times:

- ☑ `aaac`

---
```
/(?<![cd])b/
```
(#567) succeeded 2 times:

- ☑ `B\nB` no match
- ☑ `dbcb` no match

---
```
/(?<![cd])[ab]/
```
(#568) succeeded 1 times:

- ☑ `dbaacb`

---
```
/(?<!(c|d))b/
```
(#569) succeeded 0 times:


---
```
/(?<!(c|d))[ab]/
```
(#570) succeeded 1 times:

- ☑ `dbaacb`

---
```
/(?<!cd)[ab]/
```
(#571) succeeded 1 times:

- ☑ `cdaccb`

---
```
/^(?:a?b?)*$/
```
(#572) succeeded 6 times:

- ☐ `\`
- ☑ `a`
- ☑ `ab`
- ☑ `aaa`
- ☑ `dbcb` no match
- ☑ `a--` no match
- ☑ `aa--` no match

---
```
/((?s)^a(.))((?m)^b$)/
```
(#573) succeeded 1 times:

- ☑ `a\nb\nc\n`

---
```
/((?m)^b$)/
```
(#574) succeeded 1 times:

- ☑ `a\nb\nc\n`

---
```
/(?m)^b/
```
(#575) succeeded 1 times:

- ☑ `a\nb\n`

---
```
/(?m)^(b)/
```
(#576) succeeded 1 times:

- ☑ `a\nb\n`

---
```
/((?m)^b)/
```
(#577) succeeded 1 times:

- ☑ `a\nb\n`

---
```
/\n((?m)^b)/
```
(#578) succeeded 1 times:

- ☑ `a\nb\n`

---
```
/((?s).)c(?!.)/
```
(#579) succeeded 2 times:

- ☑ `a\nb\nc\n`
- ☑ `a\nb\nc\n`

---
```
/((?s)b.)c(?!.)/
```
(#580) succeeded 2 times:

- ☑ `a\nb\nc\n`
- ☑ `a\nb\nc\n`

---
```
/^b/
```
(#581) succeeded 0 times:


---
```
/()^b/
```
(#582) succeeded 2 times:

- ☑ `a\nb\nc\n` no match
- ☑ `a\nb\nc\n` no match

---
```
/((?m)^b)/
```
(#583) succeeded 1 times:

- ☑ `a\nb\nc\n`

---
```
/(x)?(?(1)a|b)/
```
(#584) succeeded 2 times:

- ☑ `a` no match
- ☑ `a` no match

---
```
/(x)?(?(1)b|a)/
```
(#585) succeeded 1 times:

- ☑ `a`

---
```
/()?(?(1)b|a)/
```
(#586) succeeded 0 times:

- ☐ `a`

---
```
/()(?(1)b|a)/
```
(#587) succeeded 0 times:


---
```
/()?(?(1)a|b)/
```
(#588) succeeded 1 times:

- ☑ `a`

---
```
/^(\()?blah(?(1)(\)))$/
```
(#589) succeeded 5 times:

- ☑ `(blah)`
- ☑ `blah`
- ☑ `a` no match
- ☑ `blah)` no match
- ☑ `(blah` no match

---
```
/^(\(+)?blah(?(1)(\)))$/
```
(#590) succeeded 4 times:

- ☑ `(blah)`
- ☑ `blah`
- ☑ `blah)` no match
- ☑ `(blah` no match

---
```
/(?(?!a)a|b)/
```
(#591) succeeded 0 times:


---
```
/(?(?!a)b|a)/
```
(#592) succeeded 1 times:

- ☑ `a`

---
```
/(?(?=a)b|a)/
```
(#593) succeeded 2 times:

- ☑ `a` no match
- ☑ `a` no match

---
```
/(?(?=a)a|b)/
```
(#594) succeeded 1 times:

- ☑ `a`

---
```
/(?=(a+?))(\1ab)/
```
(#595) succeeded 1 times:

- ☑ `aaab`

---
```
/^(?=(a+?))\1ab/
```
(#596) succeeded 0 times:


---
```
/(\w+:)+/
```
(#597) succeeded 1 times:

- ☑ `one:`

---
```
/$(?<=^(a))/
```
(#598) succeeded 1 times:

- ☑ `a`

---
```
/(?=(a+?))(\1ab)/
```
(#599) succeeded 1 times:

- ☑ `aaab`

---
```
/^(?=(a+?))\1ab/
```
(#600) succeeded 2 times:

- ☑ `aaab` no match
- ☑ `aaab` no match

---
```
/([\w:]+::)?(\w+)$/
```
(#601) succeeded 2 times:

- ☑ `abcd`
- ☑ `xy:z:::abcd`

---
```
/^[^bcd]*(c+)/
```
(#602) succeeded 1 times:

- ☑ `aexycd`

---
```
/(a*)b+/
```
(#603) succeeded 1 times:

- ☑ `caab`

---
```
/([\w:]+::)?(\w+)$/
```
(#604) succeeded 4 times:

- ☑ `abcd`
- ☑ `xy:z:::abcd`
- ☑ `abcd:` no match
- ☑ `abcd:` no match

---
```
/^[^bcd]*(c+)/
```
(#605) succeeded 1 times:

- ☑ `aexycd`

---
```
/(>a+)ab/
```
(#606) succeeded 0 times:


---
```
/(?>a+)b/
```
(#607) succeeded 1 times:

- ☑ `aaab`

---
```
/([[:]+)/
```
(#608) succeeded 1 times:

- ☑ `a:[b]:`

---
```
/([[=]+)/
```
(#609) succeeded 1 times:

- ☑ `a=[b]=`

---
```
/([[.]+)/
```
(#610) succeeded 1 times:

- ☑ `a.[b].`

---
```
/((?>a+)b)/
```
(#611) succeeded 1 times:

- ☑ `aaab`

---
```
/(?>(a+))b/
```
(#612) succeeded 1 times:

- ☑ `aaab`

---
```
/((?>[^()]+)|\([^()]*\))+/
```
(#613) succeeded 1 times:

- ☑ `((abc(ade)ufh()()x`

---
```
/a\Z/
```
(#614) succeeded 2 times:

- ☑ `aaab` no match
- ☑ `a\nb\n` no match

---
```
/b\Z/
```
(#615) succeeded 1 times:

- ☑ `a\nb\n`

---
```
/b\z/
```
(#616) succeeded 0 times:


---
```
/b\Z/
```
(#617) succeeded 1 times:

- ☑ `a\nb`

---
```
/b\z/
```
(#618) succeeded 2 times:

- ☑ `a\nb`
- ☑ ``

---
```
/^(?>(?(1)\.|())[^\W_](?>[a-z0-9-]*[^\W_])?)+$/
```
(#619) succeeded 22 times:

- ☑ `a`
- ☑ `abc`
- ☑ `a-b`
- ☑ `0-9`
- ☑ `a.b`
- ☑ `5.6.7`
- ☑ `the.quick.brown.fox`
- ☑ `a100.b200.300c`
- ☑ `12-ab.1245`
- ☑ `\` no match
- ☑ `.a` no match
- ☑ `-a` no match
- ☑ `a-` no match
- ☑ `a.` no match
- ☑ `a_b` no match
- ☑ `a.-` no match
- ☑ `a..` no match
- ☑ `ab..bc` no match
- ☑ `the.quick.brown.fox-` no match
- ☑ `the.quick.brown.fox.` no match
- ☑ `the.quick.brown.fox_` no match
- ☑ `the.quick.brown.fox+` no match

---
```
/(?>.*)(?<=(abcd|wxyz))/
```
(#620) succeeded 3 times:

- ☑ `alphabetabcd`
- ☑ `endingwxyz`
- ☑ `a rather long string that doesn't end with one of them` no match

---
```
/word (?>(?:(?!otherword)[a-zA-Z0-9]+ ){0,30})otherword/
```
(#621) succeeded 3 times:

- ☑ `word cat dog elephant mussel cow horse canary baboon snake shark otherword`
- ☑ `word cat dog elephant mussel cow horse canary baboon snake shark` no match
- ☑ `` no match

---
```
/word (?>[a-zA-Z0-9]+ ){0,30}otherword/
```
(#622) succeeded 1 times:

- ☑ `word cat dog elephant mussel cow horse canary baboon snake shark the quick brown fox and the lazy dog and several other words getting close to thirty by now I hope` no match

---
```
/(?<=\d{3}(?!999))foo/
```
(#623) succeeded 4 times:

- ☑ `999foo`
- ☑ `123999foo`
- ☑ `123abcfoo` no match
- ☑ `` no match

---
```
/(?<=(?!...999)\d{3})foo/
```
(#624) succeeded 3 times:

- ☑ `999foo`
- ☑ `123999foo`
- ☑ `123abcfoo` no match

---
```
/(?<=\d{3}(?!999)...)foo/
```
(#625) succeeded 3 times:

- ☑ `123abcfoo`
- ☑ `123456foo`
- ☐ `123999foo` no match
- ☑ `` no match

---
```
/(?<=\d{3}...)(?<!999)foo/
```
(#626) succeeded 3 times:

- ☑ `123abcfoo`
- ☑ `123456foo`
- ☑ `123999foo` no match

---
```
/<a[\s]+href[\s]*=[\s]*          # find <a href=
 ([\"\'])?                       # find single or double quote
 (?(1) (.*?)\1 | ([^\s]+))       # if quote found, match up to next matching
                                 # quote, otherwise match up to next space
/isx
```
(#627) succeeded 3 times:

- ☑ `<a href=abcd xyz`
- ☑ `<a href="abcd xyz pqr" cats`
- ☑ `<a href='abcd xyz pqr' cats`

---
```
/<a\s+href\s*=\s*                # find <a href=
 (["'])?                         # find single or double quote
 (?(1) (.*?)\1 | (\S+))          # if quote found, match up to next matching
                                 # quote, otherwise match up to next space
/isx
```
(#628) succeeded 3 times:

- ☑ `<a href=abcd xyz`
- ☑ `<a href="abcd xyz pqr" cats`
- ☑ `<a href       =       'abcd xyz pqr' cats`

---
```
/<a\s+href(?>\s*)=(?>\s*)        # find <a href=
 (["'])?                         # find single or double quote
 (?(1) (.*?)\1 | (\S+))          # if quote found, match up to next matching
                                 # quote, otherwise match up to next space
/isx
```
(#629) succeeded 3 times:

- ☑ `<a href=abcd xyz`
- ☑ `<a href="abcd xyz pqr" cats`
- ☑ `<a href       =       'abcd xyz pqr' cats`

---
```
/((Z)+|A)*/
```
(#630) succeeded 1 times:

- ☑ `ZABCDEFG`

---
```
/(Z()|A)*/
```
(#631) succeeded 1 times:

- ☑ `ZABCDEFG`

---
```
/(Z(())|A)*/
```
(#632) succeeded 1 times:

- ☑ `ZABCDEFG`

---
```
/((?>Z)+|A)*/
```
(#633) succeeded 1 times:

- ☑ `ZABCDEFG`

---
```
/((?>)+|A)*/
```
(#634) succeeded 1 times:

- ☑ `ZABCDEFG`

---
```
/a*/g
```
(#635) succeeded -1 times:


---
```
/[[:space:]]+/
```
(#636) succeeded 2 times:

- ☑ `> 	\n<`
- ☑ ``

---
```
/[[:blank:]]+/
```
(#637) succeeded 2 times:

- ☑ `> 	\n<`
- ☑ ``

---
```
/[\s]+/
```
(#638) succeeded 2 times:

- ☑ `> 	\n<`
- ☑ ``

---
```
/\s+/
```
(#639) succeeded 2 times:

- ☑ `> 	\n<`
- ☑ ``

---
```
/ab/x
```
(#640) succeeded 1 times:

- ☑ `ab`

---
```
/(?!\A)x/m
```
(#641) succeeded 1 times:

- ☑ `a\nxb\n`

---
```
/(?!^)x/m
```
(#642) succeeded 1 times:

- ☑ `a\nxb\n` no match

---
```
/abc\Qabc\Eabc/
```
(#643) succeeded 2 times:

- ☑ `abcabcabc`
- ☑ ``

---
```
/abc\Q(*+|\Eabc/
```
(#644) succeeded 1 times:

- ☑ `abc(*+|abc`

---
```
/   abc\Q abc\Eabc/x
```
(#645) succeeded 3 times:

- ☑ `abc abcabc`
- ☑ `abcabcabc` no match
- ☑ `` no match

---
```
/abc#comment
    \Q#not comment
    literal\E/x
```
(#646) succeeded 1 times:

- ☑ `abc#not comment\n    literal`

---
```
/abc#comment
    \Q#not comment
    literal/x
```
(#647) succeeded 1 times:

- ☑ `abc#not comment\n    literal`

---
```
/abc#comment
    \Q#not comment
    literal\E #more comment
    /x
```
(#648) succeeded 1 times:

- ☑ `abc#not comment\n    literal`

---
```
/abc#comment
    \Q#not comment
    literal\E #more comment/x
```
(#649) succeeded 1 times:

- ☑ `abc#not comment\n    literal`

---
```
/\Qabc\$xyz\E/
```
(#650) succeeded 0 times:

- ☐ `abc\$xyz`

---
```
/\Qabc\E\$\Qxyz\E/
```
(#651) succeeded 1 times:

- ☑ `abc$xyz`

---
```
/\Gabc/
```
(#652) succeeded 2 times:

- ☑ `abc`
- ☑ `xyzabc` no match

---
```
/\Gabc./g
```
(#653) succeeded -1 times:


---
```
/abc./g
```
(#654) succeeded -1 times:


---
```
/a(?x: b c )d/
```
(#655) succeeded 2 times:

- ☑ `XabcdY`
- ☑ `Xa b c d Y` no match

---
```
/((?x)x y z | a b c)/
```
(#656) succeeded 2 times:

- ☑ `XabcY`
- ☑ `AxyzB`

---
```
/(?i)AB(?-i)C/
```
(#657) succeeded 2 times:

- ☑ `XabCY`
- ☑ `XabcY` no match

---
```
/((?i)AB(?-i)C|D)E/
```
(#658) succeeded 6 times:

- ☑ `abCE`
- ☑ `DE`
- ☑ `abcE` no match
- ☑ `abCe` no match
- ☑ `dE` no match
- ☑ `De` no match

---
```
/(.*)\d+\1/
```
(#659) succeeded 2 times:

- ☑ `abc123abc`
- ☑ `abc123bc`

---
```
/(.*)\d+\1/s
```
(#660) succeeded 3 times:

- ☑ `abc123abc`
- ☑ `abc123bc`
- ☑ ``

---
```
/((.*))\d+\1/
```
(#661) succeeded 2 times:

- ☑ `abc123abc`
- ☑ `abc123bc`

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
(#662) succeeded 14 times:

- ☑ `a123::a123`
- ☑ `a123:b342::abcd`
- ☑ `a123:b342::324e:abcd`
- ☑ `a123:ddde:b342::324e:abcd`
- ☑ `a123:ddde:b342::324e:dcba:abcd`
- ☑ `a123:ddde:9999:b342::324e:dcba:abcd`
- ☑ `1:2:3:4:5:6:7:8` no match
- ☑ `a123:bce:ddde:9999:b342::324e:dcba:abcd` no match
- ☑ `a123::9999:b342::324e:dcba:abcd` no match
- ☑ `abcde:2:3:4:5:6:7:8` no match
- ☑ `::1` no match
- ☑ `abcd:fee0:123::` no match
- ☑ `:1` no match
- ☑ `1:` no match

---
```
/[z\Qa-d]\E]/
```
(#663) succeeded 6 times:

- ☑ `z`
- ☑ `a`
- ☑ `-`
- ☑ `d`
- ☑ `]`
- ☑ `b` no match

---
```
/(a+)*b/
```
(#664) succeeded -1 times:


---
```
/(?i)reg(?:ul(?:[a�]|ae)r|ex)/
```
(#665) succeeded -1 times:


---
```
/����[�-��-�]+/
```
(#666) succeeded -1 times:


---
```
/(?<=Z)X./
```
(#667) succeeded 0 times:


---
```
/ab cd (?x) de fg/
```
(#668) succeeded 1 times:

- ☑ `ab cd defg`

---
```
/ab cd(?x) de fg/
```
(#669) succeeded 2 times:

- ☑ `ab cddefg`
- ☑ `abcddefg` no match

---
```
/(?<![^f]oo)(bar)/
```
(#670) succeeded 2 times:

- ☑ `foobarX`
- ☑ `boobarX` no match

---
```
/(?<![^f])X/
```
(#671) succeeded 2 times:

- ☑ `offX`
- ☑ `onyX` no match

---
```
/(?<=[^f])X/
```
(#672) succeeded 2 times:

- ☑ `onyX`
- ☑ `offX` no match

---
```
/^/gm
```
(#673) succeeded -1 times:


---
```
/(?<=C\n)^/gm
```
(#674) succeeded -1 times:


---
```
/(?:(?(1)a|b)(X))+/
```
(#675) succeeded 1 times:

- ☑ `bXaX`

---
```
/(?:(?(1)\1a|b)(X|Y))+/
```
(#676) succeeded 1 times:

- ☐ `bXXaYYaY`
- ☑ `bXYaXXaX`

---
```
/()()()()()()()()()(?:(?(10)\10a|b)(X|Y))+/
```
(#677) succeeded 1 times:

- ☑ `bXXaYYaY`

---
```
/[[,abc,]+]/
```
(#678) succeeded 3 times:

- ☑ `abc]`
- ☑ `a,b]`
- ☑ `[a,b,c]`

---
```
/(?-x: )/x
```
(#679) succeeded 2 times:

- ☑ `A B`
- ☑ ``

---
```
/(?x)(?-x: \s*#\s*)/
```
(#680) succeeded 2 times:

- ☑ `A # B`
- ☑ `#` no match

---
```
/(?x-is)(?:(?-ixs) \s*#\s*) include/
```
(#681) succeeded 3 times:

- ☑ `A #include`
- ☑ `A#include` no match
- ☑ `A #Include` no match

---
```
/a*b*\w/
```
(#682) succeeded 3 times:

- ☑ `aaabbbb`
- ☑ `aaaa`
- ☑ `a`

---
```
/a*b?\w/
```
(#683) succeeded 3 times:

- ☑ `aaabbbb`
- ☑ `aaaa`
- ☑ `a`

---
```
/a*b{0,4}\w/
```
(#684) succeeded 3 times:

- ☑ `aaabbbb`
- ☑ `aaaa`
- ☑ `a`

---
```
/a*b{0,}\w/
```
(#685) succeeded 4 times:

- ☑ `aaabbbb`
- ☑ `aaaa`
- ☑ `a`
- ☑ ``

---
```
/a*\d*\w/
```
(#686) succeeded 3 times:

- ☑ `0a`
- ☑ `a`
- ☑ ``

---
```
/a*b *\w/x
```
(#687) succeeded 1 times:

- ☑ `a`

---
```
/a*b#comment
  *\w/x
```
(#688) succeeded 1 times:

- ☑ `a`

---
```
/a* b *\w/x
```
(#689) succeeded 1 times:

- ☑ `a`

---
```
/^\w+=.*(\\\n.*)*/
```
(#690) succeeded 1 times:

- ☑ `abc=xyz\\npqr`

---
```
/(?=(\w+))\1:/
```
(#691) succeeded 1 times:

- ☑ `abcd:`

---
```
/^(?=(\w+))\1:/
```
(#692) succeeded 1 times:

- ☑ `abcd:`

---
```
/^\Eabc/
```
(#693) succeeded -1 times:


---
```
/^[\Eabc]/
```
(#694) succeeded 3 times:

- ☑ `a`
- ☑ `E` no match
- ☑ `` no match

---
```
/^[a-\Ec]/
```
(#695) succeeded 1 times:

- ☐ `b`
- ☐ `-` no match
- ☑ `E` no match

---
```
/^[a\E\E-\Ec]/
```
(#696) succeeded 1 times:

- ☐ `b`
- ☐ `-` no match
- ☑ `E` no match

---
```
/^[\E\Qa\E-\Qz\E]+/
```
(#697) succeeded 1 times:

- ☐ `b`
- ☐ `-` no match
- ☑ `` no match

---
```
/^[a\Q]bc\E]/
```
(#698) succeeded 4 times:

- ☑ `a`
- ☑ `]`
- ☑ `c`
- ☑ ``

---
```
/^[a-\Q\E]/
```
(#699) succeeded 2 times:

- ☑ `a`
- ☑ `-`

---
```
/^(a()*)*/
```
(#700) succeeded 1 times:

- ☑ `aaaa`

---
```
/^(?:a(?:(?:))*)*/
```
(#701) succeeded 1 times:

- ☑ `aaaa`

---
```
/^(a()+)+/
```
(#702) succeeded 1 times:

- ☑ `aaaa`

---
```
/^(?:a(?:(?:))+)+/
```
(#703) succeeded 1 times:

- ☑ `aaaa`

---
```
/(a){0,3}(?(1)b|(c|))*D/
```
(#704) succeeded 3 times:

- ☑ `abbD`
- ☑ `ccccD`
- ☑ `D`

---
```
/(a|)*\d/
```
(#705) succeeded 2 times:

- ☑ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4`
- ☑ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---
```
/(?>a|)*\d/
```
(#706) succeeded 2 times:

- ☑ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4`
- ☑ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---
```
/(?:a|)*\d/
```
(#707) succeeded 2 times:

- ☑ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4`
- ☑ `aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---
```
/\Z/g
```
(#708) succeeded -1 times:


---
```
/^(?s)(?>.*)(?<!\n)/
```
(#709) succeeded 2 times:

- ☑ `abc`
- ☑ `abc\n` no match

---
```
/^(?![^\n]*\n\z)/
```
(#710) succeeded 2 times:

- ☑ `abc`
- ☑ `abc\n` no match
- ☐ `` no match

---
```
/\z(?<!\n)/
```
(#711) succeeded 2 times:

- ☑ `abc`
- ☑ `abc\n` no match

---
```
/(.*(.)?)*/
```
(#712) succeeded 1 times:

- ☑ `abcd`

---
```
/( (A | (?(1)0|) )*   )/x
```
(#713) succeeded 1 times:

- ☑ `abcd`

---
```
/( ( (?(1)0|) )*   )/x
```
(#714) succeeded 1 times:

- ☑ `abcd`

---
```
/(  (?(1)0|)*   )/x
```
(#715) succeeded 1 times:

- ☑ `abcd`

---
```
/[[:abcd:xyz]]/
```
(#716) succeeded 3 times:

- ☑ `a]`
- ☑ `:]`
- ☑ ``

---
```
/[abc[:x\]pqr]/
```
(#717) succeeded 5 times:

- ☑ `a`
- ☑ `[`
- ☑ `:`
- ☑ `]`
- ☑ `p`

---
```
/.*[op][xyz]/
```
(#718) succeeded 1 times:

- ☑ `fooabcfoo` no match

---
```
/(?(?=.*b)b|^)/
```
(#719) succeeded 2 times:

- ☑ `adc`
- ☑ `abc`

---
```
/(?(?=^.*b)b|^)/
```
(#720) succeeded 2 times:

- ☑ `adc`
- ☑ `abc` no match

---
```
/(?(?=.*b)b|^)*/
```
(#721) succeeded 2 times:

- ☑ `adc`
- ☑ `abc`

---
```
/(?(?=.*b)b|^)+/
```
(#722) succeeded 2 times:

- ☑ `adc`
- ☑ `abc`

---
```
/(?(?=b).*b|^d)/
```
(#723) succeeded 1 times:

- ☑ `abc`

---
```
/(?(?=.*b).*b|^d)/
```
(#724) succeeded 1 times:

- ☑ `abc`

---
```
/^%((?(?=[a])[^%])|b)*%$/
```
(#725) succeeded 1 times:

- ☑ `%ab%`

---
```
/(?i)a(?-i)b|c/
```
(#726) succeeded 4 times:

- ☑ `XabX`
- ☑ `XAbX`
- ☑ `CcC`
- ☑ `XABX` no match

---
```
/[\x00-\xff\s]+/
```
(#727) succeeded 1 times:

- ☑ `\n`

---
```
/(abc)\1/i
```
(#728) succeeded 1 times:

- ☑ `abc` no match

---
```
/(abc)\1/
```
(#729) succeeded 1 times:

- ☑ `abc` no match

---
```
/[^a]*/i
```
(#730) succeeded 2 times:

- ☑ `12abc`
- ☑ `12ABC`

---
```
/[^a]*+/i
```
(#731) succeeded 2 times:

- ☑ `12abc`
- ☑ `12ABC`

---
```
/[^a]*?X/i
```
(#732) succeeded 3 times:

- ☑ `12abc` no match
- ☑ `12ABC` no match
- ☑ `` no match

---
```
/[^a]+?X/i
```
(#733) succeeded 2 times:

- ☑ `12abc` no match
- ☑ `12ABC` no match

---
```
/[^a]?X/i
```
(#734) succeeded 3 times:

- ☑ `12aXbcX`
- ☑ `12AXBCX`
- ☑ `BCX`

---
```
/[^a]??X/i
```
(#735) succeeded 4 times:

- ☑ `12aXbcX`
- ☑ `12AXBCX`
- ☑ `BCX`
- ☑ ``

---
```
/[^a]?+X/i
```
(#736) succeeded 3 times:

- ☑ `12aXbcX`
- ☑ `12AXBCX`
- ☑ `BCX`

---
```
/[^a]{2,3}/i
```
(#737) succeeded 2 times:

- ☑ `abcdef`
- ☑ `ABCDEF`

---
```
/[^a]{2,3}?/i
```
(#738) succeeded 2 times:

- ☑ `abcdef`
- ☑ `ABCDEF`

---
```
/[^a]{2,3}+/i
```
(#739) succeeded 2 times:

- ☑ `abcdef`
- ☑ `ABCDEF`

---
```
/((a|)+)+Z/
```
(#740) succeeded 1 times:

- ☑ `Z`

---
```
/(a)b|(a)c/
```
(#741) succeeded 1 times:

- ☑ `ac`

---
```
/(?>(a))b|(a)c/
```
(#742) succeeded 1 times:

- ☑ `ac`

---
```
/(?=(a))ab|(a)c/
```
(#743) succeeded 1 times:

- ☑ `ac`

---
```
/((?>(a))b|(a)c)/
```
(#744) succeeded 1 times:

- ☑ `ac`

---
```
/((?>(a))b|(a)c)++/
```
(#745) succeeded 1 times:

- ☑ `ac`

---
```
/(?:(?>(a))b|(a)c)++/
```
(#746) succeeded 1 times:

- ☑ `ac`

---
```
/(?=(?>(a))b|(a)c)(..)/
```
(#747) succeeded 1 times:

- ☑ `ac`

---
```
/(?>(?>(a))b|(a)c)/
```
(#748) succeeded 1 times:

- ☑ `ac`

---
```
/(?:(?>([ab])))+a=/aftertext
```
(#749) succeeded -1 times:


---
```
/(?>([ab]))+a=/aftertext
```
(#750) succeeded -1 times:


---
```
/((?>(a+)b)+(aabab))/
```
(#751) succeeded 1 times:

- ☑ `aaaabaaabaabab`

---
```
/(?>a+|ab)+?c/
```
(#752) succeeded 1 times:

- ☑ `aabc` no match

---
```
/(?>a+|ab)+c/
```
(#753) succeeded 1 times:

- ☑ `aabc` no match

---
```
/(?:a+|ab)+c/
```
(#754) succeeded 1 times:

- ☑ `aabc`

---
```
/(?(?=(a))a)/
```
(#755) succeeded 1 times:

- ☑ `a`

---
```
/(?(?=(a))a)(b)/
```
(#756) succeeded 1 times:

- ☑ `ab`

---
```
/^(?:a|ab)++c/
```
(#757) succeeded 1 times:

- ☑ `aaaabc` no match

---
```
/^(?>a|ab)++c/
```
(#758) succeeded 1 times:

- ☑ `aaaabc` no match

---
```
/^(?:a|ab)+c/
```
(#759) succeeded 1 times:

- ☑ `aaaabc`

---
```
/(?=abc){3}abc/aftertext
```
(#760) succeeded -1 times:


---
```
/(?=abc)+abc/aftertext
```
(#761) succeeded -1 times:


---
```
/(?=abc)++abc/aftertext
```
(#762) succeeded -1 times:


---
```
/(?=abc){0}xyz/
```
(#763) succeeded 1 times:

- ☑ `xyz`

---
```
/(?=abc){1}xyz/
```
(#764) succeeded 2 times:

- ☑ `xyz` no match
- ☑ `` no match

---
```
/(?=(a))?./
```
(#765) succeeded 3 times:

- ☑ `ab`
- ☑ `bc`
- ☑ ``

---
```
/(?=(a))??./
```
(#766) succeeded 2 times:

- ☑ `ab`
- ☑ `bc`

---
```
/^(?=(?1))?[az]([abc])d/
```
(#767) succeeded 2 times:

- ☑ `abd`
- ☑ `zcdxx`

---
```
/^(?!a){0}\w+/
```
(#768) succeeded 1 times:

- ☑ `aaaaa`

---
```
/(?<=(abc))?xyz/
```
(#769) succeeded 2 times:

- ☑ `abcxyz`
- ☑ `pqrxyz`

---
```
/^[\g<a>]+/
```
(#770) succeeded 3 times:

- ☑ `ggg<<<aaa>>>`
- ☑ `\ga` no match
- ☑ `` no match

---
```
/^[\ga]+/
```
(#771) succeeded 2 times:

- ☑ `gggagagaxyz`
- ☑ ``

---
```
/^[:a[:digit:]]+/
```
(#772) succeeded 1 times:

- ☑ `aaaa444:::Z`

---
```
/^[:a[:digit:]:b]+/
```
(#773) succeeded 1 times:

- ☑ `aaaa444:::bbbZ`

---
```
/[:a]xxx[b:]/
```
(#774) succeeded 2 times:

- ☑ `:xxx:`
- ☑ ``

---
```
/(?<=a{2})b/i
```
(#775) succeeded 2 times:

- ☑ `xaabc`
- ☑ `xabc` no match

---
```
/(?<!a{2})b/i
```
(#776) succeeded 2 times:

- ☑ `xabc`
- ☑ `xaabc` no match

---
```
/(?<=a\h)c/
```
(#777) succeeded 2 times:

- ☑ `xa c`
- ☑ ``

---
```
/(?<=[^a]{2})b/
```
(#778) succeeded 3 times:

- ☑ `axxbc`
- ☑ `aAAbc`
- ☑ `xaabc` no match

---
```
/(?<=[^a]{2})b/i
```
(#779) succeeded 3 times:

- ☑ `axxbc`
- ☑ `aAAbc` no match
- ☑ `xaabc` no match

---
```
/(?<=a\H)c/
```
(#780) succeeded 1 times:

- ☑ `abc`

---
```
/(?<=a\V)c/
```
(#781) succeeded 2 times:

- ☑ `abc`
- ☑ ``

---
```
/(?<=a\v)c/
```
(#782) succeeded 1 times:

- ☑ `a\nc`

---
```
/(?(?=c)c|d)++Y/
```
(#783) succeeded 1 times:

- ☑ `XcccddYX`

---
```
/(?(?=c)c|d)*+Y/
```
(#784) succeeded 1 times:

- ☑ `XcccddYX`

---
```
/^(a{2,3}){2,}+a/
```
(#785) succeeded 3 times:

- ☑ `aaaaaaa`
- ☑ `aaaaaa` no match
- ☑ `aaaaaaaaa` no match

---
```
/^(a{2,3})++a/
```
(#786) succeeded 1 times:

- ☑ `aaaaaa` no match

---
```
/^(a{2,3})*+a/
```
(#787) succeeded 1 times:

- ☑ `aaaaaa` no match

---
```
/\H\h\V\v/
```
(#788) succeeded 4 times:

- ☑ `X X\n`
- ☑ `X	X`
- ☑ `  X\n` no match
- ☑ `` no match

---
```
/\H*\h+\V?\v{3,4}/
```
(#789) succeeded 5 times:

- ☑ `	  X\n\n`
- ☑ `	  \n\n`
- ☑ `	  \n`
- ☑ `	  \n` no match
- ☑ `` no match

---
```
/\H{3,4}/
```
(#790) succeeded 3 times:

- ☑ `XY  ABCDE`
- ☑ `XY  PQR ST`
- ☑ ``

---
```
/.\h{3,4}./
```
(#791) succeeded 1 times:

- ☑ `XY  AB    PQRS`

---
```
/\h*X\h?\H+Y\H?Z/
```
(#792) succeeded 4 times:

- ☑ `>XNNNYZ`
- ☑ `>  X NYQZ`
- ☑ `>XYZ` no match
- ☑ `>  X NY Z` no match

---
```
/\v*X\v?Y\v+Z\V*\x0a\V+\x0b\V{2,3}\x0c/
```
(#793) succeeded 2 times:

- ☑ `>XY\nZ\nANN`
- ☑ `>\nX\nY\nZZZ\nAAANNN`

---
```
/(foo)\Kbar/
```
(#794) succeeded -1 times:


---
```
/(foo)(\Kbar|baz)/
```
(#795) succeeded -1 times:


---
```
/(foo\Kbar)baz/
```
(#796) succeeded -1 times:


---
```
/abc\K|def\K/g,aftertext
```
(#797) succeeded -1 times:


---
```
/ab\Kc|de\Kf/g,aftertext
```
(#798) succeeded -1 times:


---
```
/(?=C)/g,aftertext
```
(#799) succeeded -1 times:


---
```
/^abc\K/aftertext
```
(#800) succeeded -1 times:


---
```
/^(a(b))\1\g1\g{1}\g-1\g{-1}\g{-2}Z/
```
(#801) succeeded 1 times:

- ☑ `ababababbbabZXXXX`

---
```
/(?<A>tom|bon)-\g{A}/
```
(#802) succeeded 3 times:

- ☑ `tom-tom`
- ☑ `bon-bon`
- ☑ ``

---
```
/(^(a|b\g{-1}))/
```
(#803) succeeded 1 times:

- ☑ `bacxxx` no match

---
```
/(?|(abc)|(xyz))\1/
```
(#804) succeeded 5 times:

- ☑ `abcabc`
- ☑ `xyzxyz`
- ☑ `abcxyz` no match
- ☑ `xyzabc` no match
- ☑ `` no match

---
```
/(?|(abc)|(xyz))(?1)/
```
(#805) succeeded 4 times:

- ☑ `abcabc`
- ☑ `xyzabc`
- ☑ `xyzxyz` no match
- ☑ `` no match

---
```
/^X(?5)(a)(?|(b)|(q))(c)(d)(Y)/
```
(#806) succeeded 1 times:

- ☑ `XYabcdY`

---
```
/^X(?7)(a)(?|(b|(r)(s))|(q))(c)(d)(Y)/
```
(#807) succeeded 1 times:

- ☑ `XYabcdY`

---
```
/^X(?7)(a)(?|(b|(?|(r)|(t))(s))|(q))(c)(d)(Y)/
```
(#808) succeeded 1 times:

- ☑ `XYabcdY`

---
```
/(?'abc'\w+):\k<abc>{2}/
```
(#809) succeeded 4 times:

- ☑ `a:aaxyz`
- ☑ `ab:ababxyz`
- ☑ `a:axyz` no match
- ☑ `ab:abxyz` no match

---
```
/(?'abc'\w+):\g{abc}{2}/
```
(#810) succeeded 4 times:

- ☑ `a:aaxyz`
- ☑ `ab:ababxyz`
- ☑ `a:axyz` no match
- ☑ `ab:abxyz` no match

---
```
/^(?<ab>a)? (?(<ab>)b|c) (?('ab')d|e)/x
```
(#811) succeeded 2 times:

- ☑ `abd`
- ☑ `ce`

---
```
/^(a.)\g-1Z/
```
(#812) succeeded 1 times:

- ☑ `aXaXZ`

---
```
/^(a.)\g{-1}Z/
```
(#813) succeeded 1 times:

- ☑ `aXaXZ`

---
```
/^(?(DEFINE) (?<A> a) (?<B> b) )  (?&A) (?&B) /x
```
(#814) succeeded 1 times:

- ☑ `abcd`

---
```
/(?<NAME>(?&NAME_PAT))\s+(?<ADDR>(?&ADDRESS_PAT))
  (?(DEFINE)
  (?<NAME_PAT>[a-z]+)
  (?<ADDRESS_PAT>\d+)
  )/x
```
(#815) succeeded 1 times:

- ☑ `metcalfe 33`

---
```
/(?(DEFINE)(?<byte>2[0-4]\d|25[0-5]|1\d\d|[1-9]?\d))\b(?&byte)(\.(?&byte)){3}/
```
(#816) succeeded 5 times:

- ☑ `1.2.3.4`
- ☑ `131.111.10.206`
- ☑ `10.0.0.0`
- ☑ `10.6` no match
- ☑ `455.3.4.5` no match

---
```
/\b(?&byte)(\.(?&byte)){3}(?(DEFINE)(?<byte>2[0-4]\d|25[0-5]|1\d\d|[1-9]?\d))/
```
(#817) succeeded 5 times:

- ☑ `1.2.3.4`
- ☑ `131.111.10.206`
- ☑ `10.0.0.0`
- ☑ `10.6` no match
- ☑ `455.3.4.5` no match

---
```
/^(\w++|\s++)*$/
```
(#818) succeeded 2 times:

- ☑ `now is the time for all good men to come to the aid of the party`
- ☑ `this is not a line with only words and spaces!` no match

---
```
/(\d++)(\w)/
```
(#819) succeeded 2 times:

- ☑ `12345a`
- ☑ `12345+` no match

---
```
/a++b/
```
(#820) succeeded 1 times:

- ☑ `aaab`

---
```
/(a++b)/
```
(#821) succeeded 1 times:

- ☑ `aaab`

---
```
/(a++)b/
```
(#822) succeeded 1 times:

- ☑ `aaab`

---
```
/([^()]++|\([^()]*\))+/
```
(#823) succeeded 1 times:

- ☑ `((abc(ade)ufh()()x`

---
```
/\(([^()]++|\([^()]+\))+\)/
```
(#824) succeeded 3 times:

- ☑ `(abc)`
- ☑ `(abc(def)xyz)`
- ☑ `((()aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa` no match

---
```
/^([^()]|\((?1)*\))*$/
```
(#825) succeeded 4 times:

- ☑ `abc`
- ☑ `a(b)c`
- ☑ `a(b(c))d`
- ☑ `a(b(c)d` no match

---
```
/^>abc>([^()]|\((?1)*\))*<xyz<$/
```
(#826) succeeded 3 times:

- ☑ `>abc>123<xyz<`
- ☑ `>abc>1(2)3<xyz<`
- ☑ `>abc>(1(2)3)<xyz<`

---
```
/^(?:((.)(?1)\2|)|((.)(?3)\4|.))$/i
```
(#827) succeeded 2 times:

- ☑ `1221`
- ☐ `Satanoscillatemymetallicsonatas`
- ☐ `AmanaplanacanalPanama`
- ☐ `AblewasIereIsawElba`
- ☑ `Thequickbrownfox` no match

---
```
/^(\d+|\((?1)([+*-])(?1)\)|-(?1))$/
```
(#828) succeeded 4 times:

- ☑ `12`
- ☑ `(((2+2)*-3)-7)`
- ☑ `-12`
- ☑ `((2+2)*-3)-7)` no match

---
```
/^(x(y|(?1){2})z)/
```
(#829) succeeded 4 times:

- ☑ `xyz`
- ☑ `xxyzxyzz`
- ☑ `xxyzz` no match
- ☑ `xxyzxyzxyzz` no match

---
```
/((< (?: (?(R) \d++  | [^<>]*+) | (?2)) * >))/x
```
(#830) succeeded -1 times:


---
```
/^a+(*FAIL)/
```
(#831) succeeded 2 times:

- ☑ `aaaaaa` no match
- ☑ `` no match

---
```
/a+b?c+(*FAIL)/
```
(#832) succeeded 1 times:

- ☑ `aaabccc` no match

---
```
/a+b?(*PRUNE)c+(*FAIL)/
```
(#833) succeeded -1 times:


---
```
/a+b?(*COMMIT)c+(*FAIL)/
```
(#834) succeeded -1 times:


---
```
/a+b?(*SKIP)c+(*FAIL)/
```
(#835) succeeded -1 times:


---
```
/^(?:aaa(*THEN)\w{6}|bbb(*THEN)\w{5}|ccc(*THEN)\w{4}|\w{3})/
```
(#836) succeeded -1 times:


---
```
/^(aaa(*THEN)\w{6}|bbb(*THEN)\w{5}|ccc(*THEN)\w{4}|\w{3})/
```
(#837) succeeded -1 times:


---
```
/a+b?(*THEN)c+(*FAIL)/
```
(#838) succeeded -1 times:


---
```
/(A (A|B(*ACCEPT)|C) D)(E)/x
```
(#839) succeeded -1 times:


---
```
/^\W*+(?:((.)\W*+(?1)\W*+\2|)|((.)\W*+(?3)\W*+\4|\W*+.\W*+))\W*+$/i
```
(#840) succeeded 2 times:

- ☑ `1221`
- ☐ `Satan, oscillate my metallic sonatas!`
- ☐ `A man, a plan, a canal: Panama!`
- ☐ `Able was I ere I saw Elba.`
- ☑ `The quick brown fox` no match

---
```
/^((.)(?1)\2|.)$/
```
(#841) succeeded 8 times:

- ☑ `a`
- ☑ `aba`
- ☑ `aabaa`
- ☑ `abcdcba`
- ☑ `pqaabaaqp`
- ☑ `ablewasiereisawelba`
- ☑ `rhubarb` no match
- ☑ `the quick brown fox` no match

---
```
/(a)(?<=b(?1))/
```
(#842) succeeded 3 times:

- ☑ `baz`
- ☑ `caz` no match
- ☑ `` no match

---
```
/(?<=b(?1))(a)/
```
(#843) succeeded 3 times:

- ☑ `zbaaz`
- ☑ `aaa` no match
- ☑ `` no match

---
```
/(?<X>a)(?<=b(?&X))/
```
(#844) succeeded 1 times:

- ☑ `baz`

---
```
/^(?|(abc)|(def))\1/
```
(#845) succeeded 5 times:

- ☑ `abcabc`
- ☑ `defdef`
- ☑ `abcdef` no match
- ☑ `defabc` no match
- ☑ `` no match

---
```
/^(?|(abc)|(def))(?1)/
```
(#846) succeeded 4 times:

- ☑ `abcabc`
- ☑ `defabc`
- ☑ `defdef` no match
- ☑ `abcdef` no match

---
```
/(?:a(?<quote> (?<apostrophe>')|(?<realquote>")) |b(?<quote> (?<apostrophe>')|(?<realquote>")) ) (?('quote')[a-z]+|[0-9]+)/x,dupnames
```
(#847) succeeded 3 times:

- ☑ `a"aaaaa`
- ☑ `b"aaaaa`
- ☑ `b"11111` no match

---
```
/(?:(?1)|B)(A(*F)|C)/
```
(#848) succeeded 3 times:

- ☑ `ABCD`
- ☑ `CCD`
- ☑ `CAD` no match

---
```
/^(?:(?1)|B)(A(*F)|C)/
```
(#849) succeeded 5 times:

- ☑ `CCD`
- ☑ `BCD`
- ☑ `ABCD` no match
- ☑ `CAD` no match
- ☑ `BAD` no match

---
```
/(?:(?1)|B)(A(*ACCEPT)XX|C)D/
```
(#850) succeeded -1 times:


---
```
/(?(DEFINE)(A))B(?1)C/
```
(#851) succeeded 1 times:

- ☑ `BAC`

---
```
/(?(DEFINE)((A)\2))B(?1)C/
```
(#852) succeeded 1 times:

- ☑ `BAAC`

---
```
/(?<pn> \( ( [^()]++ | (?&pn) )* \) )/x
```
(#853) succeeded 1 times:

- ☑ `(ab(cd)ef)`

---
```
/^(?=a(*SKIP)b|ac)/
```
(#854) succeeded -1 times:


---
```
/^(?=a(*PRUNE)b)/
```
(#855) succeeded -1 times:


---
```
/^(?=a(*ACCEPT)b)/
```
(#856) succeeded -1 times:


---
```
/(?>a\Kb)/
```
(#857) succeeded -1 times:


---
```
/((?>a\Kb))/
```
(#858) succeeded -1 times:


---
```
/(a\Kb)/
```
(#859) succeeded -1 times:


---
```
/^a\Kcz|ac/
```
(#860) succeeded -1 times:


---
```
/(?>a\Kbz|ab)/
```
(#861) succeeded -1 times:


---
```
/^(?&t)(?(DEFINE)(?<t>a\Kb))$/
```
(#862) succeeded -1 times:


---
```
/^([^()]|\((?1)*\))*$/
```
(#863) succeeded 2 times:

- ☑ `a(b)c`
- ☑ `a(b(c)d)e`

---
```
/(?P<L1>(?P<L2>0)(?P>L1)|(?P>L2))/
```
(#864) succeeded 3 times:

- ☑ `0`
- ☑ `00`
- ☑ `0000`

---
```
/(?P<L1>(?P<L2>0)|(?P>L2)(?P>L1))/
```
(#865) succeeded 3 times:

- ☑ `0`
- ☑ `00`
- ☑ `0000`

---
```
/A(*COMMIT)(B|D)/
```
(#866) succeeded -1 times:


---
```
/^(A(*PRUNE:A)B|C(*PRUNE:B)D)/mark
```
(#867) succeeded -1 times:


---
```
/(*MARK:A)(*SKIP:B)(C|X)/mark
```
(#868) succeeded -1 times:


---
```
/^(A(*THEN:A)B|C(*THEN:B)D)/mark
```
(#869) succeeded -1 times:


---
```
/^(?:A(*THEN:A)B|C(*THEN:B)D)/mark
```
(#870) succeeded -1 times:


---
```
/^(?>A(*THEN:A)B|C(*THEN:B)D)/mark
```
(#871) succeeded -1 times:


---
```
/A(*MARK:A)A+(*SKIP:A)(B|Z) | AC/x,mark
```
(#872) succeeded -1 times:


---
```
/A(*MARK:A)A+(*MARK:B)(*SKIP:A)(B|Z) | AC/x,mark
```
(#873) succeeded -1 times:


---
```
/A(*:A)A+(*SKIP:A)(B|Z) | AC/x,mark
```
(#874) succeeded -1 times:


---
```
/(*:A)A+(*SKIP:A)(B|Z)/mark
```
(#875) succeeded -1 times:


---
```
/A(*MARK:A)A+(*SKIP:B)(B|Z) | AC/x,mark
```
(#876) succeeded -1 times:


---
```
/A(*MARK:A)A+(*SKIP:B)(B|Z) | AC(*:B)/x,mark
```
(#877) succeeded -1 times:


---
```
/(*COMMIT)(A|P)(B|P)(C|P)/
```
(#878) succeeded -1 times:


---
```
/(\w+)(?>b(*COMMIT))\w{2}/
```
(#879) succeeded -1 times:


---
```
/(\w+)b(*COMMIT)\w{2}/
```
(#880) succeeded -1 times:


---
```
/(?&t)(?#()(?(DEFINE)(?<t>a))/
```
(#881) succeeded 1 times:

- ☑ `bac`

---
```
/(?>(*COMMIT)(?>yes|no)(*THEN)(*F))?/
```
(#882) succeeded -1 times:


---
```
/(?>(*COMMIT)(yes|no)(*THEN)(*F))?/
```
(#883) succeeded -1 times:


---
```
/b?(*SKIP)c/
```
(#884) succeeded -1 times:


---
```
/(*SKIP)bc/
```
(#885) succeeded -1 times:


---
```
/(*SKIP)b/
```
(#886) succeeded -1 times:


---
```
/(?P<abn>(?P=abn)xxx|)+/
```
(#887) succeeded -1 times:


---
```
/(?i:([^b]))(?1)/
```
(#888) succeeded 6 times:

- ☑ `aa`
- ☑ `aA`
- ☑ `ab` no match
- ☑ `aB` no match
- ☑ `Ba` no match
- ☑ `ba` no match

---
```
/^(?&t)*+(?(DEFINE)(?<t>a))\w$/
```
(#889) succeeded 2 times:

- ☑ `aaaaaaX`
- ☑ `aaaaaa` no match

---
```
/^(?&t)*(?(DEFINE)(?<t>a))\w$/
```
(#890) succeeded 2 times:

- ☑ `aaaaaaX`
- ☑ `aaaaaa`

---
```
/^(a)*+(\w)/
```
(#891) succeeded 3 times:

- ☑ `aaaaX`
- ☑ `YZ`
- ☑ `aaaa` no match

---
```
/^(?:a)*+(\w)/
```
(#892) succeeded 3 times:

- ☑ `aaaaX`
- ☑ `YZ`
- ☑ `aaaa` no match

---
```
/^(a)++(\w)/
```
(#893) succeeded 3 times:

- ☑ `aaaaX`
- ☑ `aaaa` no match
- ☑ `YZ` no match

---
```
/^(?:a)++(\w)/
```
(#894) succeeded 3 times:

- ☑ `aaaaX`
- ☑ `aaaa` no match
- ☑ `YZ` no match

---
```
/^(a)?+(\w)/
```
(#895) succeeded 2 times:

- ☑ `aaaaX`
- ☑ `YZ`

---
```
/^(?:a)?+(\w)/
```
(#896) succeeded 2 times:

- ☑ `aaaaX`
- ☑ `YZ`

---
```
/^(a){2,}+(\w)/
```
(#897) succeeded 3 times:

- ☑ `aaaaX`
- ☑ `aaa` no match
- ☑ `YZ` no match

---
```
/^(?:a){2,}+(\w)/
```
(#898) succeeded 3 times:

- ☑ `aaaaX`
- ☑ `aaa` no match
- ☑ `YZ` no match

---
```
/(a|)*(?1)b/
```
(#899) succeeded 3 times:

- ☑ `b`
- ☑ `ab`
- ☑ `aab`

---
```
/(a)++(?1)b/
```
(#900) succeeded 2 times:

- ☑ `ab` no match
- ☑ `aab` no match

---
```
/(a)*+(?1)b/
```
(#901) succeeded 2 times:

- ☑ `ab` no match
- ☑ `aab` no match

---
```
/(?1)(?:(b)){0}/
```
(#902) succeeded 1 times:

- ☑ `b`

---
```
/(foo ( \( ((?:(?> [^()]+ )|(?2))*) \) ) )/x
```
(#903) succeeded 1 times:

- ☑ `foo(bar(baz)+baz(bop))`

---
```
/(A (A|B(*ACCEPT)|C) D)(E)/x
```
(#904) succeeded -1 times:


---
```
/\A.*?(a|bc)/
```
(#905) succeeded 1 times:

- ☑ `ba`

---
```
/\A.*?(?:a|bc)++/
```
(#906) succeeded 1 times:

- ☑ `ba`

---
```
/\A.*?(a|bc)++/
```
(#907) succeeded 1 times:

- ☑ `ba`

---
```
/\A.*?(?:a|bc|d)/
```
(#908) succeeded 1 times:

- ☑ `ba`

---
```
/(?:(b))++/
```
(#909) succeeded 1 times:

- ☑ `beetle`

---
```
/(?(?=(a(*ACCEPT)z))a)/
```
(#910) succeeded -1 times:


---
```
/^(a)(?1)+ab/
```
(#911) succeeded 2 times:

- ☑ `aaaab`
- ☑ ``

---
```
/^(a)(?1)++ab/
```
(#912) succeeded 1 times:

- ☑ `aaaab` no match

---
```
/^(?=a(*:M))aZ/mark
```
(#913) succeeded -1 times:


---
```
/^(?!(*:M)b)aZ/mark
```
(#914) succeeded -1 times:


---
```
/(?(DEFINE)(a))?b(?1)/
```
(#915) succeeded 1 times:

- ☑ `backgammon`

---
```
/^\N+/
```
(#916) succeeded 2 times:

- ☑ `abc\ndef`
- ☑ ``

---
```
/^\N{1,}/
```
(#917) succeeded 1 times:

- ☑ `abc\ndef`

---
```
/(?(R)a+|(?R)b)/
```
(#918) succeeded -1 times:


---
```
/(?(R)a+|((?R))b)/
```
(#919) succeeded -1 times:


---
```
/((?(R)a+|(?1)b))/
```
(#920) succeeded -1 times:


---
```
/((?(R1)a+|(?1)b))/
```
(#921) succeeded -1 times:


---
```
/((?(R)a|(?1)))*/
```
(#922) succeeded -1 times:


---
```
/((?(R)a|(?1)))+/
```
(#923) succeeded -1 times:


---
```
/a(*:any 
name)/mark
```
(#924) succeeded -1 times:


---
```
/(?>(?&t)c|(?&t))(?(DEFINE)(?<t>a|b(*PRUNE)c))/
```
(#925) succeeded -1 times:


---
```
/^.*? (a(*THEN)b) c/x
```
(#926) succeeded -1 times:


---
```
/^.*? (a(*THEN)b|(*F)) c/x
```
(#927) succeeded -1 times:


---
```
/^.*? ( (a(*THEN)b) | (*F) ) c/x
```
(#928) succeeded -1 times:


---
```
/^.*? ( (a(*THEN)b) ) c/x
```
(#929) succeeded -1 times:


---
```
/^.*? (?:a(*THEN)b) c/x
```
(#930) succeeded -1 times:


---
```
/^.*? (?:a(*THEN)b|(*F)) c/x
```
(#931) succeeded -1 times:


---
```
/^.*? (?: (?:a(*THEN)b) | (*F) ) c/x
```
(#932) succeeded -1 times:


---
```
/^.*? (?: (?:a(*THEN)b) ) c/x
```
(#933) succeeded -1 times:


---
```
/^.*? (?>a(*THEN)b) c/x
```
(#934) succeeded -1 times:


---
```
/^.*? (?>a(*THEN)b|(*F)) c/x
```
(#935) succeeded -1 times:


---
```
/^.*? (?> (?>a(*THEN)b) | (*F) ) c/x
```
(#936) succeeded -1 times:


---
```
/^.*? (?> (?>a(*THEN)b) ) c/x
```
(#937) succeeded -1 times:


---
```
/^.*? (a(*THEN)b)++ c/x
```
(#938) succeeded -1 times:


---
```
/^.*? (a(*THEN)b|(*F))++ c/x
```
(#939) succeeded -1 times:


---
```
/^.*? ( (a(*THEN)b)++ | (*F) )++ c/x
```
(#940) succeeded -1 times:


---
```
/^.*? ( (a(*THEN)b)++ )++ c/x
```
(#941) succeeded -1 times:


---
```
/^.*? (?:a(*THEN)b)++ c/x
```
(#942) succeeded -1 times:


---
```
/^.*? (?:a(*THEN)b|(*F))++ c/x
```
(#943) succeeded -1 times:


---
```
/^.*? (?: (?:a(*THEN)b)++ | (*F) )++ c/x
```
(#944) succeeded -1 times:


---
```
/^.*? (?: (?:a(*THEN)b)++ )++ c/x
```
(#945) succeeded -1 times:


---
```
/^(?(?=a(*THEN)b)ab|ac)/
```
(#946) succeeded -1 times:


---
```
/^.*?(?(?=a)a|b(*THEN)c)/
```
(#947) succeeded -1 times:


---
```
/^.*?(?:(?(?=a)a|b(*THEN)c)|d)/
```
(#948) succeeded -1 times:


---
```
/^.*?(?(?=a)a(*THEN)b|c)/
```
(#949) succeeded -1 times:


---
```
/^.*(?=a(*THEN)b)/
```
(#950) succeeded -1 times:


---
```
/(?>a(*:m))/imsx,mark
```
(#951) succeeded -1 times:


---
```
/(?>(a)(*:m))/imsx,mark
```
(#952) succeeded -1 times:


---
```
/(?<=a(*ACCEPT)b)c/
```
(#953) succeeded -1 times:


---
```
/(?<=(a(*ACCEPT)b))c/
```
(#954) succeeded -1 times:


---
```
/(?<=(a(*COMMIT)b))c/
```
(#955) succeeded -1 times:


---
```
/(?<!a(*FAIL)b)c/
```
(#956) succeeded 2 times:

- ☑ `xcd`
- ☑ `acd`

---
```
/(?<=a(*:N)b)c/mark
```
(#957) succeeded -1 times:


---
```
/(?<=a(*PRUNE)b)c/
```
(#958) succeeded -1 times:


---
```
/(?<=a(*SKIP)b)c/
```
(#959) succeeded -1 times:


---
```
/(?<=a(*THEN)b)c/
```
(#960) succeeded -1 times:


---
```
/(a)(?2){2}(.)/
```
(#961) succeeded 1 times:

- ☑ `abcd`

---
```
/(*MARK:A)(*PRUNE:B)(C|X)/mark
```
(#962) succeeded -1 times:


---
```
/(*MARK:A)(*PRUNE:B)(C|X)/mark
```
(#963) succeeded -1 times:


---
```
/(*MARK:A)(*THEN:B)(C|X)/mark
```
(#964) succeeded -1 times:


---
```
/(*MARK:A)(*THEN:B)(C|X)/mark,no_start_optimize
```
(#965) succeeded -1 times:


---
```
/(*MARK:A)(*THEN:B)(C|X)/mark
```
(#966) succeeded -1 times:


---
```
/A(*MARK:A)A+(*SKIP)(B|Z) | AC/x,mark
```
(#967) succeeded -1 times:


---
```
/A(*MARK:A)A+(*MARK:B)(*SKIP:B)(B|Z) | AC/x,mark
```
(#968) succeeded -1 times:


---
```
/A(*:A)A+(*SKIP)(B|Z) | AC/x,mark
```
(#969) succeeded -1 times:


---
```
/A(*MARK:A)A+(*SKIP:)(B|Z) | AC/x,mark
```
(#970) succeeded -1 times:


---
```
/A(*:A)B|XX(*:B)Y/mark
```
(#971) succeeded -1 times:


---
```
/^(A(*THEN:A)B|C(*THEN:B)D)/mark
```
(#972) succeeded -1 times:


---
```
/^(A(*PRUNE:A)B|C(*PRUNE:B)D)/mark
```
(#973) succeeded -1 times:


---
```
/^(A(*PRUNE:)B|C(*PRUNE:B)D)/mark
```
(#974) succeeded -1 times:


---
```
/A(*PRUNE:A)B/mark
```
(#975) succeeded -1 times:


---
```
/A(*:A)B|X(*:A)Y/mark
```
(#976) succeeded -1 times:


---
```
/b(*:m)f|a(*:n)w/mark
```
(#977) succeeded -1 times:


---
```
/b(*:m)f|aw/mark
```
(#978) succeeded -1 times:


---
```
/A(*MARK:A)A+(*SKIP:B)(B|Z) | AAC/x,mark
```
(#979) succeeded -1 times:


---
```
/(?=a(*MARK:A)b)..x/mark
```
(#980) succeeded -1 times:


---
```
/(?=a(*MARK:A)b)..(*:Y)x/mark
```
(#981) succeeded -1 times:


---
```
/(?=a(*PRUNE:A)b)..x/mark
```
(#982) succeeded -1 times:


---
```
/(?=a(*PRUNE:A)b)..(*:Y)x/mark
```
(#983) succeeded -1 times:


---
```
/(?=a(*THEN:A)b)..x/mark
```
(#984) succeeded -1 times:


---
```
/(?=a(*THEN:A)b)..(*:Y)x/mark
```
(#985) succeeded -1 times:


---
```
/(another)?(\1?)test/
```
(#986) succeeded 1 times:

- ☑ `hello world test`

---
```
/(another)?(\1+)test/
```
(#987) succeeded 1 times:

- ☑ `hello world test` no match

---
```
/(a(*COMMIT)b){0}a(?1)|aac/
```
(#988) succeeded -1 times:


---
```
/((?:a?)*)*c/
```
(#989) succeeded 1 times:

- ☑ `aac`

---
```
/((?>a?)*)*c/
```
(#990) succeeded 1 times:

- ☑ `aac`

---
```
/(?>.*?a)(?<=ba)/
```
(#991) succeeded 1 times:

- ☑ `aba`

---
```
/(?:.*?a)(?<=ba)/
```
(#992) succeeded 1 times:

- ☑ `aba`

---
```
/(?>.*?a)b/s
```
(#993) succeeded 1 times:

- ☑ `aab`

---
```
/(?>.*?a)b/
```
(#994) succeeded 1 times:

- ☑ `aab`

---
```
/(?>^a)b/s
```
(#995) succeeded 1 times:

- ☑ `aab` no match

---
```
/(?>.*?)(?<=(abcd)|(wxyz))/
```
(#996) succeeded 2 times:

- ☑ `alphabetabcd`
- ☑ `endingwxyz`

---
```
/(?>.*)(?<=(abcd)|(wxyz))/
```
(#997) succeeded 2 times:

- ☑ `alphabetabcd`
- ☑ `endingwxyz`

---
```
/(?>.*)foo/
```
(#998) succeeded 2 times:

- ☑ `abcdfooxyz` no match
- ☑ `` no match

---
```
/(?>.*?)foo/
```
(#999) succeeded 1 times:

- ☑ `abcdfooxyz`

---
```
/(?:(a(*PRUNE)b)){0}(?:(?1)|ac)/
```
(#1000) succeeded -1 times:


---
```
/(?:(a(*SKIP)b)){0}(?:(?1)|ac)/
```
(#1001) succeeded -1 times:


---
```
/(?<=(*SKIP)ac)a/
```
(#1002) succeeded -1 times:


---
```
/A(*MARK:A)A+(*SKIP:B)(B|Z) | AC/x,mark
```
(#1003) succeeded -1 times:


---
```
/a(*SKIP:m)x|ac(*:n)(*SKIP:n)d|ac/mark
```
(#1004) succeeded -1 times:


---
```
/A(*SKIP:m)x|A(*SKIP:n)x|AB/mark
```
(#1005) succeeded -1 times:


---
```
/((*SKIP:r)d){0}a(*SKIP:m)x|ac(*:n)|ac/mark
```
(#1006) succeeded -1 times:


---
```
/aaaaa(*PRUNE)b|a+c/
```
(#1007) succeeded -1 times:


---
```
/aaaaa(*SKIP)(*PRUNE)b|a+c/
```
(#1008) succeeded -1 times:


---
```
/aaaaa(*SKIP:N)(*PRUNE)b|a+c/
```
(#1009) succeeded -1 times:


---
```
/aaaa(*:N)a(*SKIP:N)(*PRUNE)b|a+c/
```
(#1010) succeeded -1 times:


---
```
/aaaaa(*THEN)(*PRUNE)b|a+c/
```
(#1011) succeeded -1 times:


---
```
/aaaaa(*COMMIT)(*PRUNE)b|a+c/
```
(#1012) succeeded -1 times:


---
```
/aaaaa(*SKIP)b|a+c/
```
(#1013) succeeded -1 times:


---
```
/aaaaa(*PRUNE)(*SKIP)b|a+c/
```
(#1014) succeeded -1 times:


---
```
/aaaaa(*THEN)(*SKIP)b|a+c/
```
(#1015) succeeded -1 times:


---
```
/aaaaa(*COMMIT)(*SKIP)b|a+c/
```
(#1016) succeeded -1 times:


---
```
/aaaaa(*COMMIT)b|a+c/
```
(#1017) succeeded -1 times:


---
```
/aaaaa(*THEN)b|a+c/
```
(#1018) succeeded -1 times:


---
```
/aaaaa(*SKIP)(*THEN)b|a+c/
```
(#1019) succeeded -1 times:


---
```
/aaaaa(*PRUNE)(*THEN)b|a+c/
```
(#1020) succeeded -1 times:


---
```
/aaaaa(*COMMIT)(*THEN)b|a+c/
```
(#1021) succeeded -1 times:


---
```
/aaaaa(*:m)(*PRUNE:m)(*SKIP:m)m|a+/
```
(#1022) succeeded -1 times:


---
```
/aaaaa(*:m)(*MARK:m)(*PRUNE)(*SKIP:m)m|a+/
```
(#1023) succeeded -1 times:


---
```
/aaaaa(*:n)(*PRUNE:m)(*SKIP:m)m|a+/
```
(#1024) succeeded -1 times:


---
```
/aaaaa(*:n)(*MARK:m)(*PRUNE)(*SKIP:m)m|a+/
```
(#1025) succeeded -1 times:


---
```
/a(*MARK:A)aa(*PRUNE:A)a(*SKIP:A)b|a+c/
```
(#1026) succeeded -1 times:


---
```
/a(*MARK:A)aa(*MARK:A)a(*SKIP:A)b|a+c/
```
(#1027) succeeded -1 times:


---
```
/aaa(*PRUNE:A)a(*SKIP:A)b|a+c/
```
(#1028) succeeded -1 times:


---
```
/aaa(*MARK:A)a(*SKIP:A)b|a+c/
```
(#1029) succeeded -1 times:


---
```
/a(*:m)a(*COMMIT)(*SKIP:m)b|a+c/mark
```
(#1030) succeeded -1 times:


---
```
/.?(a|b(*THEN)c)/
```
(#1031) succeeded -1 times:


---
```
/(a(*COMMIT)b)c|abd/
```
(#1032) succeeded -1 times:


---
```
/(?=a(*COMMIT)b)abc|abd/
```
(#1033) succeeded -1 times:


---
```
/(?>a(*COMMIT)b)c|abd/
```
(#1034) succeeded -1 times:


---
```
/a(?=b(*COMMIT)c)[^d]|abd/
```
(#1035) succeeded -1 times:


---
```
/a(?=bc).|abd/
```
(#1036) succeeded 3 times:

- ☑ `abd`
- ☑ `abc`
- ☑ ``

---
```
/a(?>b(*COMMIT)c)d|abd/
```
(#1037) succeeded -1 times:


---
```
/a(?>bc)d|abd/
```
(#1038) succeeded 1 times:

- ☑ `abceabd`

---
```
/(?>a(*COMMIT)b)c|abd/
```
(#1039) succeeded -1 times:


---
```
/(?>a(*COMMIT)c)d|abd/
```
(#1040) succeeded -1 times:


---
```
/((?=a(*COMMIT)b)ab|ac){0}(?:(?1)|a(c))/
```
(#1041) succeeded -1 times:


---
```
/^(a)?(?(1)a|b)+$/
```
(#1042) succeeded 1 times:

- ☑ `a` no match

---
```
/(?=a\Kb)ab/
```
(#1043) succeeded -1 times:


---
```
/(?!a\Kb)ac/
```
(#1044) succeeded -1 times:


---
```
/^abc(?<=b\Kc)d/
```
(#1045) succeeded -1 times:


---
```
/^abc(?<!b\Kq)d/
```
(#1046) succeeded -1 times:


---
```
/A(*PRUNE:A)A+(*SKIP:A)(B|Z) | AC/x,mark
```
(#1047) succeeded -1 times:


---
```
/^((abc|abcx)(*THEN)y|abcd)/
```
(#1048) succeeded -1 times:


---
```
/^((yes|no)(*THEN)(*F))?/
```
(#1049) succeeded -1 times:


---
```
/(A (.*)   C? (*THEN)  | A D) (*FAIL)/x
```
(#1050) succeeded -1 times:


---
```
/(A (.*)   C? (*THEN)  | A D) z/x
```
(#1051) succeeded -1 times:


---
```
/(A (.*)   C? (*THEN)  | A D) \s* (*FAIL)/x
```
(#1052) succeeded -1 times:


---
```
/(A (.*)   C? (*THEN)  | A D) \s* z/x
```
(#1053) succeeded -1 times:


---
```
/(A (.*)   (?:C|) (*THEN)  | A D) (*FAIL)/x
```
(#1054) succeeded -1 times:


---
```
/(A (.*)   (?:C|) (*THEN)  | A D) z/x
```
(#1055) succeeded -1 times:


---
```
/(A (.*)   C{0,6} (*THEN)  | A D) (*FAIL)/x
```
(#1056) succeeded -1 times:


---
```
/(A (.*)   C{0,6} (*THEN)  | A D) z/x
```
(#1057) succeeded -1 times:


---
```
/(A (.*)   (CE){0,6} (*THEN)  | A D) (*FAIL)/x
```
(#1058) succeeded -1 times:


---
```
/(A (.*)   (CE){0,6} (*THEN)  | A D) z/x
```
(#1059) succeeded -1 times:


---
```
/(A (.*)   (CE*){0,6} (*THEN)  | A D) (*FAIL)/x
```
(#1060) succeeded -1 times:


---
```
/(A (.*)   (CE*){0,6} (*THEN)  | A D) z/x
```
(#1061) succeeded -1 times:


---
```
/(?=a(*COMMIT)b|ac)ac|ac/
```
(#1062) succeeded -1 times:


---
```
/(?=a(*COMMIT)b|(ac)) ac | (a)c/x
```
(#1063) succeeded -1 times:


---
```
/(?(?!b(*THEN)a)bn|bnn)/
```
(#1064) succeeded -1 times:


---
```
/(?!b(*SKIP)a)bn|bnn/
```
(#1065) succeeded -1 times:


---
```
/(?(?!b(*SKIP)a)bn|bnn)/
```
(#1066) succeeded -1 times:


---
```
/(?!b(*PRUNE)a)bn|bnn/
```
(#1067) succeeded -1 times:


---
```
/(?(?!b(*PRUNE)a)bn|bnn)/
```
(#1068) succeeded -1 times:


---
```
/(?!b(*COMMIT)a)bn|bnn/
```
(#1069) succeeded -1 times:


---
```
/(?(?!b(*COMMIT)a)bn|bnn)/
```
(#1070) succeeded -1 times:


---
```
/(?=b(*SKIP)a)bn|bnn/
```
(#1071) succeeded -1 times:


---
```
/(?=b(*THEN)a)bn|bnn/
```
(#1072) succeeded -1 times:


---
```
/^(?!a(*SKIP)b)/
```
(#1073) succeeded -1 times:


---
```
/^(?!a(*SKIP)b)../
```
(#1074) succeeded -1 times:


---
```
/(?!a(*SKIP)b)../
```
(#1075) succeeded -1 times:


---
```
/^(?(?!a(*SKIP)b))/
```
(#1076) succeeded -1 times:


---
```
/^(?!a(*PRUNE)b)../
```
(#1077) succeeded -1 times:


---
```
/(?!a(*PRUNE)b)../
```
(#1078) succeeded -1 times:


---
```
/(?!a(*COMMIT)b)ac|cd/
```
(#1079) succeeded -1 times:


---
```
/\A.*?(?:a|bc)/
```
(#1080) succeeded 1 times:

- ☑ `ba`

---
```
/^(A(*THEN)B|C(*THEN)D)/
```
(#1081) succeeded -1 times:


---
```
/(*:m(m)(?&y)(?(DEFINE)(?<y>b))/mark
```
(#1082) succeeded -1 times:


---
```
/(*PRUNE:m(m)(?&y)(?(DEFINE)(?<y>b))/mark
```
(#1083) succeeded -1 times:


---
```
/(*SKIP:m(m)(?&y)(?(DEFINE)(?<y>b))/mark
```
(#1084) succeeded -1 times:


---
```
/(*THEN:m(m)(?&y)(?(DEFINE)(?<y>b))/mark
```
(#1085) succeeded -1 times:


---
```
/^\d*\w{4}/
```
(#1086) succeeded 2 times:

- ☑ `1234`
- ☑ `123` no match

---
```
/^[^b]*\w{4}/
```
(#1087) succeeded 2 times:

- ☑ `aaaa`
- ☑ `aaa` no match

---
```
/^[^b]*\w{4}/i
```
(#1088) succeeded 2 times:

- ☑ `aaaa`
- ☑ `aaa` no match

---
```
/^a*\w{4}/
```
(#1089) succeeded 2 times:

- ☑ `aaaa`
- ☑ `aaa` no match

---
```
/^a*\w{4}/i
```
(#1090) succeeded 2 times:

- ☑ `aaaa`
- ☑ `aaa` no match

---
```
/(?:(?<n>foo)|(?<n>bar))\k<n>/dupnames
```
(#1091) succeeded 2 times:

- ☑ `foofoo`
- ☑ `barbar`

---
```
/(?<n>A)(?:(?<n>foo)|(?<n>bar))\k<n>/dupnames
```
(#1092) succeeded 4 times:

- ☑ `AfooA`
- ☑ `AbarA`
- ☑ `Afoofoo` no match
- ☑ `Abarbar` no match

---
```
/^(\d+)\s+IN\s+SOA\s+(\S+)\s+(\S+)\s*\(\s*$/
```
(#1093) succeeded 1 times:

- ☑ `1 IN SOA non-sp1 non-sp2(`

---
```
/^ (?:(?<A>A)|(?'B'B)(?<A>A)) (?('A')x) (?(<B>)y)$/x,dupnames
```
(#1094) succeeded 3 times:

- ☑ `Ax`
- ☑ `BAxy`
- ☑ ``

---
```
/^A\xZ/
```
(#1095) succeeded 1 times:

- ☑ `A Z`

---
```
/^A\o{123}B/
```
(#1096) succeeded 1 times:

- ☑ `ASB`

---
```
/ ^ a + + b $ /x
```
(#1097) succeeded 2 times:

- ☑ `aaaab`
- ☑ ``

---
```
/ ^ a + #comment
  + b $ /x
```
(#1098) succeeded 2 times:

- ☑ `aaaab`
- ☑ ``

---
```
/ ^ a + #comment
  #comment
  + b $ /x
```
(#1099) succeeded 2 times:

- ☑ `aaaab`
- ☑ ``

---
```
/ ^ (?> a + ) b $ /x
```
(#1100) succeeded 1 times:

- ☑ `aaaab`

---
```
/ ^ ( a + ) + + \w $ /x
```
(#1101) succeeded 1 times:

- ☑ `aaaab`

---
```
/(?:a\Kb)*+/aftertext
```
(#1102) succeeded -1 times:


---
```
/(?>a\Kb)*/aftertext
```
(#1103) succeeded -1 times:


---
```
/(?:a\Kb)*/aftertext
```
(#1104) succeeded -1 times:


---
```
/(a\Kb)*+/aftertext
```
(#1105) succeeded -1 times:


---
```
/(a\Kb)*/aftertext
```
(#1106) succeeded -1 times:


---
```
/(?:x|(?:(xx|yy)+|x|x|x|x|x)|a|a|a)bc/
```
(#1107) succeeded 1 times:

- ☑ `acb` no match

---
```
/\A(?:[^\"]++|\"(?:[^\"]*+|\"\")*+\")++/
```
(#1108) succeeded 1 times:

- ☑ `NON QUOTED "QUOT""ED" AFTER "NOT MATCHED`

---
```
/\A(?:[^\"]++|\"(?:[^\"]++|\"\")*+\")++/
```
(#1109) succeeded 1 times:

- ☑ `NON QUOTED "QUOT""ED" AFTER "NOT MATCHED`

---
```
/\A(?:[^\"]++|\"(?:[^\"]++|\"\")++\")++/
```
(#1110) succeeded 1 times:

- ☑ `NON QUOTED "QUOT""ED" AFTER "NOT MATCHED`

---
```
/\A([^\"1]++|[\"2]([^\"3]*+|[\"4][\"5])*+[\"6])++/
```
(#1111) succeeded 1 times:

- ☑ `NON QUOTED "QUOT""ED" AFTER "NOT MATCHED`

---
```
/^\w+(?>\s*)(?<=\w)/
```
(#1112) succeeded 1 times:

- ☑ `test test`

---
```
/(?P<same>a)(?P<same>b)/g,dupnames
```
(#1113) succeeded -1 times:


---
```
/(?P<same>a)(?P<same>b)(?P=same)/g,dupnames
```
(#1114) succeeded -1 times:


---
```
/(?P=same)?(?P<same>a)(?P<same>b)/g,dupnames
```
(#1115) succeeded -1 times:


---
```
/(?:(?P=same)?(?:(?P<same>a)|(?P<same>b))(?P=same))+/g,dupnames
```
(#1116) succeeded -1 times:


---
```
/(?:(?P=same)?(?:(?P=same)(?P<same>a)(?P=same)|(?P=same)?(?P<same>b)(?P=same)){2}(?P=same)(?P<same>c)(?P=same)){2}(?P<same>z)?/g,dupnames
```
(#1117) succeeded -1 times:


---
```
/(?P<Name>a)?(?P<Name2>b)?(?(<Name>)c|d)*l/
```
(#1118) succeeded 4 times:

- ☑ `acl`
- ☑ `bdl`
- ☑ `adl`
- ☑ `bcl`

---
```
/\sabc/
```
(#1119) succeeded 1 times:

- ☑ `abc`

---
```
/[\Qa]\E]+/
```
(#1120) succeeded 1 times:

- ☑ `aa]]`

---
```
/[\Q]a\E]+/
```
(#1121) succeeded 1 times:

- ☑ `aa]]`

---
```
/A((((((((a))))))))\8B/
```
(#1122) succeeded 1 times:

- ☑ `AaaB`

---
```
/A(((((((((a)))))))))\9B/
```
(#1123) succeeded 2 times:

- ☑ `AaaB`
- ☑ ``

---
```
/A[\8\9]B/
```
(#1124) succeeded 2 times:

- ☑ `A8B`
- ☑ `A9B`

---
```
/(|ab)*?d/
```
(#1125) succeeded 2 times:

- ☑ `abd`
- ☑ `xyd`

---
```
/(?:((abcd))|(((?:(?:(?:(?:abc|(?:abcdef))))b)abcdefghi)abc)|((*ACCEPT)))/
```
(#1126) succeeded -1 times:


---
```
/(\2|a)(\1)/
```
(#1127) succeeded 1 times:

- ☑ `aaa`

---
```
/(\2)(\1)/
```
(#1128) succeeded 0 times:


---
```
/Z*(|d*){216}/
```
(#1129) succeeded 0 times:


---
```
/(?1)(?#?'){8}(a)/
```
(#1130) succeeded 1 times:

- ☑ `baaaaaaaaac`

---
```
/((((((((((((x))))))))))))\12/
```
(#1131) succeeded 1 times:

- ☑ `xx`

---
```
/A[\8]B[\9]C/
```
(#1132) succeeded 1 times:

- ☑ `A8B9C`

---
```
/(?1)()((((((\1++))\x85)+)|))/
```
(#1133) succeeded 1 times:

- ☑ ``

---
```
/(?|(\k'Pm')|(?'Pm'))/
```
(#1134) succeeded 1 times:

- ☑ `abcd`

---
```
/(?|(aaa)|(b))\g{1}/
```
(#1135) succeeded 2 times:

- ☑ `aaaaaa`
- ☑ `bb`

---
```
/(?|(aaa)|(b))(?1)/
```
(#1136) succeeded 3 times:

- ☑ `aaaaaa`
- ☑ `baaa`
- ☑ `bb` no match

---
```
/(?|(aaa)|(b))/
```
(#1137) succeeded 2 times:

- ☑ `xaaa`
- ☑ `xbc`

---
```
/(?|(?'a'aaa)|(?'a'b))\k'a'/
```
(#1138) succeeded 2 times:

- ☑ `aaaaaa`
- ☑ `bb`

---
```
/(?|(?'a'aaa)|(?'a'b))(?'a'cccc)\k'a'/dupnames
```
(#1139) succeeded 2 times:

- ☑ `aaaccccaaa`
- ☑ `bccccb`

---
```
/x (*MARK:ab cd # comment
ef) x/x,mark
```
(#1140) succeeded -1 times:


---
```
/(?<=a(B){0}c)X/
```
(#1141) succeeded 1 times:

- ☑ `acX`

---
```
/(?<DEFINE>b)(?(DEFINE)(a+))(?&DEFINE)/
```
(#1142) succeeded 2 times:

- ☑ `bbbb`
- ☑ `baaab` no match

---
```
/(?=.*[A-Z])(?=.*[a-z])(?=.*[0-9])(?=.*[,;:])(?=.{8,16})(?!.*[\s])/
```
(#1143) succeeded 1 times:

- ☑ `   Fred:099`

---
```
/(?=.*X)X$/
```
(#1144) succeeded 1 times:

- ☑ `  X`

---
```
/(?s)(?=.*?)b/
```
(#1145) succeeded 1 times:

- ☑ `aabc`

---
```
/(Z)(a)\2{1,2}?(?-i)\1X/i
```
(#1146) succeeded -1 times:


---
```
/(?'c')XX(?'YYYYYYYYYYYYYYYYYYYYYYYCl')/
```
(#1147) succeeded 0 times:


---
```
/[s[:digit:]\E-H]+/
```
(#1148) succeeded 1 times:

- ☑ `s09-H`

---
```
/[s[:digit:]\Q\E-H]+/
```
(#1149) succeeded 1 times:

- ☑ `s09-H`

---
```
/a+(?:|b)a/
```
(#1150) succeeded 1 times:

- ☑ `aaaa`

---
```
/X?(R||){3335}/
```
(#1151) succeeded 0 times:


---
```
/(?1)(A(*COMMIT)|B)D/
```
(#1152) succeeded -1 times:


---
```
/(?(DEFINE)(?<m> 1? (?=(?<cond>2)?) 1 2 (?('cond')|3)))
    \A
    ()
    (?&m)
    \Z/x
```
(#1153) succeeded 1 times:

- ☑ `123`

---
```
/^(?: 
(?: A| (1? (?=(?<cond>2)?) (1) 2 (?('cond')|3)) )
(Z)
)+$/x
```
(#1154) succeeded 3 times:

- ☑ `AZ123Z`
- ☑ `AZ12Z` no match
- ☑ `` no match

---
```
/^ (?(DEFINE) ( (?!(a)\2b)..) )   ()(?1)  /x
```
(#1155) succeeded 3 times:

- ☑ `acb`
- ☑ `aab` no match
- ☑ `` no match

---
```
/(?>ab|abab){1,5}?M/
```
(#1156) succeeded 1 times:

- ☑ `abababababababababababM`

---
```
/(?>ab|abab){2}?M/
```
(#1157) succeeded 1 times:

- ☑ `abababM`

---
```
/((?(?=(a))a)+k)/
```
(#1158) succeeded 1 times:

- ☑ `bbak`

---
```
/((?(?=(a))a|)+k)/
```
(#1159) succeeded 1 times:

- ☑ `bbak`

---
```
/(?(?!(b))a|b)+k/
```
(#1160) succeeded 1 times:

- ☑ `ababbalbbadabak`

---
```
/(?!(b))c|b/
```
(#1161) succeeded 2 times:

- ☑ `Ab`
- ☑ `Ac`

---
```
/(?=(b))b|c/
```
(#1162) succeeded 2 times:

- ☑ `Ab`
- ☑ `Ac`

---
```
/^(.|(.)(?1)\2)$/
```
(#1163) succeeded 5 times:

- ☑ `a`
- ☑ `aba`
- ☑ `abcba`
- ☑ `ababa`
- ☑ `abcdcba`

---
```
/^((.)(?1)\2|.?)$/
```
(#1164) succeeded 8 times:

- ☑ `a`
- ☑ `aba`
- ☑ `abba`
- ☑ `abcba`
- ☑ `ababa`
- ☑ `abccba`
- ☑ `abcdcba`
- ☑ `abcddcba`

---
```
/^(.)(\1|a(?2))/
```
(#1165) succeeded 1 times:

- ☑ `bab`

---
```
/^(.|(.)(?1)?\2)$/
```
(#1166) succeeded 2 times:

- ☑ `abcba`
- ☑ ``

---
```
/^(?(?=(a))abc|def)/
```
(#1167) succeeded 1 times:

- ☑ `abc`

---
```
/^(?(?!(a))def|abc)/
```
(#1168) succeeded 1 times:

- ☑ `abc`

---
```
/^(?(?=(a)(*ACCEPT))abc|def)/
```
(#1169) succeeded -1 times:


---
```
/^(?(?!(a)(*ACCEPT))def|abc)/
```
(#1170) succeeded -1 times:


---
```
/^(?1)\d{3}(a)/
```
(#1171) succeeded 1 times:

- ☑ `a123a`

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
(#1172) succeeded 12 times:

- ☑ `Alan Other <user@dom.ain>`
- ☑ `<user@dom.ain>`
- ☑ `user@dom.ain`
- ☑ `user@[]`
- ☑ `user@[domain literal]`
- ☑ `user@[domain literal with "[square brackets"] inside]`
- ☑ `"A. Other" <user.1234@dom.ain> (a comment)`
- ☑ `A. Other <user.1234@dom.ain> (a comment)`
- ☑ `"/s=user/ou=host/o=place/prmd=uu.yy/admd= /c=gb/"@x400-re.lay`
- ☑ `A missing angle <user@some.where` no match
- ☑ `The quick brown fox` no match
- ☑ `` no match

---
```
/(?sx)(?(DEFINE)

(?<assertion>         (?&simple_assertion) | (?&lookaround) )

(?<atomic_group>      \( \? > (?&regex) \) )

(?<back_reference>    \\ \d+ |
                      \\g (?: [+-]?\d+ | \{ (?: [+-]?\d+ | (?&groupname) ) \} ) |
                      \\k <(?&groupname)> |
                      \\k '(?&groupname)' |
                      \\k \{ (?&groupname) \} |
                      \( \? P= (?&groupname) \) )

(?<branch>            (?:(?&assertion) |
                         (?&callout) |
                         (?&comment) |
                         (?&option_setting) |
                         (?&qualified_item) |
                         (?&quoted_string) |
                         (?&quoted_string_empty) | 
                         (?&special_escape) |
                         (?&verb)
                      )* )

(?<callout>           \(\?C (?: \d+ | 
                      (?: (?<D>["'`^%\#\$]) 
                        (?: \k'D'\k'D' | (?!\k'D') . )* \k'D' |
                      \{ (?: \}\} | [^}]*+ )* \} ) 
                      )? \) )

(?<capturing_group>   \( (?: \? P? < (?&groupname) > | \? ' (?&groupname) ' )?
                      (?&regex) \) )

(?<character_class>   \[ \^?+ (?: \] (?&class_item)* | (?&class_item)+ ) \] )

(?<character_type>    (?! \\N\{\w+\} ) \\ [dDsSwWhHvVRN] )

(?<class_item>        (?: \[ : (?:
                      alnum|alpha|ascii|blank|cntrl|digit|graph|lower|print|
                      punct|space|upper|word|xdigit
                      ) : \] |
                      (?&quoted_string) |  
                      (?&quoted_string_empty) | 
                      (?&escaped_character) | 
                      (?&character_type) | 
                      [^]] ) )

(?<comment>           \(\?\# [^)]* \) | (?&quoted_string_empty) | \\E )

(?<condition>         (?: \( [+-]? \d+ \) |
                          \( < (?&groupname) > \) |
                          \( ' (?&groupname) ' \) |
                          \( R \d* \) |
                          \( R & (?&groupname) \) |
                          \( (?&groupname) \) | 
                          \( DEFINE \) |
                          \( VERSION >?=\d+(?:\.\d\d?)? \) |
                          (?&callout)?+ (?&comment)* (?&lookaround) ) )

(?<conditional_group> \(\? (?&condition) (?&branch) (?: \| (?&branch) )? \) )

(?<delimited_regex>   (?<delimiter> [-\x{2f}!"'`=_:;,%&@~]) (?&regex) 
                      \k'delimiter' .* )

(?<escaped_character> \\ (?: 0[0-7]{1,2} | [0-7]{1,3} | o\{ [0-7]+ \} |
                      x \{ (*COMMIT) [[:xdigit:]]* \} | x [[:xdigit:]]{0,2} | 
                      [aefnrt] | c[[:print:]] |
                      [^[:alnum:]] ) )

(?<group>             (?&capturing_group) | (?&non_capturing_group) |
                      (?&resetting_group) | (?&atomic_group) |
                      (?&conditional_group) )

(?<groupname>         [a-zA-Z_]\w* )

(?<literal_character> (?! (?&range_qualifier) ) [^[()|*+?.\$\\] )

(?<lookaround>        \(\? (?: = | ! | <= | <! ) (?&regex) \) )

(?<non_capturing_group> \(\? [iJmnsUx-]* : (?&regex) \) )

(?<option_setting>    \(\? [iJmnsUx-]* \) )

(?<qualified_item>    (?:\. |
                         (?&lookaround) |
                         (?&back_reference) |
                         (?&character_class) |
                         (?&character_type) |
                         (?&escaped_character) |
                         (?&group) |
                         (?&subroutine_call) |
                         (?&literal_character) |
                         (?&quoted_string) 
                      ) (?&comment)? (?&qualifier)? )

(?<qualifier>         (?: [?*+] | (?&range_qualifier) ) [+?]? )

(?<quoted_string>     (?: \\Q (?: (?!\\E | \k'delimiter') . )++ (?: \\E | ) ) ) 
                      
(?<quoted_string_empty>  \\Q\\E ) 

(?<range_qualifier>   \{ (?: \d+ (?: , \d* )? | , \d+ ) \} )

(?<regex>             (?&start_item)* (?&branch) (?: \| (?&branch) )* )

(?<resetting_group>   \( \? \| (?&regex) \) )

(?<simple_assertion>  \^ | \$ | \\A | \\b | \\B | \\G | \\z | \\Z )

(?<special_escape>    \\K )

(?<start_item>        \( \* (?:
                      ANY |
                      ANYCRLF |
                      BSR_ANYCRLF |
                      BSR_UNICODE |
                      CR |
                      CRLF |
                      LF |
                      LIMIT_MATCH=\d+ |
                      LIMIT_DEPTH=\d+ |
                      LIMIT_HEAP=\d+ | 
                      NOTEMPTY |
                      NOTEMPTY_ATSTART |
                      NO_AUTO_POSSESS |
                      NO_DOTSTAR_ANCHOR |
                      NO_JIT |
                      NO_START_OPT |
                      NUL |
                      UTF |
                      UCP ) \) )

(?<subroutine_call>   (?: \(\?R\) | \(\?[+-]?\d+\) |
                      \(\? (?: & | P> ) (?&groupname) \) |
                      \\g < (?&groupname) > |
                      \\g ' (?&groupname) ' |
                      \\g < [+-]? \d+ > |
                      \\g ' [+-]? \d+ ) )

(?<verb>              \(\* (?: ACCEPT | FAIL | F | COMMIT |
                      (?:MARK)?:(?&verbname) |
                      (?:PRUNE|SKIP|THEN) (?: : (?&verbname)? )? ) \) )

(?<verbname>          [^)]+ )

) # End DEFINE
# Kick it all off...
^(?&delimited_regex)$/subject_literal,jitstack=256
```
(#1173) succeeded -1 times:


---
```
/<(?x:[a b])>/xx
```
(#1174) succeeded 1 times:

- ☑ `< >`

---
```
/<(?:[a b])>/xx
```
(#1175) succeeded 1 times:

- ☑ `< >` no match

---
```
/<(?xxx:[a b])>/
```
(#1176) succeeded 2 times:

- ☑ `< >` no match
- ☑ `` no match

---
```
/<(?-x:[a b])>/xx
```
(#1177) succeeded 1 times:

- ☑ `< >`

---
```
/[[:digit:]-]+/
```
(#1178) succeeded 1 times:

- ☑ `12-24`

---
```
/((?<=((*ACCEPT)) )\1?\b) /
```
(#1179) succeeded -1 times:


---
```
/((?<=((*ACCEPT))X)\1?Y)\1/
```
(#1180) succeeded -1 times:


---
```
/((?<=((*ACCEPT))X)\1?Y(*ACCEPT))\1/
```
(#1181) succeeded -1 times:


---
```
/(?(DEFINE)(?<optional_a>a?)X)^(?&optional_a)a$/
```
(#1182) succeeded 2 times:

- ☑ `aa`
- ☑ `a`

---
```
/^(a?)b(?1)a/
```
(#1183) succeeded 4 times:

- ☑ `abaa`
- ☑ `aba`
- ☑ `baa`
- ☑ `ba`

---
```
/^(a?)+b(?1)a/
```
(#1184) succeeded 4 times:

- ☑ `abaa`
- ☑ `aba`
- ☑ `baa`
- ☑ `ba`

---
```
/^(a?)++b(?1)a/
```
(#1185) succeeded 4 times:

- ☑ `abaa`
- ☑ `aba`
- ☑ `baa`
- ☑ `ba`

---
```
/^(a?)+b/
```
(#1186) succeeded 3 times:

- ☑ `b`
- ☑ `ab`
- ☑ `aaab`

---
```
/(?=a+)a(a+)++b/
```
(#1187) succeeded 1 times:

- ☑ `aab`

---
```
/(?<=\G.)/g,aftertext
```
(#1188) succeeded -1 times:


---
```
/(?<=(?=.)?)/
```
(#1189) succeeded -1 times:


---
```
/(?<=(?=.)?+)/
```
(#1190) succeeded -1 times:


---
```
/(?<=(?=.)*)/
```
(#1191) succeeded -1 times:


---
```
/(?<=(?=.){4,5})/
```
(#1192) succeeded -1 times:


---
```
/(?<=(?=.){4,5}x)/
```
(#1193) succeeded -1 times:


---
```
/a(?=.(*:X))(*SKIP:X)(*F)|(.)/
```
(#1194) succeeded -1 times:


---
```
/a(?>(*:X))(*SKIP:X)(*F)|(.)/
```
(#1195) succeeded -1 times:


---
```
/a(?:(*:X))(*SKIP:X)(*F)|(.)/
```
(#1196) succeeded -1 times:


---
```
/(?>a(*:1))(?>b(*:1))(*SKIP:1)x|.*/
```
(#1197) succeeded -1 times:


---
```
/(?>a(*:1))(?>b)(*SKIP:1)x|.*/
```
(#1198) succeeded -1 times:


---
```
/a(*ACCEPT:X)b/
```
(#1199) succeeded -1 times:


---
```
/(?=a(*ACCEPT:QQ)bc)axyz/
```
(#1200) succeeded -1 times:


---
```
/(?(DEFINE)(a(*ACCEPT:X)))(?1)b/
```
(#1201) succeeded -1 times:


---
```
/a(*F:X)b/
```
(#1202) succeeded 2 times:

- ☑ `abc` no match
- ☑ `` no match

---
```
/(?(DEFINE)(a(*F:X)))(?1)b/
```
(#1203) succeeded 1 times:

- ☑ `abc` no match

---
```
/a(*COMMIT:X)b/
```
(#1204) succeeded -1 times:


---
```
/(?(DEFINE)(a(*COMMIT:X)))(?1)b/
```
(#1205) succeeded -1 times:


---
```
/a+(*:Z)b(*COMMIT:X)(*SKIP:Z)c|.*/
```
(#1206) succeeded -1 times:


---
```
/a+(*:Z)b(*COMMIT:X)(*SKIP:X)c|.*/
```
(#1207) succeeded -1 times:


---
```
/a(*COMMIT:X)b/
```
(#1208) succeeded -1 times:


---
```
/(.COMMIT)(*COMMIT::::::::::interal error:::)/
```
(#1209) succeeded -1 times:


---
```
/(*COMMIT:��)/
```
(#1210) succeeded -1 times:


---
```
/(*COMMIT:]w)/
```
(#1211) succeeded -1 times:


---
```
/(?i)A(?^)B(?^x:C D)(?^i)e f/
```
(#1212) succeeded 3 times:

- ☑ `aBCDE F`
- ☑ `aBCDEF` no match
- ☑ `AbCDe f` no match

---
```
/(*pla:foo).{6}/
```
(#1213) succeeded 2 times:

- ☑ `abcfoobarxyz`
- ☑ `abcfooba` no match

---
```
/(*positive_lookahead:foo).{6}/
```
(#1214) succeeded 2 times:

- ☑ `abcfoobarxyz`
- ☑ ``

---
```
/(?(*pla:foo).{6}|a..)/
```
(#1215) succeeded 2 times:

- ☑ `foobarbaz`
- ☑ `abcfoobar`

---
```
/(?(*positive_lookahead:foo).{6}|a..)/
```
(#1216) succeeded 3 times:

- ☑ `foobarbaz`
- ☑ `abcfoobar`
- ☑ ``

---
```
/(*plb:foo)bar/
```
(#1217) succeeded 2 times:

- ☑ `abcfoobar`
- ☑ `abcbarfoo` no match

---
```
/(*positive_lookbehind:foo)bar/
```
(#1218) succeeded 3 times:

- ☑ `abcfoobar`
- ☑ `abcbarfoo` no match
- ☑ `` no match

---
```
/(?(*plb:foo)bar|baz)/
```
(#1219) succeeded 5 times:

- ☑ `abcfoobar`
- ☑ `bazfoobar`
- ☑ `abcbazfoobar`
- ☑ `foobazfoobar`
- ☑ ``

---
```
/(?(*positive_lookbehind:foo)bar|baz)/
```
(#1220) succeeded 5 times:

- ☑ `abcfoobar`
- ☑ `bazfoobar`
- ☑ `abcbazfoobar`
- ☑ `foobazfoobar`
- ☑ ``

---
```
/(*nlb:foo)bar/
```
(#1221) succeeded 2 times:

- ☑ `abcbarfoo`
- ☑ `abcfoobar` no match

---
```
/(*negative_lookbehind:foo)bar/
```
(#1222) succeeded 3 times:

- ☑ `abcbarfoo`
- ☑ `abcfoobar` no match
- ☑ `` no match

---
```
/(?(*nlb:foo)bar|baz)/
```
(#1223) succeeded 4 times:

- ☑ `abcfoobaz`
- ☑ `abcbarbaz`
- ☑ `abcfoobar` no match
- ☑ `` no match

---
```
/(?(*negative_lookbehind:foo)bar|baz)/
```
(#1224) succeeded 4 times:

- ☑ `abcfoobaz`
- ☑ `abcbarbaz`
- ☑ `abcfoobar` no match
- ☑ `` no match

---
```
/(*atomic:a+)\w/
```
(#1225) succeeded 2 times:

- ☑ `aaab`
- ☑ `aaaa` no match

---
```
/   (?<word> \w+ )*    \.   /xi
```
(#1226) succeeded 2 times:

- ☑ `pokus.`
- ☑ ``

---
```
/(?(DEFINE) (?<word> \w+ ) ) (?&word)*   \./xi
```
(#1227) succeeded 1 times:

- ☑ `pokus.`

---
```
/(?(DEFINE) (?<word> \w+ ) ) ( (?&word)* )   \./xi
```
(#1228) succeeded 1 times:

- ☑ `pokus.`

---
```
/(?&word)*  (?(DEFINE) (?<word> \w+ ) )  \./xi
```
(#1229) succeeded 1 times:

- ☑ `pokus.`

---
```
/(?&word)*  \. (?<word> \w+ )/xi
```
(#1230) succeeded 1 times:

- ☑ `pokus.hokus`

---
```
/a(?(?=(*:2)b).)/mark
```
(#1231) succeeded -1 times:


---
```
/a(?(?!(*:2)b).)/mark
```
(#1232) succeeded -1 times:


---
```
/(?:a|ab){1}+c/
```
(#1233) succeeded 1 times:

- ☑ `abc` no match

---
```
/(a|ab){1}+c/
```
(#1234) succeeded 2 times:

- ☑ `abc` no match
- ☑ `` no match

---
```
/(a+){1}+a/
```
(#1235) succeeded 1 times:

- ☑ `aaaa` no match

---
```
/(?(DEFINE)(a|ab))(?1){1}+c/
```
(#1236) succeeded 1 times:

- ☑ `abc` no match

---
```
/(?:a|(?=b)|.)*\z/
```
(#1237) succeeded 2 times:

- ☑ `abc`
- ☑ ``

---
```
/(?:a|(?=b)|.)*/
```
(#1238) succeeded 2 times:

- ☑ `abc`
- ☑ ``

---
```
/(?<=a(*SKIP)x)|c/
```
(#1239) succeeded -1 times:


---
```
/(?<=a(*SKIP)x)|d/
```
(#1240) succeeded -1 times:


---
```
/(?<=(?=.(?<=x)))/aftertext
```
(#1241) succeeded -1 times:


---
```
/(?<=(?=(?<=a)))b/
```
(#1242) succeeded 1 times:

- ☑ `ab`

---
```
/^(?<A>a)(?(<A>)b)((?<=b).*)$/
```
(#1243) succeeded 1 times:

- ☑ `abc`

---
```
/^(a\1?){4}$/
```
(#1244) succeeded 2 times:

- ☑ `aaaa`
- ☑ `aaaaaa`

---
```
/^((\1+)|\d)+133X$/
```
(#1245) succeeded 1 times:

- ☑ `111133X`

---
```
/^(?=.*(?=(([A-Z]).*(?(1)\1)))(?!.+\2)){26}/i
```
(#1246) succeeded 0 times:

- ☐ `The quick brown fox jumps over the lazy dog.`
- ☐ `Jackdaws love my big sphinx of quartz.`
- ☐ `Pack my box with five dozen liquor jugs.`
- ☐ `The quick brown fox jumps over the lazy cat.` no match
- ☐ `Hackdaws love my big sphinx of quartz.` no match
- ☐ `Pack my fox with five dozen liquor jugs.` no match

---
```
/^(?>.*?([A-Z])(?!.*\1)){26}/i
```
(#1247) succeeded 4 times:

- ☐ `The quick brown fox jumps over the lazy dog.`
- ☑ `Jackdaws love my big sphinx of quartz.`
- ☑ `Pack my box with five dozen liquor jugs.`
- ☑ `The quick brown fox jumps over the lazy cat.` no match
- ☐ `Hackdaws love my big sphinx of quartz.` no match
- ☑ `Pack my fox with five dozen liquor jugs.` no match

---
```
/(?<=X(?(DEFINE)(A)))X(*F)/
```
(#1248) succeeded 1 times:

- ☑ `AXYZ` no match

---
```
/(?<=X(?(DEFINE)(A)))./
```
(#1249) succeeded 1 times:

- ☑ `AXYZ`

---
```
/(?<=X(?(DEFINE)(.*))Y)./
```
(#1250) succeeded 1 times:

- ☑ `AXYZ`

---
```
/(?<=X(?(DEFINE)(Y))(?1))./
```
(#1251) succeeded 1 times:

- ☑ `AXYZ`

---
```
/(?(DEFINE)(?<foo>bar))(?<![-a-z0-9])word/
```
(#1252) succeeded 1 times:

- ☑ `word`
