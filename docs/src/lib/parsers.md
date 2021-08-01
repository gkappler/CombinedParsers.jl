# Parser Templates

## Constant and `TextParse` Conversion
Parsing Numbers or Dates is most efficiently done with TextParse.

```@docs
parser
convert
```

For non base 10 numbers, use
```@docs
CombinedParsers.integer_base
```
	

## Parser Building Blocks
PCRE regular expressions provides established building blocks as escape sequences.
Equivalent `CombinedParser`s are provided by name.
!!! note 
    You can also use PCRE [regex](regexp.md) syntax with the `@re_str` 
    to build identical `CombinedParser`s!

## Predefined Parsers
### Horizontal and Vertical Space

#### Trimming space
```@docs
CombinedParsers.trim
CombinedParsers.@trimmed
```

#### Matching Space
```@docs
CombinedParsers.whitespace_char
CombinedParsers.horizontal_space_char
CombinedParsers.vertical_space_char
CombinedParsers.Regexp.bsr
```

#### Words
```@docs
CombinedParsers.caseless
CombinedParsers.word
CombinedParsers.words
CombinedParsers.inline
CombinedParsers.word_char
CombinedParsers.word_boundary
CombinedParsers.beyond_word
```

## Predefined Assertions
```@docs
CombinedParsers.at_linestart
CombinedParsers.at_lineend
```

