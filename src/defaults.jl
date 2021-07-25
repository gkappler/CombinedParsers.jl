export whitespace_char
whitespace_string = " \t\U0085\U200E\U200F\U2028\U2029"*"\U2029\U000C\U000B"
whitespace_char = CharIn("[:space:]",whitespace_string)

export horizontal_space_char
# The horizontal space characters are:
horizontal_space_char=CharIn(
    '\U0009', # "Horizontal tab (HT)"),
    '\U0020', # "Space"),
    '\U00A0', # "Non-break space"),
    '\U1680', # "Ogham space mark"),
    '\U180E', # "Mongolian vowel separator"),
    '\U2000', # "En quad"),
    '\U2001', # "Em quad"),
    '\U2002', # "En space"),
    '\U2003', # "Em space"),
    '\U2004', # "Three-per-em space"),
    '\U2005', # "Four-per-em space"),
    '\U2006', # "Six-per-em space"),
    '\U2007', # "Figure space"),
    '\U2008', # "Punctuation space"),
    '\U2009', # "Thin space"),
    '\U200A', # "Hair space"),
    '\U202F', # "Narrow no-break space"),
    '\U205F', # "Medium mathematical space"),
    '\U3000' # "Ideographic space"))
)

export vertical_space_char
# The vertical space characters are:
vertical_space_char=CharIn(
    '\U000A', # "Linefeed (LF)"),
    '\U000B', # "Vertical tab (VT)"),
    '\U000C', # "Form feed (FF)"),
    '\U000D', # "Carriage return (CR)"),
    '\U0085', # "Next line (NEL)"),
    '\U2028', # "Line separator"),
    '\U2029') # "Paragraph separator"))

export bsr
"""
```jldoctest
julia> CombinedParsers.Regexp.bsr
(?>|🗄...) Either |> Atomic
├─ \r\n 
└─ [\n\x0b\f\r\x85] CharIn
::Union{Char, SubString}
```

PCRE backslash R (BSR), for newlines.
"""
bsr = Atomic(Either("\r\n",
                    !CharIn(raw"\n\x0b\f\r\x85", '\n','\x0b','\f','\r','\U0085', '\U2028','\U2029')));

export at_linestart, at_lineend
"""
```jldoctest
julia> CombinedParsers.Regexp.at_linestart
|🗄... Either
├─ ^ AtStart
└─ (?<=🗄...)) PositiveLookbehind
   ├─ \n\r 
   └─ [\n\x0b\f\r\x85] CharIn
::Union{AtStart, Char, SubString}
```

Note: in PCRE
```julia
Either(
    on_options(Base.PCRE.MULTILINE, 
           '^' => at_linestart),
    parser('^' => AtStart())
)
```
"""
@with_names at_linestart = Either(AtStart(),PositiveLookbehind(bsr))
@with_names at_lineend   = Either(AtEnd(),PositiveLookahead(bsr))

export newline, inline, whitespace_maybe, whitespace
newline = bsr
inline = !Repeat(CharNotIn(vertical_space_char))
whitespace_maybe = !Repeat(CharIn("\\h",horizontal_space_char))
whitespace_horizontal = !Repeat1(CharIn("\\h",horizontal_space_char))
whitespace = whitespace_horizontal

export whitespace_newline
whitespace_newline = Repeat1(CharIn("\\h\\v",horizontal_space_char,vertical_space_char))
