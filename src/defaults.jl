export whitespace_char, whitespace_maybe, whitespace
export horizontal_space_char, horizontal_space_maybe, horizontal_space
export vertical_space_char, vertical_space_maybe, vertical_space
export space, space_maybe, whitespace_newline
export trim, @trimmed
export newline, inline
export at_linestart, at_lineend

char_label = Dict(
    '\U0009' => "Horizontal tab (HT)",
    '\U0020' => "Space",
    '\U00A0' => "Non-break space",
    '\U1680' => "Ogham space mark",
    '\U180E' => "Mongolian vowel separator",
    '\U2000' => "En quad",
    '\U2001' => "Em quad",
    '\U2002' => "En space",
    '\U2003' => "Em space",
    '\U2004' => "Three-per-em space",
    '\U2005' => "Four-per-em space",
    '\U2006' => "Six-per-em space",
    '\U2007' => "Figure space",
    '\U2008' => "Punctuation space",
    '\U2009' => "Thin space",
    '\U200A' => "Hair space",
    '\U202F' => "Narrow no-break space",
    '\U205F' => "Medium mathematical space",
    '\U3000' => "Ideographic space",
    '\u2028' => "Line Separator",
    '\u200e' => "Left-to-right mark",
    '\v' => "Vertical tab",
    '\u2029' => "Paragraph separator",
    '\u85' => "Next line",
    '\u200f' => "Right-to-left mark",
    '\f' => "Form feed",
    '\U000A' => "Linefeed (LF)",
    '\U000B' => "Vertical tab (VT)",
    '\U000C' => "Form feed (FF)",
    '\U000D' => "Carriage return (CR)",
    '\U0085' => "Next line (NEL)",
    '\U2028' => "Line separator",
    '\U2029' => "Paragraph separator"
)

char_label_table(x::Union{ValueIn,ValueNotIn}) =
    char_label_table(x.sets)
function char_label_table(x)
    chars = sort(collect(x))
    rs = [ escape_string(repr(c)) for c in chars ]
    labels = [ get(char_label, c, "") for c in chars ]
    nr = maximum(length.(rs))
    n = maximum(length.(labels))
    println("| ",lpad("Char",nr)," | ", lpad("",n), " |")
    println("|-","-"^nr, "-|-", "-"^n,      "-|")
    for (c,l) in zip(rs, labels)
        print("| ")
        print(lpad(c,nr))
        println(" | ", lpad(l,n), " |")
    end
end

whitespace_char = ValueIn(
    "[:space:]",
    " \t\U0085\U200E\U200F\U2028\U2029"*"\U2029\U000C\U000B")

whitespace_maybe = !Atomic(Repeat(whitespace_char))
whitespace = !Atomic(Repeat1(whitespace_char))


"""
    whitespace_char  = re"[[:space:]]"
    whitespace_maybe = re"(?>[[:space:]]*)"
    whitespace       = re"(?>[[:space:]]+)"

```jldoc
julia> CombinedParsers.char_label_table(whitespace_char)
|      Char |                     |
|-----------|---------------------|
|     '\\t' | Horizontal tab (HT) |
|     '\\v' |   Vertical tab (VT) |
|     '\\f' |      Form feed (FF) |
|       ' ' |               Space |
|   '\\u85' |     Next line (NEL) |
| '\\u200e' |  Left-to-right mark |
| '\\u200f' |  Right-to-left mark |
| '\\u2028' |      Line separator |
| '\\u2029' | Paragraph separator |
```
"""
whitespace_char, whitespace_maybe, whitespace


horizontal_space_char=ValueIn("\\h",
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

horizontal_space_maybe = Atomic(!Repeat(horizontal_space_char))
horizontal_space = Atomic(!Repeat1(horizontal_space_char))



"""
    horizontal_space_char  = re"[\\h]"
    horizontal_space_maybe = re"(?>[\\h]*)"
    horizontal_space       = re"(?>[\\h]+)"

```jldoc
julia> CombinedParsers.char_label_table(horizontal_space_char)
|      Char |                           |
|-----------|---------------------------|
|     '\\t' |       Horizontal tab (HT) |
|       ' ' |                     Space |
|       ' ' |           Non-break space |
|       ' ' |          Ogham space mark |
| '\\u180e' | Mongolian vowel separator |
|       ' ' |                   En quad |
|       ' ' |                   Em quad |
|       ' ' |                  En space |
|       ' ' |                  Em space |
|       ' ' |        Three-per-em space |
|       ' ' |         Four-per-em space |
|       ' ' |          Six-per-em space |
|       ' ' |              Figure space |
|       ' ' |         Punctuation space |
|       ' ' |                Thin space |
|       ' ' |                Hair space |
|       ' ' |     Narrow no-break space |
|       ' ' | Medium mathematical space |
|       '　' |         Ideographic space |
```
"""
horizontal_space_char, horizontal_space_maybe, horizontal_space

whitespace_horizontal = horizontal_space

"""
    trim(p...; left=horizontal_space_maybe, right=horizontal_space_maybe)

Ignore whitespace `left` and `right` of `sSequence(p...)`.
"""
trim(p...; left=horizontal_space_maybe, right=horizontal_space_maybe) =
    Sequence(left, sSequence(p...), right)[2]



trimmed(x) = x
function trimmed(node::Expr)
    if node.head == :(=) && length(node.args) == 2 && isa(node.args[1], Symbol)
        node.args[2] = Expr(:call, :trim, Expr(:call, :with_name, QuoteNode(node.args[1]), node.args[2]))
    end
    if node.head != :call 
        node.args = map(trimmed, node.args)
    end
    node
end


"""
    @trimmed

Create parser within `whitespace_maybe` to match the variables they are asigned to.

See also [`trim`](@ref).

```@meta
DocTestFilters = r"map\\(.+\\)"
```

so, for example
```jldoctest
julia> @trimmed foo = AnyChar()
🗄 Sequence[2]
├─ (?>[\\h]*) ValueIn |> Repeat |> Atomic
├─ . AnyValue |> with_name(:foo)
└─ (?>[\\h]*) ValueIn |> Repeat |> Atomic
::Char

julia> parse(log_names(foo),"  ab  ")
   match foo@3-4:   ab
                    ^
'a': ASCII/Unicode U+0061 (category Ll: Letter, lowercase)
```

"""
macro trimmed(block)
    esc(trimmed(block))
end

vertical_space_char=ValueIn("\\v",
    '\U000A', # "Linefeed (LF)"),
    '\U000B', # "Vertical tab (VT)"),
    '\U000C', # "Form feed (FF)"),
    '\U000D', # "Carriage return (CR)"),
    '\U0085', # "Next line (NEL)"),
    '\U2028', # "Line separator"),
    '\U2029') # "Paragraph separator"))

vertical_space_maybe = Atomic(!Repeat(vertical_space_char))
vertical_space = Atomic(!Repeat1(vertical_space_char))

"""
    vertical_space_char  = re"[\\v]"
    vertical_space_maybe = re"(?>[\\v]*)"
    vertical_space       = re"(?>[\\v]+)"

```jldoc
julia> CombinedParsers.char_label_table(vertical_space_char)
|      Char |                      |
|-----------|----------------------|
|     '\\n' |        Linefeed (LF) |
|     '\\v' |    Vertical tab (VT) |
|     '\\f' |       Form feed (FF) |
|     '\\r' | Carriage return (CR) |
|   '\\u85' |      Next line (NEL) |
| '\\u2028' |       Line separator |
| '\\u2029' |  Paragraph separator |
```
"""
vertical_space_char, vertical_space_maybe, vertical_space


"Equivalent PRCE `\\h\\v`, [`horizontal_space_char`](@ref), [`vertical_space_char`](@ref)"
space_char  = ValueIn("\\h\\v",horizontal_space_char,vertical_space_char)
space_maybe = Atomic(!Repeat(space_char))
space = Atomic(!Repeat1(ValueIn("\\h\\v",space_char)))


@deprecate whitespace_newline space

"""
    CombinedParsers.newline
    CombinedParsers.Regexp.bsr

newlines, PCRE `\\r` backslash R (BSR).

```jldoctest
julia> CombinedParsers.Regexp.bsr
(?>|🗄) Either |> Atomic |> with_name(:bsr)
├─ \\r\\n 
└─ [\\n\\x0b\\f\\r\\x85] ValueIn |> !
::SubString{String}
```

"""
@with_names bsr = Atomic(Either("\r\n",
                                !ValueIn(raw"\n\x0b\f\r\x85", '\n','\x0b','\f','\r','\U0085', '\U2028','\U2029')));

newline = bsr

"Equivalent PRCE `\\w`: Char with unicode class `L`, `N`, or `_`."
word_char=ValueIn("\\w",UnicodeClass("L","N"),'_')

"SubString of at leat 1 repeated [`word_char`](@ref)."
word = JoinSubstring(Repeat1(word_char)) ## "[[:alpha:] ]+"

"SubString of at leat 1 repeated [`word_char`](@ref) or [`horizontal_space_char`](@ref)."
words = JoinSubstring(Repeat1(ValueIn("\\w\\h", horizontal_space_char, word_char))) ## "[[:alpha:] ]+"

non_word_char=ValueNotIn("\\W",UnicodeClass("L","N"),'_')
non_word = JoinSubstring(Repeat1(non_word_char)) ## "[[:alpha:] ]+"

"""
    beyond_word = Either(non_word_char,AtStart(),AtEnd())

Parser part of `word_boundary`.
"""
beyond_word = Either(non_word_char,AtStart(),AtEnd())

"""
    word_boundary = re"\b"
"""
@with_names word_boundary = Either(
    Sequence(PositiveLookbehind(word_char),PositiveLookahead(beyond_word)),
    Sequence(PositiveLookbehind(beyond_word),PositiveLookahead(word_char))
)

"""
    at_linestart

```jldoctest
julia> CombinedParsers.Regexp.at_linestart
|🗄 Either |> with_name(:at_linestart)
├─ ^ AtStart
└─ (?<=🗄)) Either |> Atomic |> with_name(:bsr) |> PositiveLookbehind
   ├─ \\r\\n
   └─ [\\n\\x0b\\f\\r\\x85] ValueIn |> !
::Union{AtStart, SubString}
```

!!! note
    used in `re"^"` if `Base.PCRE.MULTILINE` is set.
"""
@with_names at_linestart = Either(AtStart(),PositiveLookbehind(bsr))
# lineend   = Either(AtEnd(),bsr)


"""
    at_lineend

```jldoctest
julia> CombinedParsers.Regexp.at_lineend
|🗄 Either |> with_name(:at_lineend)
├─ \$ AtEnd
└─ (?=(?>|🗄)) Either |> Atomic |> with_name(:bsr) |> PositiveLookahead
   ├─ \\r\\n 
   └─ [\\n\\x0b\\f\\r\\x85] ValueIn |> !
::Union{AtEnd, SubString{String}}
```

!!! note
    used in `re"\$"` if `Base.PCRE.MULTILINE` is set.
"""
@with_names at_lineend   = Either(AtEnd(),PositiveLookahead(bsr))


