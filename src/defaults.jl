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

char_label_table(x::Union{CharIn,CharNotIn}) =
    char_label_table(x.sets)
function char_label_table(x)
    chars = sort(collect(x))
    labels = [ get(char_label, c, "") for c in chars ]
    n = maximum(length.(labels))
    println("| Unicode | ", lpad("",n), " |")
    println("|---------|-", "-"^n,      "-|")
    for (c,l) in zip(chars, labels)
        print("|  ")
        show(convert(UInt16,c))
        println(" | ", lpad(l,n), " |")
    end
end

whitespace_char = CharIn(
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
| Unicode |                     |
|---------|---------------------|
|  0x0009 | Horizontal tab (HT) |
|  0x000b |   Vertical tab (VT) |
|  0x000c |      Form feed (FF) |
|  0x0020 |               Space |
|  0x0085 |     Next line (NEL) |
|  0x200e |  Left-to-right mark |
|  0x200f |  Right-to-left mark |
|  0x2028 |      Line separator |
|  0x2029 | Paragraph separator |
```
"""
whitespace_char, whitespace_maybe, whitespace


horizontal_space_char=CharIn("\\h",
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
| Unicode |                           |
|---------|---------------------------|
|  0x0009 |       Horizontal tab (HT) |
|  0x0020 |                     Space |
|  0x00a0 |           Non-break space |
|  0x1680 |          Ogham space mark |
|  0x180e | Mongolian vowel separator |
|  0x2000 |                   En quad |
|  0x2001 |                   Em quad |
|  0x2002 |                  En space |
|  0x2003 |                  Em space |
|  0x2004 |        Three-per-em space |
|  0x2005 |         Four-per-em space |
|  0x2006 |          Six-per-em space |
|  0x2007 |              Figure space |
|  0x2008 |         Punctuation space |
|  0x2009 |                Thin space |
|  0x200a |                Hair space |
|  0x202f |     Narrow no-break space |
|  0x205f | Medium mathematical space |
|  0x3000 |         Ideographic space |
```
"""
horizontal_space_char, horizontal_space_maybe, horizontal_space


"""
    trim(; whitespace=CharIn(' '))

Match any whitespace and result in `tuple()`.
"""
trim(; whitespace=horizontal_space) =
    map(whitespace) do v
        tuple()
    end

"""
    trim(p; whitespace=CharIn(' '))

Ignore whitespace at left and right of `p`.
"""
trim(p; whitespace=Atomic(Repeat(horizontal_space_char))) =
    Sequence(whitespace, p, whitespace)[2]


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
🗄 Sequence |> map(#55)
├─ (?>[\\h]*) CharIn |> Repeat |> Atomic
├─ . AnyChar |> with_name(:foo)
└─ (?>[\\h]*) CharIn |> Repeat |> Atomic
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

vertical_space_char=CharIn("\\v",
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
| Unicode |                      |
|---------|----------------------|
|  0x000a |        Linefeed (LF) |
|  0x000b |    Vertical tab (VT) |
|  0x000c |       Form feed (FF) |
|  0x000d | Carriage return (CR) |
|  0x0085 |      Next line (NEL) |
|  0x2028 |       Line separator |
|  0x2029 |  Paragraph separator |
```
"""
vertical_space_char, vertical_space_maybe, vertical_space


"Equivalent PRCE `\\h\\v`, [`horizontal_space_char`](@ref), [`vertical_space_char`](@ref)"
space_char  = CharIn("\\h\\v",horizontal_space_char,vertical_space_char)
space_maybe = Atomic(!Repeat(space_char))
space = Atomic(!Repeat1(CharIn("\\h\\v",space_char)))

@deprecate whitespace_newline space


"""
    CombinedParsers.newline
    CombinedParsers.Regexp.bsr

newlines, PCRE `\\r` backslash R (BSR).

```jldoctest
julia> CombinedParsers.Regexp.bsr
(?>|🗄) Either |> Atomic |> with_name(:bsr)
├─ \\r\\n 
└─ [\\n\\x0b\\f\\r\\x85] CharIn |> !
::SubString{String}
```

"""
@with_names bsr = Atomic(Either("\r\n",
                                !CharIn(raw"\n\x0b\f\r\x85", '\n','\x0b','\f','\r','\U0085', '\U2028','\U2029')));

newline = bsr

"`SubString` of repeated non [`vertical_space_char`](@ref)s."
inline = !Repeat(CharNotIn(vertical_space_char))

"Equivalent PRCE `\\w`: Char with unicode class `L`, `N`, or `_`."
word_char=CharIn("\\w",UnicodeClass("L","N"),'_')

"SubString of at leat 1 repeated [`word_char`](@ref)."
word = JoinSubstring(Repeat1(word_char)) ## "[[:alpha:] ]+"

"SubString of at leat 1 repeated [`word_char`](@ref) or [`horizontal_space_char`](@ref)."
words = JoinSubstring(Repeat1(CharIn("\\w\\h", horizontal_space_char, word_char))) ## "[[:alpha:] ]+"

non_word_char=CharNotIn("\\W",UnicodeClass("L","N"),'_')
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
   └─ [\\n\\x0b\\f\\r\\x85] CharIn |> !
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
   └─ [\\n\\x0b\\f\\r\\x85] CharIn |> !
::Union{AtEnd, SubString{String}}
```

!!! note
    used in `re"\$"` if `Base.PCRE.MULTILINE` is set.
"""
@with_names at_lineend   = Either(AtEnd(),PositiveLookahead(bsr))


