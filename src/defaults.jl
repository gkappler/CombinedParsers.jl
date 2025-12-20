export @defaults
export integer_base

const hex_digit = CharIn("[:xdigit:]",'A':'F','a':'f','0':'9')

"""
    integer_base(base,mind=1,maxd=Repeat_max)

Parser matching a integer format on base `base`.

!!! note
    Uses a second Base.parse call on match.
    
    A custom parser could aggregate result incrementally while matching.
"""
function integer_base(base=10,mind=1,maxd=Repeat_max)
    dig = if base == 16
        hex_digit
    elseif base <= 10
        ValueIn('0':('0'+(base-1)))
    else
        error("Base $base not supported")
    end
    with_name(Symbol("integer_base$base"),map(!Repeat(mind:maxd,dig)) do v
                  s = convert(String,v)
                  s == "" ? 0 : parse(Int,s,base=base)::Int
              end)
end


_integer(maxchar=3) =
    with_name(:integer,
              Sequence(Optional('-'),integer_base(10,1,maxchar)) do v
                  if v[1]===missing
                      v[2]
                  else
                      -v[2]
                  end
              end)

integer() = _integer(Repeat_max)

export trim, @trimmed

"""
    trim(p...; whitespace=horizontal_space_maybe, 
               left=whitespace, right=whitespace)

Ignore whitespace `left` and `right` of `sSequence(p...)`.
"""
trim(p...; whitespace=horizontal_space_maybe, left=whitespace, right=whitespace) =
    Sequence(left, sSequence(p...), right)[2]

export padded
padded(p; start=AtStart(), stop=AtEnd()) =
    trim(p, left=start, right=stop)


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
├─ (?>[\\h]*) ValueIn |> Repeat |> ! |> Atomic
├─ . AnyValue |> with_name(:foo)
└─ (?>[\\h]*) ValueIn |> Repeat |> ! |> Atomic
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




char_label_table(x::Union{ValueIn,ValueNotIn}) =
    char_label_table(x.sets)
function char_label_table(x)
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

const whitespace_char = CharIn(
    "[:space:]",
    " \t\U0085\U200E\U200F\U2028\U2029"*"\U2029\U000C\U000B")

const whitespace_maybe = !Atomic(Repeat(whitespace_char))
const whitespace = !Atomic(Repeat1(whitespace_char))

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


const horizontal_space_char = CharIn(
    "\\h",
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

const horizontal_space_maybe = with_name(
    :horizontal_space_maybe,
    !Atomic(Repeat(horizontal_space_char)),
    "\\h*")
const horizontal_space = with_name(
    :horizontal_space,
    !Atomic(Repeat1(horizontal_space_char)),
    "\\h+")


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

const whitespace_horizontal = horizontal_space

const vertical_space_char = CharIn(
    "\\v",
    '\U000A', # "Linefeed (LF)"),
    '\U000B', # "Vertical tab (VT)"),
    '\U000C', # "Form feed (FF)"),
    '\U000D', # "Carriage return (CR)"),
    '\U0085', # "Next line (NEL)"),
    '\U2028', # "Line separator"),
    '\U2029') # "Paragraph separator"))

const vertical_space_maybe = with_name(:vertical_space_maybe,
                                       !Atomic(Repeat(vertical_space_char)),
                                       "\\v*")
const vertical_space = with_name(:vertical_space,
                                 !Atomic(Repeat1(vertical_space_char)),
                                 "\\v+")

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
const space_char = CharIn("\\h\\v",horizontal_space_char,vertical_space_char)

const space_maybe = Atomic(!Repeat(space_char))
const space = Atomic(!Repeat1(CharIn("\\h\\v",space_char)))

"""
    CombinedParsers.newline
    CombinedParsers.Regexp.bsr

newlines, PCRE `\\r` backslash R (BSR).

```jldoctest
julia> CombinedParsers.bsr
\\r with_name(:bsr)
::SubString{String}

julia> CombinedParsers.bsr.parser
(?>|🗄) Either |> Atomic |> !
├─ \\r\\n 
└─ [\\n\\x0b\\f\\r\\x85] ValueIn
::SubString{String}
```

"""
const bsr = with_name(
    :bsr, !Atomic(Either("\r\n",
                         CharIn(raw"\n\x0b\f\r\x85", '\n','\x0b','\f','\r','\U0085', '\U2028','\U2029'))),
    "\\r");

const newline = bsr

const alpha = CharIn('a':'z','A':'Z')
const alphanum = CharIn('a':'z','A':'Z','0':'9')

"Equivalent PRCE `\\w`: Char with unicode class `L`, `N`, or `_`."
const word_char = CharIn("\\w",UnicodeClass("L","N"),'_')

"SubString of at least 1 repeated [`CombinedParsers.word_char`](@ref)."
const word = with_name(:word, !Repeat1(word_char), ## "[[:alpha:] ]+"
                       "\\w+")

"Vector of at least 1 repeated [`CombinedParsers.word`](@ref)s delimited by [`CombinedParsers.whitespace_horizontal`](@ref)."
const words = join(word, whitespace_horizontal) ## "[[:alpha:] ]+"

const non_word_char = CharNotIn("\\W",UnicodeClass("L","N"),'_')
const non_word = with_name(:non_word, !Repeat1(non_word_char),
                           "\\W+")## "[[:alpha:] ]+"

"""
    beyond_word = Either(non_word_char,AtStart,AtEnd)

Parser part of `word_boundary`.
"""
const beyond_word = Either(non_word_char,AtStart(),AtEnd())

"""
    word_boundary = re"\b"
"""
const word_boundary = Either(
    Sequence(PositiveLookbehind(word_char),PositiveLookahead(beyond_word)),
    Sequence(PositiveLookbehind(beyond_word),PositiveLookahead(word_char))
)

"""
    at_linestart

```jldoctest
julia> CombinedParsers.at_linestart
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
const at_linestart = Either(AtStart(),PositiveLookbehind(bsr))


"""
    at_lineend

```jldoctest
julia> CombinedParsers.at_lineend
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
const at_lineend = Either(AtEnd(),PositiveLookahead(bsr))
#@deprecate lineend at_lineend

"""
    inline = !Atomic(Repeat(NegativeLookahead(at_lineend)*AnyChar))

See [`at_lineend`](@ref).
"""
const inline = !Atomic(Repeat(NegativeLookahead(at_lineend)*AnyChar()))



