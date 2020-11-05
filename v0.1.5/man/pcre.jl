# # Regular Expressions [`CombinedParser`](@ref)
# Parsing is reading by a computer program,
# formally transforming character sequences 
# into defined and structured representations.
#
# -- name parser slide
#
# Representation and Transformation
#
# -- number list slide
#
# So parsing is a fundamental
# obstacle before you can begin working with data.
# Writing parsers is ubiquitous.
#
# ## What is a regular expression?
# Regular expressions and state machines are the most common tools of the parsing trade.
# Most programmers and data scientists are familiar with regular expressions.
#
# A regular expression is a textual representation of a parser program.
# The syntax of regular expressions is condensed and arcane.
#
# Parsing has been a painpoint in all of my work as a developer and a researcher with a focus on text
# while I accumulated plenty of regular expressions.
# I still use these regexps at many places and do not want to rewrite all regular expressions I still need.
# So I wrote a CombinedParser that parses regex, and transforms it into an equivalent julia parser program, 
using CombinedParsers

# This guide demonstrates constructing a recursive `CombinedParser` with the **[`@syntax`](@ref) or [`push!`](@ref) to [`Either`](@ref)** technique.
# This page outlines essentials of the [`@re_str`](@ref) macro 
using CombinedParsers.Regexp


# ## Characters and Escaped Characters
# The simplest regular expression pattern is a character matcher.
import CombinedParsers.Regexp: meta_chars
char_matcher =  Either(
    character = CharNotIn(meta_chars),
    escaped_meta =  Sequence(
	2,    # transform: emit unescaped at index
        '\\', CharIn(meta_chars)
    )
)

# The `char_matcher` unescapes regex `meta_chars`:
parse(Repeat(char_matcher),raw"a\[b"; log=true)

# ## Repeated patterns
# ### Number of Repetitions: Transforming a Parsing
# A basic feature of regular expression is whether a repeatable pattern (e.g. a char) is optional `?` (repeated 0 or 1 times), or should be repeated at least once `+`, any times `*`, or `{min,max}` times ([pcre spec](https://www.pcre.org/original/doc/html/pcrepattern.html#SEC17)).
# The syntax of regex repetitions require mapping such `SubString`s to representions as `UnitRange{Int}`.
# 
#
# Above, the `:escaped_meta` mapped the parsing to emit the unescaped character at the second position in its sequence.
# Any Julia function can be used for transformations with the `do` syntax after a `CombinedParser` constructor to implicitly call [`map`](@ref) creating a [`Transformation`](@ref) parser.

import CombinedParsers.Regexp: integer, Repeat_max
@with_names repetitions = Either(
    '?' => 0:1, 
    '+' => 1:Repeat_max,
    '*' => 0:Repeat_max,
    Sequence(
        '{', 
        :min => integer,
        :max => Optional(
	    Sequence(2,
		     ',', integer | Repeat_max)),
        '}'
    ) do v
    if v.max isa Missing
    v.min:v.min
    else
    v.min:v.max
    end::UnitRange{Int}
    end
)

# `repetitions` parses PCRE repetitions syntax into a `(min, max)`:
parse(repetitions, "?")
parse(repetitions, "*")
parse(repetitions, "+")
parse(repetitions, "{2}")
parse(repetitions, "{5,}")

# ## [`Either`](@ref)
# Regular expressions can repeat character matchers and other sub-patterns when appending the `repetitions` suffix.
repeatable = Either{CombinedParser}(
    map(CharIn,char_matcher),
    '.' => AnyChar()
)

# We will add capture groups and other sub-patterns to `repeatable` later.




# ## Repeatable patterns, [`Optional`](@ref), [`Lazy`](@ref) and [`Atomic`](@ref)
# The CombinedParser to parse a repeated pattern from PCRE syntax
@with_names quantified=Sequence(
        repeatable,
        Optional(repetitions, default=(1,1)),
        Optional(map(v->convert(Char,v),with_name("possessive_quantifier",CharIn('+','?'))))
    ) do v
        pat = v[1]
        result = if v[2]==(1,1)
            pat
        elseif v[2]==(0,1)
            Optional(pat)
        else
            Repeat(v[2],pat)
        end
        if v[3] === missing
            result
        elseif v[3]=='+'
            Atomic(result)
        elseif v[3]=='?'
            Lazy(result)
        else
            result
        end
    end

# PCRE repetitions syntax can be `quantified`:
p = quantified(raw"\++?", log=true)
parse(p, "+++")



# ## [`Sequence`](@ref)s and Alternations
# Regular expression patterns can be written in sequence, delimited by `|` for matching either one of several alternatives.
subpattern = Either{CombinedParser}(
    '^' => AtStart(),
    '$' => AtEnd(),
    quantified);

@with_names sequence =
    map( v->sSequence(v...),
         Repeat( subpattern ));
p = sequence(raw"\++?$")
parse(p, "+++")


# The alternation parser transforms a regex string into a parser
@with_names pattern = join(sequence, '|') do v
    sEither(v...)::CombinedParser
end;

result_type(pattern)
p = parse(pattern, "ab|c", log=(:sequence, :pattern))
parse(p, "c")

# ## [`Capture`](@ref)s
# Parentheses allow groupings that are repeatable.
import CombinedParsers.Regexp: name

# The [`push!`](@ref) to [`Either`](@ref) technique allows for the construction of recursive parsers.
@with_names capture_group =
    map(Capture, Sequence(2, '(', pattern, ')'));
push!(repeatable,capture_group);

# Conveniently, the [`@syntax`](@ref) with a `for <name> in <either>` calls `push!`.
@syntax for noncapture_group in repeatable
    Sequence(2, '(?:', pattern, ')')
end;

regcomb = map(ParserWithCaptures,pattern);

match(regcomb("(1a(2b?)*)*0"),"1a1a21a2b22b0")
match(CombinedParsers.Regexp.Regcomb("(1a(2b?)*)*0"),"1a1a21a2b22b0")
match(Regex("(1a(2b?)*)*0"),"1a1a21a2b22b0")
pattern("(1a(2b?)*)*0")("1a1a21a2b22b0")


# ## [`Lookahead`](@ref) and [`Lookbehind`](@ref)


@syntax for lookaround in repeatable
    Sequence(
        "(?",
        :direction => Either(
            '<' => Lookbehind,
            Always() => Lookahead),
        :doesmatch => Either(
            '=' => true,
            '!' => false),
        :pattern => pattern,
        ')') do v
            v.direction(v.doesmatch,v.pattern)::CombinedParser
            ## annotation needed for type inference
            ## obfuscated by :direction
        end
end;

match(pattern("a(?=c|d)."),"abad")
match(Regex("a(?=c|d)."),"abad")


# ## Regular Expression Brackets
# The regular expression parser packaged with CombinedParser is widely compliant with the PCRE C library used in Julia `Base.Regex` -- and in more than half the tests actually faster than C
# [see testing and benchmarking against PCRE tests](pcre-compliance.md).
import CombinedParsers.Regexp: bracket, ParserWithCaptures
push!(repeatable,bracket);



# [`@re_str`](@ref) is tested and benchmarked against the PCRE C library testset (see [compliance report](pcre-compliance.md)).


# Syntax is essential, semantics is high-brow.
# What is the semantics of a regular expression?
# The semantics of regular expression is a parser program that reads a context free regular language.
#
