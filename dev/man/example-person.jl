# # What is Parsing?

# When you read texts, you understand what the words mean.
# Your smart brain analyzes the series of letter symbols.
# Like you move your muscles without being aware of every detail, intuitively,
# your brain understands the meaning of text after assigning syntax to each word.
#
# This post explains how a computer can read a text (parsing) with 
# a simple example, reading a name in a letter, a list of students, or citizens.
# The post also explains how to program a parser in julia.
#
# Let's get to it:
# These examples represent how a name should be parsed into a Julia `NamedTuple` representation.
using CombinedParsers
using CombinedParsers.Regexp
# you can write a program to parse such names and addresses.
# The example uses parser building blocks:
import CombinedParsers.Regexp: word, words, whitespace, whitespace_maybe, newline
import CombinedParsers.Regexp: whitespace, whitespace_maybe, newline

# ## Names
# can be written in many ways.
# The way a name is written tn terms of these building blocks, is a
@syntax for name in texts
    examples = (
        "Lieber Lorenz,"    => (greeting="Lieber", lastname = missing, name="Lorenz"),
        "Namen, Vornamen"   => (greeting=missing, lastname = "Namen", name="Vornamen"),
        "Vornamen    Namen" => (greeting=missing, lastname = "Namen", name="Vornamen")
    )
    Sequence(
        whitespace_maybe,
        !Either(
            "Hi",re"Lieber?", re"Sehr geehrter?",
            "Herzliche Grüße,\n*",
            "" => missing),
        whitespace_maybe,
        Either( 
            Sequence(
                :lastname => word,
                re" *, *", # comma separating preceding lastname
                :name     => words),
            Sequence(
                :name     => words,
                *(" "),     # spaces
                :lastname => word)
        ) ## todo: remove :person, splice NamedTuple
    ) do v
        ( greeting = v[2],
          name = v[4].name,
          lastname = v[4].lastname
          )
    end
end;

# `CombinedParsers.@syntax` defines a string macro for using the syntax parser in your Julia program:
# See also [`@syntax`](@ref)
name"Person, Called"

# `CombinedParsers.@syntax` defines a syntax parser to use in your Julia program:
name("Best,"*whitespace_maybe, "Best, yours truly")


# ## Addresses
# On a letter, you find the
@syntax for street_address in texts
    examples = "Am Hang 19" => (street="Am Hang", no=19)
    Sequence(:street => !Repeat(AnyChar()),
             " ",
             :no =>Numeric(Int))
end

# In programming, regular expressions are the standard language for matching sequences of letters, a `String`.
# The street is a sequence of alphabetical symbols or space (not numbers or `AnyChar()`)
# can be written concisely as `re"Regex"`:
@syntax for street_address in texts ## todo: override in Either
    Sequence(:street => !re"[[:alpha:] ]+", whitespace, :no =>Numeric(Int))
end;



# Defined `@syntax`'s can be combined.
whitenewline = Repeat1(Either(whitespace_maybe,newline));
@syntax for address in texts
    Sequence(
        Optional("Adresse:",whitenewline),
        :door => street_address, whitenewline,
        :zip =>Numeric(Int), whitespace,
        :city => !re"[[:alpha:] ]+",
        whitespace
    )
end;

# A `@syntax` can be applied as a julia String macro
address"""
Allee 47
80000 Augsburg
"""

# ## Person's address data
# For person entries in texts a `CombinedParser` can be intuitively written as
@syntax for person_adresses in texts
    Sequence(Optional("Name:",whitenewline),
             :person => name, Repeat1(whitenewline),
             :adresses => join(address, Repeat1(whitenewline)));
end;

person_adresses"""
Name: Gottfried Mutbürger

Adresse:
Am Hang 19
86653 Glauberg

Adresse: 
Allee 47
80650 Zweistadt
"""


# This example demonstrated the terms "syntax" and "parsing".
# Here, a syntax defines the ways in which information is written.
# Parsing means the computer reads the information back from the way it was written:
# The `CombinedParser.jl` definitions create a program that parses a formal `NamedTuple` representation from texts written in that way.

