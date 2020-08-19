# # What is Parsing?

# When you read texts, you understand what the words mean.
# Your smart brain analyzes the series of letter symbols.
# Like you move your muscles without being aware of every detail, intuitively,
# your brain understands the meaning of text after assigning syntax to each word.
#
# This post explains how a computer can read a text (parsing) with 
# a simple example, reading a name in a letter, a list of students, or citizens.
# The post also explains how to program a [`CombinedParser`](@ref) in julia using [`@syntax`](@ref).
#
# Let's get to it:
# These examples represent how a name should be parsed into a Julia `NamedTuple` representation.
# You can write a program to parse such names and addresses
using CombinedParsers
using CombinedParsers.Regexp
# ## Names
# can be written in many ways.
# The example uses parser building blocks:
import CombinedParsers.Regexp: word, words
import CombinedParsers.Regexp: whitespace_horizontal, whitespace_maybe, newline
# A name can be written in terms of these building blocks, 
@syntax name = 
    Either( 
        Sequence(
            :lastname => word,
            re" *, *", # comma separating preceding lastname
            :name     => words),
        Sequence(
            v -> (lastname = v[3], name= v[1]),
            words, *(" "), word)
    )

# `CombinedParsers.@syntax` defines a string macro `name""` for using the syntax parser in your Julia program:
# See also [`@syntax`](@ref)
name"Person, Called"

# `CombinedParsers.@syntax` also defines a syntax parser function `name()` to use in your Julia program:
(re"Best, *" * name)("Best, yours truly")


# ## Addresses
# On a letter, you see the
@syntax for street_address in texts
    examples = "Am Hang 19" => (street="Am Hang", no=19)
    Sequence(:street => !!Repeat(AnyChar()),
             " ",
             :no =>Numeric(Int))
end;

# In programming, regular expressions are the standard language for matching sequences of letters, a `String`.
# The street is a sequence of alphabetical symbols or space (not numbers or `AnyChar()`)
# can be written concisely as `re"Regex"`:
@syntax for street_address in texts ## todo: override in Either
    Sequence(:street => !!re"[[:alpha:] ]+", whitespace_horizontal, :no =>Numeric(Int))
end;

street_address"Allee 47"


# Defined `@syntax`'s can be combined.
whitenewline = Repeat1(Either(whitespace_horizontal,newline));
@syntax for address in texts
    Sequence(
        Optional("Adresse:",whitenewline),
        :door => street_address, whitenewline,
        :zip =>Numeric(Int), whitespace_horizontal,
        :city => !re"[[:alpha:] ]+",
        whitespace_maybe
    )
end

# A `@syntax` can be applied as a julia String macro
address"""Allee 47
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

