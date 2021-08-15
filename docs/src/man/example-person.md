# Names and Adresses
can be written in different ways, e.g.
1. with the name preceding the surname, or
2. with the surname preceding a comma and the name.


## Reading, technically, is Parsing
When humany read names, we identify different writings with the same name of course.
Our smart brains analyzes the series of letter symbols
like we move muscles without being aware of every detail, intuitively.
Reading texts, we understand what the words mean.

```@meta
DocTestSetup = quote
	using CombinedParsers
	using CombinedParsers.Regexp
	import CombinedParsers: whitespace_horizontal, whitespace_maybe, newline
	import CombinedParsers: word, words

    @syntax name = Either(Sequence(:lastname => word,  # preceding lastname
    			              trim(","),          # comma separating
    				      :name     => !words),
    			     Sequence(!words, whitespace_horizontal, word) do v
                                ( lastname = v[3], name= v[1] )
                             end
    		             );
    @syntax street_adress =
               Sequence(
                   :street => !words, whitespace_horizontal,
                   :no     => Numeric(Int));
    @syntax adress =
                Sequence(
                    Optional(trim("Adresse:", whitespace=space_maybe)),
                    :door => trim(street_adress, whitespace=space_maybe),
                    :zip  => Numeric(Int),
                    :city => trim(!words, whitespace=space_maybe))
					
    @syntax person_adresses = Sequence(
                    Optional(trim("Name:", whitespace=space_maybe)),
                    :person => name, space_maybe,
                    :adresses => join(adress, space_maybe));
end
```

This post explains how a computer can read (parse) names 
with [`CombinedParser`](@ref) in julia [`@syntax`](@ref).

A name can be written in terms of these building blocks,
```julia
using CombinedParsers
import CombinedParsers: word, words
@syntax name = Either(
            Sequence(
                 :lastname => word,
    		              trim(","),  # space-padded comma
        		 :name     => !words),
            Sequence(!words, whitespace_horizontal, word) do v
                 # anonymous function to reverse NamedTuple order!
                 ( lastname = v[3], name= v[1] )
            end);
```

The constructors aim to be read naturally: a `name` is 
- [`Either`](@ref) any of two options, each is a
- [`Sequence`](@ref) that picks
- properties [`with_name`](@ref) (provided as `Pair{Symbol, CombinedParser}`s),
- of the[`MatchedSubString`](@ref) [`(!)`](@ref) of [`CombinedParser.words`](@ref) (or a single [`CombinedParser.word`](@ref)s), 
- seperated by [`whitespace_horizontal`](@ref) that can be [`trim`](@ref)med (more flexibly than a lexer),
- and the anonymous function in the second option is transforming a parsing result with [`map`](@ref).

The `CombinedParser` `name` is callable to read/parse names:
```@jldoctest
julia> name("Name Surname")
(lastname = "Surname", name = "Name")
```

Using [`@syntax`](@ref) also provides a string macro for parsing:
```@jldoctest
julia> name"Yours, Truely"
(lastname = "Surname", name = "Name")
```

### Julia `NamedTuple`s are a great language feature! 
The example converts (parses) a matching string to a
```@jldoctest
julia> result_type(name)
(lastname = "Surname", name = "Name")
```
a type without a dedicated `struct` name but with field names.
(For writing concisely, anonymous `struct` representations are as useful as anonymous functions.
Why?  Omitting to name things works around solving a hard problem.)

## Adresses
On a letter, the
```julia
@syntax street_adress =
               Sequence(
                   :street => !words, whitespace_horizontal,
                   :no     => Numeric(Int));
```

Defined parsers can be combined.
(And composed with `TextParse.`[`Numeric`](@ref), PCRE regular expression and Backus-Naur-Form syntax.
Any other parser can be plugged in by writing internal `CombinedParser` functions.)

```julia
@syntax adress =
                Sequence(
                    Optional(trim("Adresse:", whitespace=space_maybe)),
                    :door => trim(street_adress, whitespace=space_maybe),
                    :zip  => Numeric(Int),
                    :city => trim(!words, whitespace=space_maybe))
```


```jldoctest
julia> adress"""Allee 47
       80000 Augsburg
       """
(door = (street = "Allee", no = 47), zip = 80000, city = "Augsburg")
```

## Person's adress data
For person entries in texts a `CombinedParser` can be intuitively written as

```julia
@syntax person_adresses = Sequence(
                    Optional(trim("Name:", whitespace=space_maybe)),
                    :person => name, space_maybe,
                    :adresses => join(adress, space_maybe));
```


Let's test that parser with a
```jldoctest
julia> function summary(p::result_type(person_adresses)) 
            println(p.person.name, " ", p.person.lastname)
            for a in p.adresses; println("   ", a.city); end
       end;

julia> person_adresses"""
       Name: Gottfried Wirklich
       
       Adresse:
       Abhang 19
       86653 Glauberg
       
       Adresse:
       Allee 47
       80650 Pinienstadt
       """ |> summary
Gottfried Wirklich
   Glauberg
   Pinienstadt
```

This example demonstrated `CombinedParser.jl` for creating `NamedTuple` representation from `String`.
