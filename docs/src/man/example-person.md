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
    				      :name     => words),
    			     Sequence(words, whitespace_horizontal, word) do v
                                ( lastname = v[3], name= v[1] )
                             end
    		             );
    @syntax street_address =
               Sequence(
                   :street => !words, whitespace_horizontal,
                   :no     => Numeric(Int));
    @syntax address =
                Sequence(
                    Optional(trim("Adresse:", whitespace=space_maybe)),
                    :door => trim(street_address, whitespace=space_maybe),
                    :zip  => Numeric(Int),
                    :city => trim(!words, whitespace=space_maybe))
					
    @syntax person_adresses = Sequence(
                    Optional(trim("Name:", whitespace=space_maybe)),
                    :person => name, space_maybe,
                    :adresses => join(address, space_maybe));
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

The `CombinedParser` is callable and reads names:
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
The example creates a julia `NamedTuple`, a `struct` type without dedicated name but field names.
Anonymous `struct` representations are as useful as anonymous functions are to write concisely.
Why?  Omitting to name things works around solving a hard problem.

## Addresses
On a letter, the
```julia
@syntax street_address =
               Sequence(
                   :street => !words, whitespace_horizontal,
                   :no     => Numeric(Int));
```

Defined parsers can be combined.

```julia
@syntax address =
                Sequence(
                    Optional(trim("Adresse:", whitespace=space_maybe)),
                    :door => trim(street_address, whitespace=space_maybe),
                    :zip  => Numeric(Int),
                    :city => trim(!words, whitespace=space_maybe))
```


```jldoctest
julia> address"""Allee 47
       80000 Augsburg
       """
```

## Person's address data
For person entries in texts a `CombinedParser` can be intuitively written as

```julia
@syntax person_adresses = Sequence(
                    Optional(trim("Name:", whitespace=space_maybe)),
                    :person => name, space_maybe,
                    :adresses => join(address, space_maybe));
```


Lets pirate the `NamedTuple` `Base.show` method for testing that parser with a summary
```jldoctest
julia> function Base.show(io::IO, p::result_type(person_adresses)) 
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
       """
Gottfried Wirklich
   Glauberg
   Pinienstadt

julia> Base.methods(Base.show, (IO, result_type(person_adresses))) |> first |> Base.delete_method
```

This example demonstrated `CombinedParser.jl` for creating `NamedTuple` representation from `String`.
