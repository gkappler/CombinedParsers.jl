import ..CombinedParsers: MatchState, iostring

function print_opts(io,opts)
    if (opts & Base.PCRE.CASELESS ) != 0; print(io, 'i'); end
    if (opts & Base.PCRE.MULTILINE) != 0; print(io, 'm'); end
    if (opts & Base.PCRE.DOTALL   ) != 0; print(io, 's'); end
    if (opts & Base.PCRE.EXTENDED_MORE ) != 0; print(io, "xx"); end
    if (opts & Base.PCRE.EXTENDED ) != 0; print(io, 'x'); end
    if (opts & Base.PCRE.DUPNAMES   ) != 0; print(io, 'J'); end
    if (opts & Base.PCRE.UNGREEDY ) != 0; print(io, 'U'); end
end
options_string(flags) =
    iostring(print_opts, flags)

"""
A lazy element transformation type (e.g. AbstractString), 
`getindex` wraps elements in `with_options(flags,...)`.

With parsing options

TODO: make flags a transformation function?
"""
@auto_hash_equals struct CharWithOptions <: AbstractChar
    x::Char
    flags::UInt32
end

Base.codepoint(x::CharWithOptions) = codepoint(x.x)
Base.isascii(x::CharWithOptions) = isascii(x.x)
Base.isprint(x::CharWithOptions) = isprint(x.x)

@inline function iterate_state(p::CharWithOptions, sequence, till, posi, next_i, state::Nothing, nc=0)
    @inbounds sc,j=iterate(sequence,posi)
    if ismatch(p,sc)
        j, MatchState()
    else
        nothing
    end
end

Base.print(io::IO, x::CharWithOptions) =
    print(io,x.x)
Base.isless(x::CharWithOptions,y::AbstractChar) = isless(x.x,y)
Base.isless(x::AbstractChar,y::CharWithOptions) = isless(x,y.x)
(==)(x::CharWithOptions,y::AbstractChar) = x.x==y

import ..CombinedParsers: ismatch, _ismatch


function ismatch(c::CharWithOptions,p)::Bool
    _ismatch(c.x,p)
end

function ismatch(c,p::CharWithOptions)::Bool
    if !iszero(p.flags & Base.PCRE.CASELESS)
        _ismatch(lowercase(c),lowercase(p.x))
    else
        _ismatch(c,p.x)
    end
end


function ismatch(c::CharWithOptions,p::CharWithOptions)::Bool
    _ismatch(c.x,p)
end

import Base: convert
"""
    Base.convert(::Type{Char},y::CharWithOptions)

Strips options.
"""
Base.convert(::Type{Char},y::CharWithOptions) =
    y.x

using LazyStrings

"""
A lazy element transformation type (e.g. AbstractString), 
`getindex` wraps elements in `with_options(flags,...)`.

With parsing options

TODO: make flags a transformation function?
"""
@auto_hash_equals struct StringWithOptions{S<:AbstractString} <: StringWrapper
    x::S
    flags::UInt32
    function StringWithOptions(x,flags)
        @assert !isa(x,StringWithOptions)
        new{typeof(x)}(x,flags)
    end
end
import Base: Regex
Base.Regex(x::StringWithOptions) =
    Regex(x.x, options_string(x.flags))
export flags
flags(x::StringWithOptions) = x.flags
flags(x) = UInt32(0)

"""
    with_options(flags::UInt32,x::AbstractString)

Return 'x` if `iszero(0)`, otherwise `StringWithOptions` with `flags`.
"""
with_options(flags::UInt32,x::AbstractString) =
    iszero(flags) ? x : StringWithOptions(x,flags)

"""
    with_options(flags::UInt32,x::Char)

Return 'x` if `iszero(0)`, otherwise `CharWithOptions` with `flags`.
"""
with_options(flags::UInt32,x::Char) =
    iszero(flags) ? x : CharWithOptions(x,flags)


"""
    with_options(flags::AbstractString,x)

Return `with_options(parse_options(options),x)`, see [`parse_options`](@ref).
"""
with_options(options::AbstractString,str) = 
    with_options(parse_options(options),str)


WithOptions = Union{StringWithOptions,CharWithOptions}
with_options(flags::UInt32,x::WithOptions) =
    with_options(flags|x.flags,x.x)

"""
    with_options(set_flags::UInt32, unset_flags::UInt32,x)

Set options `set_flags | ( x.flags & ~unset_flags )` if `x isa WithOptions`, 
set options `set_flags` otherwise.
"""
with_options(set_flags::UInt32, unset_flags::UInt32,x::WithOptions) =
    with_options(set_flags | ( x.flags & ~unset_flags ),x.x)
with_options(set_flags::UInt32, unset_flags::UInt32,x) =
    with_options(set_flags,x)

Base.getindex(x::StringWithOptions,i::Integer) =
    with_options(x.flags,(getindex(x.x,i)))
Base.iterate(x::StringWithOptions{<:AbstractString}) =
    let n = iterate(x.x)
        n===nothing ? nothing : ( with_options(x.flags,n[1]),n[2] ) 
    end
Base.iterate(x::StringWithOptions{<:AbstractString},i::Integer) =
    let n = iterate(x.x,i)
        n===nothing ? nothing : ( with_options(x.flags,n[1]),n[2] )
    end
(==)(x::StringWithOptions{<:AbstractString},y::String) = x.x==y
Base.SubString(x::StringWithOptions,start::Int,stop::Int) =
    with_options(x.flags,SubString(x.x,start,stop))



"""
    parser(x::CharWithOptions)

A [`ConstantParser`](@ref) matching `x`, 
respecting `Base.PCRE.CASELESS`.
"""
parser(x::CharWithOptions) =
    if !iszero(x.flags & Base.PCRE.CASELESS)
        ValueIn(lowercase(x.x),uppercase(x.x))
    else
        parser(x.x)
    end

reversed(x::StringWithOptions) =
    StringWithOptions(reversed(x.x),x.flags)








export set_options, with_options, on_options, map
"""
A wrapper matching the inner parser on `with_options(set_flags, unset_flags, sequence)`.
"""
struct ParserOptions{P} <: WrappedParser{P}
    parser::P
    set_flags::UInt32
    unset_flags::UInt32
    ParserOptions(parser,set::UInt32,unset::UInt32) =
        new{typeof(parser)}(parser,set,unset)
end
_deepmap_parser(f::Function,mem::AbstractDict,x::ParserOptions,a...; kw...) =
    ParserOptions(
        deepmap_parser(f,mem,x.parser,a...; kw...),
        x.set_flags,x.unset_flags)

set_options(set::UInt32,unset::UInt32,p) =
    ParserOptions(parser(p),set,unset)

set_options(set::UInt32,parser) =
    set_options(set,UInt32(0),parser)



@inline Base.get(parser::ParserOptions, sequence, till, after, i, state) =
    get(parser.parser,
        with_options(parser.set_flags,parser.unset_flags,sequence),
        till, after, i, state)


@inline function iterate_state(parser::ParserOptions, sequence, till, posi, next_i, state)
    iterate_state(parser.parser,
                  with_options(parser.set_flags,parser.unset_flags,sequence),
                  till, posi, next_i, state)
end




"""
Helper struct to mask sequence elements from matchers.
"""
struct MatchingNever{T} end
"""
    ismatch(c::MatchingNever,p)

returns `false`.
"""
ismatch(c::MatchingNever,p)::Bool = false
ismatch(c::MatchingNever,p::AnyValue)::Bool = false

"""
Lazy wrapper for a sequence, masking elements in `getindex` with MatchingNever if any of `flags` are not set.

TODO: make flags a filter function?
resolve confound of sequence and value, like StringWithOptions, CharWithOptions
"""
struct FilterOptions{I} <: StringWrapper
    x::I
    flags::UInt32
end
import Base: Regex
Base.show(io::IO, x::FilterOptions) =
    print(io,"\"",x.x,"\"[",options_string(x.flags),"]")

Base.getindex(x::FilterOptions,i::Integer) =
    if_options(x.flags,x.x[i])

if_options(flags::UInt32,x::Char) =
    iszero(flags) ? x : MatchingNever{typeof(x)}()
if_options(flags::UInt32,x::CharWithOptions) =
    if (flags & x.flags) == flags
        x.x
    else
        MatchingNever{Char}()
    end

if_options(flags::UInt32,x::FilterOptions) =
    FilterOptions(x.x,flags | x.flags)
if_options(flags::UInt32,x) =
    iszero(flags) ? x : FilterOptions(x,flags)

Base.length(x::FilterOptions) =
    length(x.x)
Base.lastindex(x::FilterOptions) =
    lastindex(x.x)
Base.firstindex(x::FilterOptions) =
    firstindex(x.x)
Base.ncodeunits(x::FilterOptions) =
    ncodeunits(x.x)
Base.iterate(x::FilterOptions,a...) =
    let n = iterate(x.x,a...)
        n===nothing ? nothing : if_options(x.flags,tuple_pos(n)),tuple_state(n)
    end



function Base.in(x::FilterOptions,set)
    if !iszero(x.flags & Base.PCRE.CASELESS)
        lowercase(x.x) in set || uppercase(x.x) in set
    else
        x.x in set
    end
end













export on_options
"""
Parser wrapper sequence with `if_options`.
"""
struct OnOptionsParser{P} <: WrappedParser{P}
    parser::P
    flags::UInt32
    OnOptionsParser(parser,flags::UInt32) =
        new{typeof(parser)}(parser,flags)
end

function print_constructor(io::IO, x::OnOptionsParser; kw...)
    printstyled(io,"on_options(\"$(options_string(x.flags))\")"; color = treecolor.julia_structure)
end

_deepmap_parser(f::Function,mem::AbstractDict,x::OnOptionsParser,a...; kw...) =
    OnOptionsParser(
        deepmap_parser(f,mem,x.parser,a...; kw...),
        x.flags)

"""
    on_options(flags::Integer,parser)

create parser that matches if `flags` are set in sequence, and `parser` matches.

Used for PCRE parsing, e.g.
```julia
Either(
    on_options(Base.PCRE.MULTILINE, 
           '^' => at_linestart),
    parser('^' => AtStart())
)
```
"""
on_options(flags::Integer,p) =
    OnOptionsParser(parser(p),UInt32(flags))

@inline function iterate_state(parser::OnOptionsParser, sequence, till, posi, next_i, state)
    iterate_state(parser.parser,
                  (if_options(parser.flags,sequence)), till, posi, next_i, state)
end


"""
    @pcre_testset

Define `@syntax pcre_test` and `@syntax pcre_tests` for parsing unit test output of the PCRE library.
The parser is used for testing `CombinedParser` and benchmarking against `Regex`.
"""
macro pcre_tests()
    esc(quote
            ## not handled in escaped_character, but backreference, if a capture with number (in decimal) is defined
            charparser = Either(
                mSequence(
                    '\\',
                    CombinedParsers.integer_base(8,1,3)
                ) do v
                    Char(v[2])
                end,
                CombinedParsers.Regexp.escaped_character,
                AnyChar())
            ## compile
            parse(charparser,"a");
            
            unescaped=map(Repeat_until(
                AnyChar(), Sequence(Repeat(' '),'\n');
                wrap=MatchedSubSequence)) do v
                    ## join Chars after unescaping
                    join(parse(Repeat(charparser),v))
                end;
            parse(unescaped,"abc\n");
            comment_or_empty = Repeat(
                MatchedSubSequence(Either(
                    Sequence(
                        CombinedParsers.at_linestart,
                        '#',Repeat_until(AnyChar(),'\n')),
                    Sequence(
                        CombinedParsers.at_linestart,
                        Repeat_until(
                            CombinedParsers.whitespace_char,'\n')))));
            parse(comment_or_empty,"# test");
            
            #    @test parse(unescaped,"A\\123B\n") == "ASB"
            @syntax pcre_test = begin
                match_test = Sequence(Repeat1(' '),
                                      :sequence => unescaped,
                                      :expect => Repeat(Sequence(
                                          Repeat(' '),
                                          :i => Either(CombinedParsers.integer(),"MK"),':',
                                          Repeat(' '),
                                          :result => unescaped))
                                      );

                Sequence(
                    :pattern => after(CharIn("/'\""),Any) do s
                        Repeat_until(
                            AnyChar(),
                            mSequence(3, NegativeLookbehind('\\'),
                                      s, Repeat_until(
                                          AnyChar(),
                                          Sequence(Repeat(
                                              CombinedParsers.whitespace_char), '\n'),
                                          wrap=MatchedSubSequence)),
                            true; wrap=MatchedSubSequence)
                    end,
                    :test => Repeat(match_test),
                    :tests_nomatch => Optional(
                        mSequence(
                            2,
                            Optional("\\= Expect no match",
                                     Repeat_until(AnyChar(), '\n'; wrap=MatchedSubSequence)),
                            Repeat(mSequence(2,
                                             Repeat1(' '),
                                             unescaped,
                                             Optional(Sequence("No match",
                                                               Repeat_until(AnyChar(), '\n'; wrap=MatchedSubSequence)))
                                             ))))
                );
            end;

            function Base.show(io::IO, x::NamedTuple{(:pattern, :test, :tests_nomatch)})
                print(io, "Pattern: ")
                printstyled(io,"r(e)\"$(x.pattern[1])\"$(x.pattern[2])\n", color=:underline)
                println(io, "Test Examples:")
                for (i,t) in enumerate(x.test)
                    println(io, "   $i. $(t.sequence)")
                end
                println(io, "Not Examples:")
                for (i,t) in enumerate(x.tests_nomatch)
                    println(io, "   $i. $t")
                end
            end
            
            @syntax pcre_tests = Sequence(
                Repeat(Sequence(comment_or_empty,
                                pcre_test)),
                comment_or_empty,
                AtEnd());
        end)
end
