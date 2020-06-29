pcre_option = 
    Either(
        # with_name(:MARK, "mark" => UInt32(0)),
        # with_name(:aftertext, "aftertext" => UInt32(0)),
        with_name(:DUPNAMES, "dupnames" => Base.PCRE.DUPNAMES),
        # with_name(:no_start_optimize, "no_start_optimize" => UInt32(0)),
        # with_name(:subject_literal, "subject_literal" => UInt32(0)),
        # "jitstack=256" => UInt32(0),
        with_name(:EXTENDED_MORE, "xx" => Base.PCRE.EXTENDED_MORE),
        with_name(:CASELESS, 'i' => Base.PCRE.CASELESS),
        with_name(:MULTILINE, 'm' => Base.PCRE.MULTILINE),
        with_name(:NO_AUTO_CAPTURE, 'n' => Base.PCRE.NO_AUTO_CAPTURE),
        with_name(:UNGREEDY, 'U' => Base.PCRE.UNGREEDY),
        with_name(:DUPNAMES, 'J' => Base.PCRE.DUPNAMES),
        with_name(:DOTALL, 's' => Base.PCRE.DOTALL),
        with_name(:EXTENDED, 'x' => Base.PCRE.EXTENDED),
        # 'g' => UInt32(0),
        with_name(:BINCODE, 'B' => UInt32(0)), # bincode
        with_name(:INFO, 'I' => UInt32(0)) # info
    );

pcre_options = Repeat1(Sequence(1,pcre_option,Optional(','))) do v
    (|(v...))::UInt32
end

pcre_options_parser=Sequence(2,AtStart(),Optional(pcre_options,default=UInt32(0)),AtEnd())

"""
    parse_options(options::AbstractString)

Return PCRE option mask parsed from `options`.

```jldoctest
julia> CombinedParsers.Regexp.pcre_options_parser
🗄 Sequence |> map(#52)
├─ ^ AtStart
├─ 🗄+? Sequence |> map(#52) |> Repeat |> map(#44) |> Optional(default=0)
│  ├─ |🗄... Either
│  │  ├─ dupnames  |> map(Constant(0x00000040)) |> with_name(:DUPNAMES)
│  │  ├─ xx  |> map(Constant(0x01000000)) |> with_name(:EXTENDED_MORE)
│  │  ├─ i  |> map(Constant(0x00000008)) |> with_name(:CASELESS)
│  │  ├─ m  |> map(Constant(0x00000400)) |> with_name(:MULTILINE)
│  │  ├─ n  |> map(Constant(0x00002000)) |> with_name(:NO_AUTO_CAPTURE)
│  │  ├─ U  |> map(Constant(0x00040000)) |> with_name(:UNGREEDY)
│  │  ├─ J  |> map(Constant(0x00000040)) |> with_name(:DUPNAMES)
│  │  ├─ s  |> map(Constant(0x00000020)) |> with_name(:DOTALL)
│  │  ├─ x  |> map(Constant(0x00000080)) |> with_name(:EXTENDED)
│  │  ├─ B  |> map(Constant(0x00000000)) |> with_name(:BINCODE)
│  │  └─ I  |> map(Constant(0x00000000)) |> with_name(:INFO)
│  └─ ,?  |> Optional(default=missing)
└─ \$ AtEnd
::UInt32

```
"""
function parse_options(options::AbstractString)
    flags = tryparse(pcre_options_parser,options)
    if flags === nothing
        throw(UnsupportedError("options $options"))
    else
        flags
    end
end

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
    let sio = IOBuffer()
        print_opts(sio, flags)
        String(take!(sio))
    end

"""
A lazy element transformation type (e.g. AbstractString), 
`getindex` wraps elements in `with_options(flags,...)`.

With parsing options

TODO: make flags a transformation function?
"""
struct WithOptions{S} <: AbstractString
    x::S
    flags::UInt32
    function WithOptions(x,flags)
        @assert !isa(x,WithOptions)
        new{typeof(x)}(x,flags)
    end
end
export flags
flags(x::WithOptions) = x.flags
flags(x) = UInt32(0)


"""
    with_options(options::AbstractString,str::AbstractString)



"""
with_options(flags::UInt32,x::WithOptions) =
    with_options(flags|x.flags,x.x)
with_options(flags::UInt32,x) =
    flags == 0 ? x : WithOptions(x,flags)
with_options(options::AbstractString,str::AbstractString) = 
    with_options(parse_options(options),str)
with_options(set_flags::UInt32, unset_flags::UInt32,x::AbstractString) =
    with_options(set_flags,x)
with_options(set_flags::UInt32, unset_flags::UInt32,x::WithOptions) =
    with_options(set_flags | ( x.flags & ~unset_flags ),x.x)

"""
    convert(::Type{AbstractToken},x::Union{AbstractString,Char})

A [`ConstantParser`](@ref) matching `x`.
"""
Base.convert(::Type{AbstractToken},x::WithOptions{Char}) =
    if !iszero(x.flags & Base.PCRE.CASELESS)
        CharIn(lowercase(x.x),uppercase(x.x))
    else
        convert(AbstractToken,x.x)
    end

import Base: Regex
Base.Regex(x::WithOptions) =
    Regex(x.x, options_string(x.flags))
Base.show(io::IO, x::WithOptions) =
    print(io,x.x)
Base.print(io::IO, x::WithOptions{Char}) =
    print(io,x.x)

Base.getindex(x::WithOptions,i::Integer) =
    with_options(x.flags,(getindex(x.x,i)))
Base.iterate(x::WithOptions{<:AbstractString}) =
    let n = iterate(x.x)
        n===nothing ? nothing : ( with_options(x.flags,n[1]),n[2] ) 
    end
Base.iterate(x::WithOptions{<:AbstractString},i::Integer) =
    let n = iterate(x.x,i)
        n===nothing ? nothing : ( with_options(x.flags,n[1]),n[2] )
    end

Base.isless(x::WithOptions{Char},y) = isless(x.x,y)
(==)(x::WithOptions{Char},y) = x.x==y
Base.SubString(x::WithOptions,start::Int,stop::Int) = with_options(x.flags,SubString(x.x,start,stop))
Base.length(x::WithOptions) =
    length(x.x)
Base.lastindex(x::WithOptions) =
    lastindex(x.x)
Base.firstindex(x::WithOptions) =
    firstindex(x.x)
Base.prevind(x::WithOptions,i::Int,n::Int) =
    prevind(x.x,i,n)
Base.nextind(x::WithOptions,i::Int,n::Int) =
    nextind(x.x,i,n)
Base.prevind(x::WithOptions,i::Int) =
    prevind(x.x,i)
Base.nextind(x::WithOptions,i::Int) =
    nextind(x.x,i)
Base.ncodeunits(x::WithOptions) =
    ncodeunits(x.x)

import ..CombinedParsers: MatchState
@inline function _iterate(p::WithOptions{Char}, sequence, till, posi, next_i, state::Nothing, nc=0)
    @inbounds sc,j=iterate(sequence,posi)
    if ismatch(p,sc)
        j, MatchState()
    else
        nothing
    end
end

import ..CombinedParsers: ismatch, _ismatch
function ismatch(c::WithOptions{Char},p)::Bool
    _ismatch(c.x,p)
end

function _ismatch(c,p::WithOptions{Char})::Bool
    if !iszero(p.flags & Base.PCRE.CASELESS)
        _ismatch(lowercase(c),lowercase(p.x))
    else
        _ismatch(c,p.x)
    end
end

function Base.convert(::Type{CombinedParser},x::Char)
    CharIn(x)
end

import Base: convert
function Base.convert(::Type{Char},y::WithOptions{Char})
    if !iszero(y.flags & Base.PCRE.CASELESS)
        lowercase(y.x)
    else
        y.x
    end
end
function Base.convert(::Type{CombinedParser},x::WithOptions{Char})
    if x.flags & Base.PCRE.CASELESS > 0
        CharIn(unique([lowercase(x.x),uppercase(x.x)])...)
    else
        CharIn(x.x)
    end
end


revert(x::WithOptions) =
    WithOptions(revert(x.x),x.flags)








export set_options, with_options, on_options, map
"""
A wrapper matching the inner parser on `with_options(set_flags, unset_flags, sequence)`.
"""
struct ParserOptions{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    set_flags::UInt32
    unset_flags::UInt32
    ParserOptions(parser,set::UInt32,unset::UInt32) =
        new{typeof(parser),state_type(parser),result_type(parser)}(parser,set,unset)
end
deepmap_parser(f::Function,mem::AbstractDict,x::ParserOptions,a...; kw...) =
    get!(mem,x) do
        ParserOptions(
            deepmap_parser(f,mem,x.parser,a...; kw...),
            x.set_flags,x.unset_flags)
    end

set_options(set::UInt32,unset::UInt32,p) =
    ParserOptions(parser(p),set,unset)

set_options(set::UInt32,parser) =
    set_options(set,UInt32(0),parser)


function regex_prefix(x::ParserOptions)
    "(?" * options_string(x.set_flags) *
    if x.unset_flags!=0
        "-" * options_string(x.unset_flags)
    else
        ""
    end
end
function regex_suffix(x::ParserOptions)
    ")"
end
function print_constructor(io::IO,x::ParserOptions)
    print_constructor(io,x.parser)
    print(io, " |> set_options")
end


@inline Base.get(parser::ParserOptions, sequence, till, after, i, state) =
    get(parser.parser,
        with_options(parser.set_flags,parser.unset_flags,sequence),
        till, after, i, state)


@inline function _iterate(parser::ParserOptions, sequence, till, posi, next_i, state)
    _iterate(parser.parser,
             with_options(parser.set_flags,parser.unset_flags,sequence),
             till, posi, next_i, state)
end




"""
Lazy wrapper for a sequence, masking elements in `getindex` with MatchingNever if any of `flags` are not set.

TODO: make flags a filter function?
"""
struct FilterOptions{S}
    x::S
    flags::UInt32
end
import Base: Regex
Base.show(io::IO, x::FilterOptions) =
    print(io,"\"",x.x,"\"[",options_string(x.flags),"]")

Base.getindex(x::FilterOptions,i) =
    if_options(x.flags,x.x[i])
if_options(flags::UInt32,x::Char) =
    iszero(flags) ? x : MatchingNever{Char}()
if_options(flags::UInt32,x::WithOptions{Char}) =
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
Base.prevind(x::FilterOptions,i::Integer,n::Integer) =
    prevind(x.x,i,n)
Base.nextind(x::FilterOptions,i::Integer,n::Integer) =
    nextind(x.x,i,n)
Base.prevind(x::FilterOptions,i::Integer) =
    prevind(x.x,i)
Base.nextind(x::FilterOptions,i::Integer) =
    nextind(x.x,i)
Base.ncodeunits(x::FilterOptions) =
    ncodeunits(x.x)
Base.iterate(x::FilterOptions,a...) =
    let n = iterate(x.x,a...)
        n===nothing ? nothing : if_options(x.flags,tuple_pos(n)),tuple_state(n)
    end

Base.convert(::Type{Union{Char,CharIn}},x::FilterOptions{Char}) = x.x


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
struct OnOptionsParser{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    flags::UInt32
    OnOptionsParser(parser,flags::UInt32) =
        new{typeof(parser),state_type(parser),result_type(parser)}(parser,flags)
end

function print_constructor(io::IO, x::OnOptionsParser)
    print_constructor(io,x.parser)
    print(io," |> on_options(\"$(options_string(x.flags))\")")
end

deepmap_parser(f::Function,mem::AbstractDict,x::OnOptionsParser,a...; kw...) =
    get!(mem,x) do
        OnOptionsParser(
            deepmap_parser(f,mem,x.parser,a...; kw...),
            x.flags)
    end
        
on_options(flags::Integer,p) =
    OnOptionsParser(parser(p),UInt32(flags))

@inline function _iterate(parser::OnOptionsParser, sequence, till, posi, next_i, state)
    _iterate(parser.parser,
             (if_options(parser.flags,sequence)), till, posi, next_i, state)
end


































export trimstring
trimstring(x::Nothing) = nothing
trimstring(x::AbstractString) =
    replace(x, r"^[ \r\n\t]*|[ \r\n\t]*$" => s"")

export splitter
splitter(S, parse; transform_split = v -> tokenize(S, v), kw...) =
    splitter(Regex(regex_string(S)), parse;
             transform_split = transform_split, kw...)

function splitter(## R::Type,
                  split::Transformation{Regex,S},
                  parse::AbstractToken{T};
                  log=false,
                  transform = (v,i) -> v) where {S, T}
    @warn "todo: using old regex splitting..."
    transform_split = split.transform ## (v,i) -> v
    R = promote_type(S,T)
    function tpn(str, i, n, opts) ## from util.jl:_split
        ## @show str
        ## @show R
        strs = Vector{R}(undef, 0)#[]
        lstr = str[i:min(end,n)]
        r = eachmatch(split.parser, lstr)
        j = 0
        for m in r
            if j <= m.match.offset
                ## m.match.offset  is indexed at 0!!
                ## @show lstr nextind(lstr,j) m.match.offset m.match
                before = SubString(lstr,nextind(lstr,j),prevind(lstr, m.match.offset + (transform_split===nothing ? sizeof(m.match) : 1)))
                log && @info "before" before
                push!(strs, (tokenize(parse, before))) # , i+nextind(lstr,j))) ## todo pass pos!
            end
            if transform_split!==nothing
                log && @info "split" before
                push!(strs, ( transform_split(m, i))) # , i+j) )
            end
            j = m.match.offset + sizeof(m.match) # =.ncodeunits
        end
        ## j = prevind(lstr,j)
        if j <= n-i
            after = SubString(str,i+j,min(lastindex(str),n))
            log && @info "after" after
            push!(strs,
                  (tokenize(parse, after))) ## , i+j)) ## todo pass pos!
        end
        result = transform(strs,i)
        ## error()
        log && @info "split" lstr strs result i j n
        return Nullable(result), nextind(str,n)
    end
    CustomParser(tpn, R)
end


"""
    integer_base(base,mind=0,maxd=Repeat_max)

Parser matching a integer format on base `base`.
"""
function integer_base(base,mind=0,maxd=Repeat_max)
    dig = if base == 16
        hex_digit
    elseif base == 8
        CharIn('0':'7')
    elseif base ==10
        CharIn('0':'9')
    else
        error()
    end
    Repeat(mind:maxd,dig) do v
        (isempty(v) ? 0 : parse(Int,join(v),base=base))::Int
    end
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
            Sequence(
                '\\',
                CombinedParsers.Regexp.integer_base(8,1,3)
            ) do v
            Char(v[2])
            end,
            CombinedParsers.Regexp.escaped_character,
            AnyChar())
        ## compile
        parse(charparser,"a");
        
        unescaped=map(Repeat_until(
            AnyChar(), Sequence(Repeat(' '),'\n');
            wrap=JoinSubstring)) do v
        ## join Chars after unescaping
        join(parse(Repeat(charparser),v))
        end;
        parse(unescaped,"abc\n");
        comment_or_empty = Repeat(
            JoinSubstring(Either(
                Sequence(
                    CombinedParsers.Regexp.at_linestart,
                    '#',Repeat_until(AnyChar(),'\n')),
                Sequence(
                    CombinedParsers.Regexp.at_linestart,
                    Repeat_until(
                        CombinedParsers.Regexp.whitespace_char,'\n')))));
        parse(comment_or_empty,"# test");
        
        #    @test parse(unescaped,"A\\123B\n") == "ASB"
        @syntax pcre_test = begin
        match_test = Sequence(Repeat1(' '),
                              :sequence => unescaped,
                              :expect => Repeat(Sequence(
                                  Repeat(' '),
                                  :i => Either(CombinedParsers.Regexp.integer,"MK"),':',
                                  Repeat(' '),
                                  :result => unescaped))
                              );

        Sequence(
            :pattern => after(CharIn("/'\""),Any) do s
            Repeat_until(
                AnyChar(),
                Sequence(3, NegativeLookbehind('\\'),
                         s, Repeat_until(
                             AnyChar(),
                             Sequence(Repeat(
                                 CombinedParsers.Regexp.whitespace_char), '\n'),
                             wrap=JoinSubstring)),
                true; wrap=JoinSubstring)
            end,
            :test => Repeat(match_test),
            :tests_nomatch => Optional(
                Sequence(
                    2,
                    Optional("\\= Expect no match",
                             Repeat_until(AnyChar(), '\n'; wrap=JoinSubstring)),
                    Repeat(Sequence(2,
                                    Repeat1(' '),
                                    unescaped,
                                    Optional(Sequence("No match",
                                                      Repeat_until(AnyChar(), '\n'; wrap=JoinSubstring)))
                                    ))))
        );
        end;

        function Base.show(io::IO, x::result_type(pcre_test))
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
