pcre_option = 
    Either(
        "mark" => UInt32(0),
        "aftertext" => UInt32(0),
        "dupnames" => UInt32(0),
        "no_start_optimize" => UInt32(0),
        "subject_literal" => UInt32(0),
        "jitstack=256" => UInt32(0),
        "xx" => Base.PCRE.EXTENDED_MORE,
        'i' => Base.PCRE.CASELESS,
        'm' => Base.PCRE.MULTILINE,
        'n' => Base.PCRE.NO_AUTO_CAPTURE,
        'U' => Base.PCRE.UNGREEDY,
        'J' => Base.PCRE.DUPNAMES,
        's' => Base.PCRE.DOTALL,
        'x' => Base.PCRE.EXTENDED,
        'g' => UInt32(0),
        'B' => UInt32(0)
    );

pcre_options = Repeat1(UInt32, Sequence(1,pcre_option,Optional(','))) do v
    |(v...)
end

pcre_options_parser=Sequence(2,AtStart(),Optional(pcre_options,default=UInt32(0)),AtEnd())

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
struct WithOptions{S}
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

import Base: Regex
Base.Regex(x::WithOptions) =
    Regex(x.x, options_string(x.flags))
Base.show(io::IO, x::WithOptions) =
    print(io,x.x)

Base.getindex(x::WithOptions,i...) =
    with_options(x.flags,(getindex(x.x,i...)))

with_options(flags::UInt32,x::WithOptions) =
    with_options(flags,x.x)
with_options(flags::UInt32,x) =
    flags == 0 ? x : WithOptions(x,flags)
with_options(options::AbstractString,str::AbstractString) =
    with_options(parse(pcre_options,options),str)



with_options(set_flags::UInt32, unset_flags::UInt32,x::AbstractString) =
    with_options(set_flags,x)
with_options(set_flags::UInt32, unset_flags::UInt32,x::WithOptions) =
    with_options(set_flags | ( x.flags & ~unset_flags ),x.x)
Base.SubString(x::WithOptions,a...) =
    SubString(x.x,a...)

Base.length(x::WithOptions) =
    length(x.x)
Base.lastindex(x::WithOptions) =
    lastindex(x.x)
Base.firstindex(x::WithOptions) =
    firstindex(x.x)
Base.prevind(x::WithOptions,i::Integer,n::Integer) =
    prevind(x.x,i,n)
Base.nextind(x::WithOptions,i::Integer,n::Integer) =
    nextind(x.x,i,n)
Base.prevind(x::WithOptions,i::Integer) =
    prevind(x.x,i)
Base.nextind(x::WithOptions,i::Integer) =
    nextind(x.x,i)
Base.ncodeunits(x::WithOptions) =
    ncodeunits(x.x)
Base.iterate(x::WithOptions{<:AbstractString},a...) =
    let n = iterate(x.x,a...)
        n===nothing ? nothing : convert(Char,WithOptions(n[1],x.flags)),n[2]
    end

import ..CombinedParsers: ismatch, _ismatch
function ismatch(c::WithOptions{Char},p)
    _ismatch(c.x,p)
end

function Base.convert(::Type{AbstractParser},x::Char)
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
function Base.convert(::Type{AbstractParser},x::WithOptions{Char})
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
struct ParserOptions{P,T} <: WrappedParser{P,T}
    parser::P
    set_flags::UInt32
    unset_flags::UInt32
    ParserOptions(parser,set::UInt32,unset::UInt32) =
        new{typeof(parser),result_type(parser)}(parser,set,unset)
end
map_parser(f::Function,mem::AbstractDict,x::ParserOptions,a...) =
    get!(mem,x) do
        ParserOptions(
            map_parser(f,mem,x.parser,a...),
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


@inline Base.get(parser::ParserOptions, sequence, till, after, i, state) =
    get(parser.parser,
        with_options(parser.set_flags,parser.unset_flags,sequence),
        till, after, i, state)


@inline function _iterate(parser::ParserOptions, sequence, till, i, state)
    _iterate(parser.parser,
             with_options(parser.set_flags,parser.unset_flags,sequence),
             till, i, state)
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
Base.iterate(x::FilterOptions{<:AbstractString},a...) =
    let n = iterate(x.x,a...)
        n===nothing ? nothing : convert(Char,FilterOptions(n[1],x.flags)),n[2]
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
struct OnOptionsParser{P,T} <: WrappedParser{P,T}
    parser::P
    flags::UInt32
    OnOptionsParser(parser,flags::UInt32) =
        new{typeof(parser),result_type(parser)}(parser,flags)
end

function print_constructor(io::IO, x::OnOptionsParser)
    print_constructor(io,x.parser)
    print(io," |> on_options(\"$(options_string(x.flags))\")")
end

map_parser(f::Function,mem::AbstractDict,x::OnOptionsParser,a...) =
    get!(mem,x) do
        OnOptionsParser(
            map_parser(f,mem,x.parser,a...),
            x.flags)
    end
        
on_options(flags::Integer,p) =
    OnOptionsParser(parser(p),UInt32(flags))

@inline function _iterate(parser::OnOptionsParser, sequence, till, i, state)
    _iterate(parser.parser,
             (if_options(parser.flags,sequence)), till, i, state)
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
                  parse::TextParse.AbstractToken{T};
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
