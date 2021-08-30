export NumericParser, DateParser, DateTimeParser
using TextParse
import Dates
import Dates: DateFormat


@deprecate Numeric(x...) NumericParser(x...)

DateParser(format::AbstractString...; locale="english")     = DateParser(Dates.DateFormat.(format, locale)...)
DateTimeParser(format::AbstractString...; locale="english") = DateTimeParser(Dates.DateFormat.(format, locale)...)
DateParser(format::DateFormat...)     = Either(parser.(TextParse.DateTimeToken.(Dates.Date,format))...; simplify=true)
DateTimeParser(format::DateFormat...) = Either(parser.(TextParse.DateTimeToken.(Dates.DateTime,format))...; simplify=true)

"""
    DateParser(format::DateFormat...)
    DateTimeParser(format::DateFormat...)

Create a parser matching either one format
using `TextParse.DateTimeToken` for `Dates.Date` and `Dates.DateTime` respectively.

    DateParser(format::AbstractString...; locale="english")
    DateTimeParser(format::AbstractString...; locale="english")

Convenience functions for above using `Dates.DateFormat.(format, locale)`.
"""
DateParser, DateTimeParser

import TextParse: tryparsenext

result_type(x::AbstractToken) = result_type(typeof(x))
result_type(::Type{<:AbstractToken{T}}) where T = T

struct AbstractTokenParser{P<:AbstractToken,T} <: LeafParser{NCodeunitsState{T},T}
    parser::P
    function AbstractTokenParser(p::AbstractToken)
        new{typeof(p), result_type(p)}(p)
    end
    function AbstractTokenParser{T}(a...; kw...) where T
        p = T(a...; kw...)
        new{typeof(p), result_type(p)}(p)
    end
end
"""
    NumericParser(x...) = parser(TextParse.Numeric(x...))
"""
NumericParser{T} = AbstractTokenParser{T} where {T <: TextParse.Numeric}
NumericParser(x...) = parser(TextParse.Numeric(x...))

parser(x::AbstractToken) = AbstractTokenParser(x)

regex_string(::TextParse.Numeric{<:Integer}) = "-?[[:digit:]]+"
function _printnode(io::IO, x::AbstractTokenParser)
    print_constructor(io, x)
end

print_constructor(io::IO, x::AbstractTokenParser) =
    print(io, x.parser)

print_constructor(io::IO, x::AbstractTokenParser{<:TextParse.DateTimeToken}) =
    print(io, x.parser.format)

iterate_state(parser::AbstractTokenParser, sequence, till, before_i, next_i, state) = 
    iterate_state_token(parser.parser, sequence, till, before_i, next_i, state)


iterate_state_token(parser::AbstractToken, sequence, till, before_i, next_i, state) =
    nothing
function iterate_state_token(parser::AbstractToken, sequence, till, before_i, next_i, state::Nothing, opts=TextParse.default_opts)
        r,next_i_ = tryparsenext(parser, sequence, next_i, till,opts)
        if isnull(r)
            nothing
        else
            NCodeunitsState(next_i,next_i_,get(r))
        end
end

"""
    TextParse.tryparsenext(x::CombinedParser,str,i,till,opts=TextParse.default_opts)

`TextParse.jl` integrates with `CombinedParsers.jl` both ways.
> `tryparsenext` returns a tuple `(result, nextpos)` where `result` is of type `Nullable{T}`, `Nullable{T}()` if parsing failed, non-null containing the parsed value if it succeeded. 
> If parsing succeeded, `nextpos` is the position the next token, if any, starts at. If parsing failed, `nextpos` is the position at which the
parsing failed.

```jldoctest
julia> using TextParse

julia> p = ("Number:" * Repeat(' ') * TextParse.Numeric(Int))[3]
🗄 Sequence[3]
├─ Number\\:
├─ \\ *  |> Repeat
└─ <Int64>
::Int64

julia> parse(p, "Number:    42")
42

julia> TextParse.tryparsenext(p, "Number:    42")
(Nullable{Int64}(42), 14)
```

"""
function TextParse.tryparsenext(x::CombinedParser,str,i,till,opts=TextParse.default_opts)
    s = iterate_state(x,str,till,i,nothing)
    if s === nothing
        Nullable{result_type(x)}(),i
    else
        Nullable(get(x,str,till,tuple_pos(s),i,tuple_state(s))),tuple_pos(s)
    end
end




"""
    regex_prefix(x)

Prefix printed in parser tree node.
"""
regex_prefix(x::AbstractToken) = ""
"""
    regex_suffix(x)

Suffix printed in parser tree node.
"""
regex_suffix(x::AbstractToken) = ""
"""
    regex_inner(x::AbstractToken)

Regex representation of `x`.
See [`regex_string`](@ref)
"""
regex_inner(x::AbstractToken) = "$(typeof(x))"
regex_inner(::TextParse.Numeric{T}) where T = "$(T)"
