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

result_type(::Type{<:AbstractToken{T}}, a...; kw...) where T = T

@auto_hash_equals struct AbstractTokenParser{P<:AbstractToken} <: CombinedParser
    parser::P
    function AbstractTokenParser(p::AbstractToken)
        new{typeof(p)}(p)
    end
    function AbstractTokenParser{T}(a...; kw...) where T
        p = T(a...; kw...)
        new{typeof(p)}(p)
    end
end
result_type(x::AbstractTokenParser{AT}, sequence) where {AT<:AbstractToken} =
    result_type(AT,sequence)

@inline state_type(::Type{<:AbstractTokenParser{AT}}) where {AT<:AbstractToken} =
    NCodeunitsState{result_type(AT)}

"""
    NumericParser(x...) = parser(TextParse.Numeric(x...))
"""
NumericParser{T} = AbstractTokenParser{T} where {T <: TextParse.Numeric}
NumericParser(x...) = parser(TextParse.Numeric(x...))

parser(x::AbstractToken) = AbstractTokenParser(x)


iterate_state(parser::AbstractTokenParser, sequence, till, before_i, next_i, state::NCodeunitsState) =
    nothing

function iterate_state(parser::AbstractTokenParser, sequence, till, before_i, next_i, state::Nothing, opts=TextParse.default_opts)
    r,next_i_ = tryparsenext(parser.parser, sequence, next_i, till,opts)
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
        Nullable{result_type(x,str)}(),i
    else
        Nullable(get(x,str,till,tuple_pos(s),i,tuple_state(s))),tuple_pos(s)
    end
end




