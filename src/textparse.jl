export Numeric
Numeric = TextParse.Numeric
import TextParse: tryparsenext

result_type(::Type{<:AbstractToken{T}}) where T = T

struct AbstractTokenParser{P<:AbstractToken,T} <: LeafParser{NCodeunitsState{T},T}
    parser::P
    function AbstractTokenParser(p::AbstractToken) 
        new{typeof(p), result_type(p)}(p)
    end
end

parser(x::AbstractToken) = AbstractTokenParser(x)

regex_string(::TextParse.Numeric{<:Integer}) = "-?[[:digit:]]+"

print_constructor(io::IO, x::AbstractTokenParser) =
    print(io, typeof(x.parser))

_iterate(parser::AbstractTokenParser, sequence, till, before_i, next_i, state) = 
    _iterate_token(parser.parser, sequence, till, before_i, next_i, state)


function _iterate_token(parser::AbstractToken, sequence, till, before_i, next_i, state::Nothing, opts=TextParse.default_opts)
        r,next_i_ = tryparsenext(parser, sequence, next_i, till,opts)
        if isnull(r)
            nothing
        else
            NCodeunitsState(next_i,next_i_,get(r))
        end
end

"""
    TextParse.tryparsenext(x::CombinedParser,str,i,till,opts=TextParse.default_opts)

TextParse.jl integrates with CombinedParsers.jl both ways.

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
    s = _iterate(x,str,till,i,nothing)
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
