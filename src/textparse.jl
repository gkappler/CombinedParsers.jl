import TextParse: tryparsenext

"""
    TextParse.tryparsenext(x::CombinedParser,str,i,till,opts=TextParse.default_opts)

TextParse.jl integrates with CombinedParsers.jl both ways.

```@meta
DocTestFilters = r"map\\(.+\\)"
```

```jldoctest

julia> p = ("Number:" * Repeat(' ') * TextParse.Numeric(Int))[3]
🗄 Sequence |> map(IndexAt(3))
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























