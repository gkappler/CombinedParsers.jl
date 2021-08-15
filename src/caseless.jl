export caseless
"""
    caseless(x)
    
[`MappedSequenceParser`](@ref)(lowercase, deepmap_parser(lowercase,parser(x))).


```@meta
DocTestFilters = r"[0-9.]+ .s.*"
```

```jldoctest
julia> p = caseless("AlsO")
🗄  |> MappedSequenceParser
├─ also
└─ lowercase
::SubString{String}

julia> p("also")
"also"

julia> using BenchmarkTools;

julia> @btime match(p,"also");
  51.983 ns (2 allocations: 176 bytes)

julia> p = parser("also")
re"also"

julia> @btime match(p,"also");
  44.759 ns (2 allocations: 176 bytes)

```
"""
caseless(x) =
    MappedSequenceParser(lowercase, deepmap_parser(_lowercase,parser(x)))

export MappedSequenceParser
"""
    MappedSequenceParser(f::F,parser::P) where {F<:Function,P}

Match parser on [`CharMappedString`](https://github.com/gkappler/LazyStrings.jl)`(f,sequence)`, e.g. in a [`caseless`](@ref) parser.
"""
@auto_hash_equals struct MappedSequenceParser{P,S,T,F<:Function} <: WrappedParser{P,S,T}
    parser::P
    f::F
    function MappedSequenceParser(f::F,p::P) where {F<:Function,P}
        new{P,state_type(p),result_type(p),F}(p,f)
    end
end
children(x::MappedSequenceParser) = tuple(x.parser, x.f)

@inline _iterate(parser::MappedSequenceParser, sequence, till, posi,after,state) =
    _iterate(parser.parser, lmap(parser.f,sequence), till,posi,after,state)
