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

julia> p = parser("also"); @btime match(p,"also");
  44.759 ns (2 allocations: 176 bytes)

```
"""
caseless(x) =
    MappedSequenceParser(lowercase, deepmap_parser(lowercase,parser(x)))

export MappedSequenceParser
"""
    MappedSequenceParser(f::F,parser::P) where {F<:Function,P}

Match parser on [`MappedChars`](@ref)`(f,sequence)`, e.g. in a [`caseless`](@ref) parser.
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
    _iterate(parser.parser, MappedChars(parser.f,sequence), till,posi,after,state)


deepmap_parser(f::Function,mem::AbstractDict,x::MappedSequenceParser,a...;kw...) =
    get!(mem,x) do
        ## construct replacement, e.g. if P <: WrappedParser
        MappedSequenceParser(x.f,deepmap_parser(f,mem,x.parser,a...;kw...))
    end
reversed(x::MappedSequenceParser) = MappedSequenceParser(x.f,x.parser)

export MappedChars
"""
    MappedChars(f::Function,x) <: AbstractString

String implementation lazily transforming characters.
Used for parsing with [`MappedSequenceParser`](@ref).
"""
struct MappedChars{S<:AbstractString,M<:Function} <: StringWrapper
    x::S
    f::M
    function MappedChars(f::Function,x::AbstractString)
        new{typeof(x),typeof(f)}(x,f)
    end
end

@inline Base.@propagate_inbounds Base.getindex(x::MappedChars,i::Integer) =
    x.f(getindex(x.x,i))
@inline Base.@propagate_inbounds Base.iterate(x::MappedChars{<:AbstractString}) =
    let i=iterate(x.x)
        i===nothing && return nothing
        x.f(tuple_pos(i)), tuple_state(i)
    end
@inline Base.@propagate_inbounds Base.iterate(x::MappedChars{<:AbstractString},i::Integer) =
    let j=iterate(x.x,i)
        j===nothing && return nothing
        x.f(tuple_pos(j)), tuple_state(j)
    end
@inline Base.@propagate_inbounds Base.SubString(x::MappedChars,start::Int,stop::Int) =
    MappedChars(SubString(x.x,start,stop), x.f)
