export WithMemory
export MemoizingParser

"""
    WithMemory(x) <: AbstractString

String wrapper with memoization of next match states for parsers at indices.
Memoization is sometimes recommended as a way of improving the performance of parser combinators (like state machine optimization and compilation for regular languages).


!!! note
    A snappy performance gain could not be demonstrated so far, probably because the costs of state memory allocation for caching are often greater than recomputing a match. 
    If you have a case where your performance benefits with this, let me know!
```
"""
struct WithMemory{S,M} <: StringWrapper
    x::S
    mem::M
    function WithMemory(x::S,mem::M) where {S,M}
        new{S,M}(x,mem)
    end
end
function WithMemory(x)
    WithMemory(x,Dict())
end
Base.show(io::IO, x::WithMemory) =
    print(io,x.x)

"""
    MemoizingParser{P,S,T}

[`WrappedParser`](@ref) memoizing all match states.
For slow parsers with a lot of backtracking this parser can help improve speed.

(Sharing a good example where memoization makes a difference is appreciated.)
"""
@auto_hash_equals struct MemoizingParser{P,S,T} <: WrappedParser{P,S,T}
    parser::P
    function MemoizingParser(p)
        new{typeof(p),state_type(p),result_type(p)}(p)
    end
end

_deepmap_parser(f::Function,mem::AbstractDict,x::MemoizingParser,a...;kw...) =
    MemoizingParser(deepmap_parser(f,mem,x.parser,a...;kw...))

@inline function iterate_state(parser::MemoizingParser, sequence, till, posi,after,state)
    error("for memoizing, wrap sequence in WithMemory. Todo: automize wrapping in root parser with optimize")
    iterate_state(parser.parser, sequence, till,posi,after,state)
end

@inline Base.@propagate_inbounds function iterate_state(parser::MemoizingParser, sequence::WithMemory, till, posi,after,state)
    get!(sequence.mem,(parser.parser,posi,state)) do
        copy(iterate_state(parser.parser, sequence,till,posi,after,state))
    end
end

