
import Nullables: Nullable, isnull
remove_null(x::Nullable{Vector{T}}) where T =
    isnull(x) ? T[] : x.value

remove_null(x::Nullable, default=missing) =
    isnull(x) ? default : remove_null(x.value)

remove_null(x::Pair, default=missing) =
    x.first => remove_null(x.second)

remove_null(x) =
    x

import VectorDicts: VectorDict

## todo: replace with isequalfields?
export isa_reordered
isa_reordered(x::T,S::Type{NamedTuple{n2,t2}}) where {T, n2,t2} =
    all([ fieldtype(T,key) <: fieldtype(S,key)
          for key in n2 ])

isa_reordered(x::T,S::Type) where {T} =
    x isa S

# function instance(t::Type{NamedTuple{n,ts}}, v::Vector, i; kw...) where {n,ts}
#     vs = Any[ remove_null(x) for x in v ]
#     kvs = VectorDict(Pair{Symbol}[ x
#                        for x in vs
#                        if (x isa Pair && x.first !=:_match) ])
#     ks = Any[ x.first for x in kvs ]
#     NamedTuple{n, ts}(tuple([ let fn = fieldname(t,i)
#          _convert(fieldtype(t, i),
#                        get(kw, fn) do
#                        get(kvs, fn, :missing)
#                        end)
#          end
#          for i =1:length(n) ]...) )
# end

"""
construct a named tuple from an iterable after filtering Pair{Symbol} elements.
`kw` takes precedence.
(deprecate! now used in ParserAlchemy)
"""
function construct(t::Type{NamedTuple{n,ts}}, v; kw...) where {n,ts}
    kvs = VectorDict(Pair{Symbol}[ x
                                   for x in v
                                   if (x isa Pair{Symbol}) ])
    NamedTuple{n, ts}(tuple([ let fn = fieldname(t,i)
                              _convert(fieldtype(t, i),
                                       get(kw, fn) do
                                       get(kvs, fn, :missing)
                                       end)
                              end
                              for i =1:length(n) ]...) )
end

