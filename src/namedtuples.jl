
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

function instance(t::Type{NamedTuple{n,ts}}, v::Vector, i; kw...) where {n,ts}
    vs = Any[ remove_null(x) for x in v ]
    kvs = VectorDict(Pair{Symbol}[ x
                       for x in vs
                       if (x isa Pair && x.first !=:_match) ])
    ks = Any[ x.first for x in kvs ]
    NamedTuple{n, ts}(tuple([ let fn = fieldname(t,i)
         _convert(fieldtype(t, i),
                       get(kw, fn) do
                       get(kvs, fn, :missing)
                       end)
         end
         for i =1:length(n) ]...) )
end
