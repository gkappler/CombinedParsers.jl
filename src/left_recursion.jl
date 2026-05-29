# src/left_recursion.jl

export resolve_left_recursion

export resolve_left_recursion

"""
    LeftRecurse{P} <: WrappedParser{P}

A thread-safe, Packrat-style wrapper that resolves left-recursion dynamically.
It grows the recursive seed iteratively without modifying the underlying parser graph or types.
"""
struct LeftRecurse{P} <: WrappedParser{P}
    parser::P
end

# Integration with transformations
CombinedParsers._deepmap_parser(f, mem::AbstractDict, x::LeftRecurse, a...; kw...) =
    LeftRecurse(deepmap_parser(f, mem, x.parser, a...; kw...))

# 1. Type Preservation: Ensure the wrapper is completely invisible to the type system
CombinedParsers.result_type(p::LeftRecurse, seq; kw...)  = CombinedParsers.result_type(p.parser, seq; kw...)
CombinedParsers.state_type(::Type{LeftRecurse{P}}) where P = CombinedParsers.state_type(P)

# 2. Domain Preservation: Ensure user transformations (`map`) evaluate normally
Base.get(p::LeftRecurse, seq, till, after, i, state) = Base.get(p.parser, seq, till, after, i, state)

# 3. Dynamic Seed Growing (Warth's Algorithm)
function CombinedParsers.iterate_state(p::LeftRecurse, seq, till, posi, after, state)
    # Return nothing on requested backtracks for the ambiguous root
    if state !== nothing 
        return nothing
    end

    # Unique key for this parser instance and string position
    tls_key = (:CombinedParsers_LR, objectid(p), posi)
    tls = task_local_storage()
    memo = get(tls, tls_key, :unmapped)

    if memo === :unmapped
        # Base case: Prevent infinite loop on the left branch by failing the initial recursive call
        tls[tls_key] = nothing
        
        last_match = nothing
        try
            while true
                # Run the underlying parser. This evaluates Sequence and Transformations natively.
                res = CombinedParsers.iterate_state(p.parser, seq, till, posi, posi, nothing)
                
                # Stop growing the seed if it failed to match, or didn't consume more input than the last iteration
                if res === nothing || (last_match !== nothing && res[1] <= last_match[1])
                    break
                end
                
                last_match = res
                # Memoize the newly grown seed so the next `A α` iteration can build upon it
                tls[tls_key] = last_match
            end
            
            return last_match
        finally
            # Clean up TLS to ensure re-entrancy and prevent memory leaks on exceptions
            delete!(tls, tls_key)
        end
    elseif memo === nothing
        # We hit the base failure, allow alternative `Either` branches to match
        return nothing
    else
        # We hit a memoized seed, return it!
        return memo
    end
end

"""
    resolve_left_recursion(parser::CombinedParser)

A transducer function that deep-maps over a parser graph.
It identifies mutable `Either{<:Vector}` nodes (which are prone to dynamic left recursion)
and wraps them in a recursion-robust `LeftRecurse` wrapper.
"""
function resolve_left_recursion(parser::CombinedParser)
    transducer(x) = x isa Either{<:Vector} ? LeftRecurse(x) : x
    deepmap_parser(transducer, parser)
end
resolve_left_recursion(::Nothing) = nothing
