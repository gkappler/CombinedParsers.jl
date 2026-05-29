# A recursive decompiler that takes a CombinedParser graph and emits Julia Expr
function to_julia_expr(p::CombinedParsers.NamedParser)
    name = p.name
    body = to_julia_expr(p.parser)
    # Generates: @syntax name = body
    return :(@syntax $name = $body)
end

function to_julia_expr(p::CombinedParsers.Sequence)
    args = map(to_julia_expr, p.parts)
    return :(Sequence($(args...)))
end

function to_julia_expr(p::CombinedParsers.Either)
    args = map(to_julia_expr, p.options)
    return :(Either($(args...)))
end

function to_julia_expr(p::CombinedParsers.Repeat)
    # simplified for demonstration
    body = to_julia_expr(p.parser)
    return :(Repeat($body))
end

function to_julia_expr(p::String)
    return p # Base case for terminals
end

# Usage: 
# julia> ast = ebnf" rule = 'a', 'b' ; "
# julia> to_julia_expr(ast)
# :(@syntax rule = Sequence("a", "b"))
