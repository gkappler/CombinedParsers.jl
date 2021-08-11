
# unambigously
Base.convert(::Type{CombinedParser},x::CombinedParser) =
    x

"""
    Base.convert(::Type{CombinedParser},x)

[`parser`](@ref)`(x)`.
"""
Base.convert(::Type{CombinedParser},x) =
    parser(x)

"""
    parser(x)

A [`ConstantParser`](@ref) matching `x`.
"""
parser(x) =
    ConstantParser(x)

"""
    parser(x::StepRange{Char,<:Integer})

[`ValueIn`](@ref) matching x.
"""
parser(x::StepRange) =
    ValueIn(x)
parser(x::CombinedParser) = x

"""
    parser(x::Pair{Symbol, P}) where P

A parser labelled with name `x.first`.
Labels are useful in printing and logging.

See also: [`@with_names`](@ref), [`with_name`](@ref), [`log_names`](@ref)
"""
parser(x::Pair{Symbol}) =
    NamedParser(x.first, parser(x.second))
