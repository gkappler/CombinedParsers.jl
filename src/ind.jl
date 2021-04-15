
_nextind(x::AbstractString,i::Int) =
    nextind(x,i)
_prevind(x::AbstractString,i::Int) =
    prevind(x,i)

_nextind(x::AbstractString,i::Int,n::Int) =
    nextind(x,i,n)
_prevind(x::AbstractString,i::Int,n::Int) =
    prevind(x,i,n)
_nextind(x::AbstractVector,i::Int,n::Int=1) =
    i+n
_prevind(x::AbstractVector,i::Int,n::Int=1) =
    i-n


"""
    leftof(str,i,parser,state)

Left of `parser` match in `str` *before* `i` encoded by `state`,
or `i` if `state===nothing`.

!!! note
    override with [`_leftof`](@ref) and  [`_rightof`](@ref).
"""
@inline leftof(str,i,parser,state::Nothing) = i

"""
    rightof(str,i,parser,state)

Left of `parser` match in `str` at `i` encoded by `state`,
or `i` if `state===nothing`.

!!! note
    override with [`_leftof`](@ref) and  [`_rightof`](@ref).
"""
@inline rightof(str,i,parser,state::Nothing) = i

@inline leftof(str,i,parser,state) = _leftof(str,i,parser,state)
@inline rightof(str,i,parser,state) = _rightof(str,i,parser,state)

