@inline function start_index(sequence,after,parser,state::Nothing)
    after
end
@inline function start_index(sequence,after,parser,state)
    _prevind(sequence, after, parser, state)
end

_nextind(x::AbstractString,i::Int,n::Int=1) =
    nextind(x,i,n)
_prevind(x::AbstractString,i::Int,n::Int=1) =
    prevind(x,i,n)
_nextind(x::AbstractVector,i::Int,n::Int=1) =
    i+n
_prevind(x::AbstractVector,i::Int,n::Int=1) =
    i-n

@inline _prevind(str,i,parser,state::Nothing) = i
@inline _nextind(str,i,parser,state::Nothing) = i

# @inline _prevind(str,i,parser,state) = _prevind(str,i,parser,state)
# @inline _nextind(str,i,parser,state) = _nextind(str,i,parser,state)

"""
    _nextind(str,i::Int,parser,state)

Return the index after the `state` match at `i`.
!!! note
    I am in doubt whether this qualifies as type piracy because I provide an outside method for outside types.
    I thought it might not, because the differentiating two extra arguments. If you have an opinion, please let me know on GitHub.
"""
@inline _nextind(str,i::Int,parser::Union{AbstractString,Char},state) =
    i+ncodeunits(parser)

"""
    _prevind(str,i::Int,parser,state)

Return the index before the `state` match ending before position `i`.
!!! note
    I am in doubt whether this qualifies as type piracy because I provide an outside method for outside types.
    I thought it might not, because the differentiating two extra arguments. If you have an opinion, please let me know on GitHub.
"""
@inline _prevind(str,i::Int,parser::Union{AbstractString,Char},state) where L = 
    i-ncodeunits(parser)
