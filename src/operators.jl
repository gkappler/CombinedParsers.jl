
## todo: cuts
(/)(x::Any, y::AbstractToken) = sSequence(parser(x),y)
(/)(x::AbstractToken, y::Any) = sSequence(x,parser(y))
(/)(x::AbstractToken, y::AbstractToken) = sSequence(x,y)
