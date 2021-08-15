
    # error?
function Base.getindex(x::ParseMatch{<:Union{Transformation}},a...)
    println("implement lazy getindex(::$(typeof(x))")
    getindex(get(x), a...)
end


getstateindex(state::Integer, a...) = MatchState()

function Base.getindex(x::ParseMatch{<:Sequence}, i)
    mi = x.parsings
    sequence = mi.sequence
    inner = 
    outer_state = x.state
    inner_state = getindex(outer_state, i)
    l = lastindex(mi.parser.parts)
    start = stop = x.stop
    ## walk back
    while l>=i
        stop = start
        start = leftof(sequence,stop,mi.parser.parts[l],outer_state[l]) ##state[end][1]
        l-=1
    end
    pm = ParseMatch(MatchesIterator(inner,mi.sequence, mi.from, mi.till),
                    start, stop, outer_state[i])
end

function Base.getindex(x::ParseMatch{<:Repeat}, i)
    mi = x.parsings
    sequence = mi.sequence
    inner = mi.parser.parser
    outer_state = x.state
    l = state_length(mi.parser,outer_state)
    local inner_state::state_type(inner)
    start = stop = x.stop
    ## walk back
    while l>=i
        stop = start
        # outer_state::Union{<:Vector, Int}
        inner_state, outer_state = poplast!(outer_state)
        start = leftof(sequence,stop,inner,inner_state) ##state[end][1]
        l-=1
    end
    pm = ParseMatch(MatchesIterator(inner,mi.sequence, mi.from, mi.till),
                    start, stop, inner_state)
end

function Base.getindex(x::ParseMatch{<:MatchedSubSequence},a...)
    getindex(x.parsings.sequence, ( nextind(x.parsings.sequence,x.start,i-1) for i in a)...)
end

function Base.getindex(x::ParseMatch{<:WrappedParser},a...)
    mi = x.parsings
    pm = ParseMatch(MatchesIterator(mi.parser.parser,mi.sequence,mi.from, mi.till),
                    x.start,x.stop,x.state)
    getindex(pm,a...)
end
