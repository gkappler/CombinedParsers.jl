"""
    CombinedParsers.state_type(x::Type{<:CombinedParser}) where S
    @inline state_type(x::CombinedParser) = state_type(typeof(x))

Return the state type of `x`.
"""
@inline state_type(t::Type{<:CombinedParser}) =
    error("implement @inline state_type(::Type{$t})")
@inline state_type(x::CombinedParser) = state_type(typeof(x))


"""
    _leftof(str,i,parser::WrappedParser,x)

Convienience function for overriding [`leftof`](@ref) that guarantees that not `x isa Nothing` (returning `i`).
"""
@inline _leftof(str,i,parser::WrappedParser,x) = _leftof(str,i,parser.parser,x)

"""
    _rightof(str,i,parser::WrappedParser,x)

Convienience function for overriding [`rightof`](@ref) that guarantees that not `x isa Nothing` (returning `i`).
"""
@inline _rightof(str,i,parser::WrappedParser,x) = _rightof(str,i,parser.parser,x)

@inline _leftof(str,i,parser::WrappedParser,x::NCodeunitsState) = i-x.nc
@inline _rightof(str,i,parser::WrappedParser,x::NCodeunitsState) = i+x.nc
@inline leftof(str,i,parser,x::NCodeunitsState) = i-x.nc
@inline rightof(str,i,parser,x::NCodeunitsState) = i+x.nc
"""
    NCodeunitsState(posi::Int,after::Int,state)
    NCodeunitsState{S}(posi::Int,after::Int,state)

returns `(tuple_pos, NCodeunitsState(after-posi,state))`.

!!! note:
    rename
"""
NCodeunitsState(posi::Int,after::Int,state) =
    after, NCodeunitsState(after-posi,state)
@inline NCodeunitsState{S}(posi::Int,after::Int,state) where S =
    after, NCodeunitsState{S}(after-posi,state)

# Needed in Wiktionary parser
Base.convert(::Type{NCodeunitsState{S}}, x::NCodeunitsState) where S =
    NCodeunitsState{S}(x.nc,convert(S,x.state))

"""
    tuple_pos(pos_state::Tuple)

[`iterate_state`](@ref) returns a tuple `pos_state` or nothing, and 
`pos_state[1]` is position after match.
"""
@inline tuple_pos(pos_state::Tuple, default...) = pos_state[1]
@inline tuple_pos(pos_state::Nothing, default) = default

"""
    tuple_state(pos_state::Tuple)

[`iterate_state`](@ref) returns a tuple `pos_state` or nothing, and
`pos_state[2]` is the state of match.
"""
@inline tuple_state(pos_state::Tuple) = pos_state[2]
@inline tuple_state(pos_state::Nothing) = nothing




export iterate_state

"""
    iterate_state(parser, sequence[, till::Int=lastindex(sequence)[, posi::Int=firstindex(sequence)[, next_i=posi[, state=nothing]]]])

Return position `after` next match of `parser` in `sequence` at `posi`.
The next match is following current match `state` (first match iif `state==nothing`).

If no next match is found, return `nothing`.

!!! note
    `next_i` is the index in `sequence` after `parser` match at `posi` with `state`.
    
    - `leftof(sequence,next_i,parser,state)==posi`, the start of the `state`-matching subsequence.
    - `rightof(sequence,posi,parser,state)==next_i`, the position after the `state`-matching subsequence.
    - `sequence[leftof(sequence,next_i,parser,state):_prevind(sequence,next_i)]` is the matched subsequence.

!!! note 
    Writing a custom `iterate_state` implementations *must* return
    - `nothing` if no match is found
    - `Tuple{Int64,state_type(parser)}` with next position, match state if a match is found.

"""
@inline iterate_state(parser::CombinedParser, sequence, till=lastindex(sequence), posi=firstindex(sequence)) =
    iterate_state(parser, sequence,till,posi,posi,nothing)
@inline iterate_state(parser::WrappedParser, sequence, till, posi, after, state) =
    iterate_state(parser.parser, sequence, till, posi, after, state)
@inline function iterate_state(parser::FilterParser, sequence, till, posi, next_i, state)
    r::Union{Nothing,Tuple{Int,state_type(parser.parser)}} = nothing
    while r === nothing
        r = iterate_state(parser.parser, sequence, till, posi, next_i, state)
        if r === nothing
            return nothing
        elseif !parser.state_filter(sequence, till, posi, r...)
            next_i,state=r
            r = nothing
        end
    end
    r
end


# for convenience
iterate_state(parser::LeafParser, sequence, till, posi, next_i, state::MatchState)  = nothing

@inline function iterate_state(parser::NIndexParser, sequence, till, posi, next_i, state::Nothing)
    posi > till && return nothing # prevents BoundsError
    ni = rightof(sequence,posi,parser,MatchState())
    if ni <= till+1
        (ni, MatchState())
    else
        nothing
    end
end

@inline function iterate_state(parser::SideeffectParser, sequence, till, posi, next_i, state)
    r = iterate_state(parser.parser, sequence, till, posi, next_i, state)
    if r!==nothing
        parser.effect(sequence,posi,r...,parser.args...)
    else
        parser.effect(sequence,posi,posi,nothing,parser.args...)
    end
    r
end


function iterate_state(tokf::FlatMap, str, till, posi, next_i, state::Nothing)
    posi = next_i
    lr = iterate_state(tokf.left, str, till, posi, next_i, nothing)
    lr === nothing && return nothing
    next_i_ = tuple_pos(lr)
    rightp = parser(tokf.right(get(tokf.left, str, till, next_i_,next_i,tuple_state(lr))))
    rr = nothing
    while rr === nothing
        rr = iterate_state(rightp, str, till, next_i_, next_i_, nothing)
        if rr === nothing
            lr = iterate_state(tokf.left, str, till, posi, next_i_, tuple_state(lr))
            lr === nothing && return nothing
            next_i_ = tuple_pos(lr)
            rightp = parser(tokf.right(get(tokf.left, str, till, next_i_,posi,tuple_state(lr))))
        else
            return flatmap_state(nothing,tuple_state(lr), rightp, rr)
        end
    end
    nothing
end

function iterate_state(tokf::FlatMap, str, till, posi, next_i, state)
    lstate,rightp,rstate = left_state(state), right_parser(state), right_state(state)

    next_i_=next_i
    posi_ = leftof(str,next_i_,rightp,rstate)
    rr = nothing
    while rr === nothing
        rr = iterate_state(rightp, str, till, posi_, next_i_, rstate)
        if rr === nothing
            lr = iterate_state(tokf.left, str, till, posi, next_i_, lstate)
            lr === nothing && return nothing
            next_i_,lstate = lr
            rightp = parser(tokf.right(get(tokf.left, str, till, next_i_,posi,lstate)))
            rstate = nothing
        else
            return flatmap_state(state,lstate, rightp, rr)
        end
    end
end


function iterate_state(parser::Sequence, sequence, till, posi, next_i, states::Nothing)
    length(parser.parts) == 0 && return next_i, sequence_state(state_type(parser))
    sss = Vector{Any}(undef,length(parser.parts))
    sss[1] = nothing
    iterate_state(parser, sequence, till, posi, next_i, sss, 1)
end

function iterate_state(parser::Sequence, sequence, till, posi, next_i, substate::Vector{Any}, p=length(substate))
    next_i_ = next_i
    part=parser.parts
    length(part) == 0 && return nothing
    pposi = [ 0 for _ in 1:(length(substate)+1)]
    pposi[1]=posi
    if p==length(substate)
        pposi[end]=next_i
    end
    while p<=length(substate)
        if iszero(pposi[p])
            pposi[p] = leftof(sequence, pposi[p+1], part[p], @inbounds substate[p])
        end
        if (@inbounds substate[p]) === nothing
            pposi[p+1] = pposi[p]
        end
        r = iterate_state(part[p], sequence, till, pposi[p], pposi[p+1], substate[p])

        if r === nothing
            prune_captures(sequence, pposi[p])
            @inbounds substate[p] = nothing
            pposi[p+1] = pposi[p]
            p == 1 && return nothing
            p -= 1
        else
            pposi[p+1] = tuple_pos(r)
            @inbounds substate[p] = tuple_state(r)
            if p < length(substate)
                @inbounds substate[p+1]=nothing
            end
            p += 1
        end
    end
    pposi[end], sequence_state(state_type(parser), substate)
end

# unambigously
@generated function iterate_state(parser::Sequence{pts}, sequence, till, posi, next_i, states::MatchState) where {pts<:Tuple}
    nothing
end

@generated function iterate_state(parser::Sequence{pts}, sequence, till, posi, next_i, states)::Union{Nothing,Tuple{Int,state_type(Sequence{pts})}} where {pts<:Tuple}
    fpts = fieldtypes(pts)
    spts = Type[ Union{Nothing,state_type(t)} for t in fpts ]
    n = length(fpts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    pposi = Symbol[ gensym(:pos) for p in 1:(n+1) ]
    substate,init = if states<:Nothing
        substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            $(substate[i])::$t = nothing
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    elseif states<:Vector
        substate = Expr[ Expr(Symbol("::"), Expr(:ref,:states,i), t) for (i,(p,t)) in enumerate(zip(fpts,spts)) ]
        ## substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            ## @inbounds $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    elseif states<:Tuple
        substate = Symbol[ gensym(:s) for i in fpts ]
        substate, [
            quote
            @inbounds $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(p,t)) in enumerate(zip(fpts,spts))
        ]
    else
        error("strange sequence state type")
    end

    ret_state = if state_type(parser) <: MatchState
        :(MatchState())
    elseif state_type(parser) <: Tuple
        :(tuple( $([ :(($(s))) for s in substate ]...) ) )
    elseif states <: Nothing
        :(Any[ $([ :(($(s))) for s in substate ]...) ] )
    elseif states <: Vector
        quote
            ## $( [ :(@inbounds states[$i]=$(substate[i])) for i in 1:n ]...)
            states
        end
    else
        error("invalid state_type")
    end
    parseparts = [
        quote
        @label $(subsearch[p])
        if iszero($(pposi[p]))
            $(pposi[p]) = leftof(sequence, $(pposi[p+1]), $(part[p]), @inbounds $(substate[p]))
        end
        if (@inbounds $(substate[p])) === nothing
            ## if sss[$p] === nothing
            $(pposi[p+1]) = $(pposi[p])
        end
        ## TODO: gc happening in next line?
        $(subresult[p]) = iterate_state($(part[p]), sequence, till, $(pposi[p]), $(pposi[p+1]), @inbounds $(substate[p]))
        if $(subresult[p]) === nothing
        prune_captures(sequence,$(pposi[p]))
        @inbounds $(substate[p]) = nothing
        $(pposi[p+1]) = $(pposi[p])
           @goto $(p == 1 ? :theend : subsearch[p-1])
        else
            $(pposi[p+1]) = tuple_pos($(subresult[p]))
            @inbounds $(substate[p]) = tuple_state($(subresult[p]))
        ##$(pposi[p+1]), $(substate[p]) = $(subresult[p])
            $(if p < length(fpts); (:(@inbounds $((substate[p+1]))=nothing)); end )
        end
        end
        for (p,t) in enumerate(fpts)
    ]
    R = quote
        $(init...)
        $(pposi[1]) = posi
        $(pposi[end]) = next_i
        states !== nothing && @goto $(subsearch[end])
        $(parseparts...)
        return $(pposi[end]), $ret_state
        @label theend
        return nothing
    end
    R
end


function iterate_state(t::Repeat, sequence, till, posi, next_i, state)
    next_i_::Int,outer_state::state_type(typeof(t)),goback::Bool = if state === nothing
        es = emptystate(state_type(typeof(t)))
        fill_rep(t,sequence,till,next_i, es)
    else
        if state_length(t,state)==0
            return nothing
            # https://www.pcre.org/original/doc/html/pcrepattern.html:
            # It is possible to construct infinite loops by following
            # a subpattern that can match no characters with a
            # quantifier that has no upper limit, for example:
            
            #   (a?)*
            
            # Earlier versions of Perl and PCRE used to give an error
            # at compile time for such patterns. However, because
            # there are cases where this can be useful, such patterns
            # are now accepted, but if any repetition of the
            # subpattern does in fact match no characters, the loop is
            # forcibly broken.
        #     return nothing
        end
        next_i, state, true
    end
    while goback
        if state_length(t,outer_state)==0
            return nothing
        end
        inner_state, outer_state=poplast!(outer_state,t.parser)
        posi = leftof(sequence,next_i_,t.parser,inner_state) ##state[end][1]
        prune_captures(sequence,posi)
        x = iterate_state(t.parser,sequence, till, posi, next_i_, inner_state)
        x === nothing && state_length(t,outer_state) in t.range && return posi, outer_state
        next_i_,outer_state,goback = push_rep(t,sequence, till, posi, x, outer_state)
    end
    if state_length(t,outer_state) in t.range
        next_i_, outer_state
    else
        nothing
    end
end

@inline function fill_rep(t_::Lazy{<:Repeat}, sequence, till::Int, j::Int, state_)
    t = t_.parser
    tp = t.parser
    while state_length(t,state_) < t.range.start && (x = iterate_state(t.parser,sequence, till,j, j,nothing))!==nothing 
        j_=j
        j, state_ = fill_rep_j_state(x,state_,tp)
        j_==j && break
    end
    j,state_,false
end
function iterate_state(t_::Lazy{<:Repeat}, sequence, till, posi, next_i, state)
    t = t_.parser
    next_i_::Int,state_::state_type(typeof(t)),goback::Bool = if state === nothing
        es = emptystate(state_type(typeof(t)))
        fill_rep(t_,sequence,till,next_i, es)
    else
        if state_length(t,state)<t.range.stop
            x = iterate_state(t.parser,sequence, till, next_i, next_i, nothing)
            if x!==nothing && ( tuple_pos(x)>next_i || state_length(t,state)==0)
                return fill_rep_j_state(x,state,t.parser) #tuple_pos(x),pushstate!(state,t.parser,tuple_state(x))
            end
        end
        next_i, state, true
    end

    while goback
        if state_length(t,state)==0
            return nothing
        end
        lstate, state_=poplast!(state,t.parser)
        posi = leftof(sequence,next_i_,t.parser,lstate) ##state[end][1]
        x = iterate_state(t.parser,sequence, till, posi, next_i_, lstate)
        if x === nothing
            next_i_ = posi
            prune_captures(sequence,next_i_)
            if state_length(t,state_)==0
                return nothing
            end
            state = state_
        else
            next_i_,state_ = pushstate!_fill_rep(t_, sequence, till, state_, x)
            if state_length(t,state_) in t.range
                goback = false
            end
        end
    end
    if state_length(t,state_) in t.range ## && state_length(t,state_)>0
        return next_i_, state_
    else
        nothing
    end
end




function iterate_state(t::Optional, str, till, posi, next_i, state::MatchState)
    prune_captures(str,posi)
    posi, NoMatch()
end

iterate_state(t::Optional, str, till, posi, next_i, state::NoMatch) =
    nothing

function iterate_state(t::Optional, str, till, posi, next_i, state)
    posi = state === nothing ? next_i : leftof(str,next_i,t.parser,state) ##state[end][1]
    r = iterate_state(t.parser, str, till, posi, next_i, state)
    if r === nothing
        prune_captures(str,posi)
        return tuple(posi, NoMatch())
    else
        r
    end
end

iterate_state(t::Lazy{<:Optional}, str, till, posi, next_i, state::Nothing) =
    next_i, NoMatch()
iterate_state(t::Lazy{<:Optional}, str, till, posi, next_i, state::NoMatch) =
    iterate_state(t.parser.parser, str, till, posi, next_i, nothing)
iterate_state(t::Lazy{<:Optional}, str, till, posi, next_i, state) =
    iterate_state(t.parser.parser, str, till, posi, next_i, state)


@inline function iterate_state_paired(first,state,sstate::Nothing)
    nothing
end

@inline function iterate_state_paired(first, state, sstate::Tuple)
    iterate_state_paired(first, state, sstate...)
end

@inline function iterate_state_paired(first, state, next_i_::Int, nstate_)
    next_i_, with_state!(state,first,nstate_)
end

function iterate_state_paired(first, t, str, till, posi, next_i, state)
    iterate_state_paired(first, state, iterate_state(t, str, till, posi, next_i, either_state_state(state)))
end

function iterate_state(t::Either{<:Vector}, str, till, posi, next_i, state::Nothing)
    r = nothing
    for (j,o) in enumerate(t.options)
        r = iterate_state_paired(j,o,str,till,posi, next_i,nothing)
        r!== nothing && return r
    end
    nothing
end

function iterate_state(t::Either{<:Vector}, str, till, posi, next_i, state)
    @inbounds opt = t.options[either_state_option(state)]
    fromindex = either_state_option(state)+1
    posi = leftof(str,next_i,opt,either_state_state(state)) ##state[end][1]
    r = iterate_state_paired(either_state_option(state),opt,str,till,posi, next_i,state)
    r !== nothing && return r
    prune_captures(str,posi)
    ##sstate = nothing
    for j in fromindex:length(t.options)
        @inbounds r2 = iterate_state_paired(j,t.options[j],str,till,posi,posi,nothing)
        r2 !== nothing && return r2
    end
    nothing
end


function iterate_state(parser::Either{<:Tuple}, sequence, till, posi, next_i, state)
    either_first(parser,posi,next_i,state) do index, option, ni, sstate
        iterate_state_paired(index, option, sequence, till, posi, ni, sstate)
    end
end



@generated function either_first(f::Function, parser::Either{pts}, posi, next_i, state) where {pts<:Tuple}
    fpts = fieldtypes(pts)
    subsearch = Symbol[ gensym(:subsearch) for p in fpts ]
    push!(subsearch, gensym(:subsearch))
    subresult = Symbol[ gensym(:r) for p in fpts ]
    part = Symbol[ gensym(:part) for p in fpts ]
    init = Expr(:(=), Expr(:tuple, part...),:(parser.options))
    parseoptions = [
        quote
        @label $(subsearch[p])
        j > $p && @goto $(subsearch[p+1])
        $(subresult[p]) = f($p,$(part[p]), next_i_, sstate)
        $(subresult[p]) !== nothing && return $(subresult[p])# iterate_state_paired($p,state, $(subresult[p]))
        next_i_ = posi
        sstate = nothing
        end
        for (p,t) in enumerate(fpts)
    ]
    init_before = 
        quote
            j = either_state_option(state)
            sstate = state
        end
    R = quote
        next_i_::Int = next_i
        $(init)
        $(init_before)
        $(parseoptions...)
        @label $(subsearch[end])
        return nothing
    end
    R
end
@inline iterate_state(parser::Atomic, sequence, till, posi, next_i, state::Nothing) =
    iterate_state(parser.parser, sequence, till, posi, next_i, state)
@inline iterate_state(parser::Atomic, sequence, till, posi, next_i, state) =
    nothing

