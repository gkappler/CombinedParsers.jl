############################################################
## get methods
## - reorganise result_type here?

Base.map(f::Function, p::TextParse.AbstractToken) =
    map(f, parser(p))

function Base.get(parser::WrappedParser, sequence, till, after, i, state)
    get(parser.parser, sequence, till, after, i, state)
end


"""
    Base.get(parser::Bytes{N,T}, sequence::Vector{UInt8})

!!! note

    Endianness can be achieved by just mapping `bswap`
    ```jldoctest
    julia> map(bswap, Bytes(2,UInt16))([0x16,0x11])
    0x1611

    julia> Bytes(2,UInt16)([0x16,0x11])
    0x1116
    ```
"""
function Base.get(parser::Bytes{N,T}, sequence::Vector{UInt8}, till,
                  after, i, state) where {N,T}
    if isbitstype(T)
        reinterpret(T,sequence[i:after-1])[1]
    else
        T(sequence[i:after-1])
    end
end

function Base.get(parser::AbstractTokenParser,
                  sequence, till,
                  after, i, state)
    state.state
end

function Base.get(parser::Union{NIndexParser{1}, ConstantParser{Char}}, sequence, till, after, i, state)
    @inbounds sequence[i]
end

function Base.get(parser::ConstantParser, sequence, till, after, i, state)
    parser.parser
end

# Base.get(parser::ConstantParser, sequence, till, after, i, state) = parser.parser

    

function Base.get(parser::FlatMap, sequence, till, after, i, state)
    li = rightof(sequence,i,parser.left,tuple_pos(state))
    get(right_parser(state),
        sequence, till,
        after, li,
        right_state(state))
end

function Base.get(parser::Either,
                  sequence, till,
                  after, i, state)
    get(parser.options[either_state_option(state)],
        sequence, till,
        after, i, either_state_state(state))
end

function Base.get(parser::Either{<:Trie},
                  sequence, till,
                  after, i, state::NCodeunitsState)
    get(state.state)
end


if VERSION<v"1.5"
    @warn "julia < 1.5: defining ismutable function for creating copies of default Optional values (only on Union{AbstractDict, AbstractVector, AbstractSet})."
    ismutable(x::Union{AbstractDict, AbstractVector, AbstractSet}) = true
    ismutable(x) = false
end

"""
    _copy(x)

`copy(x)` iif `ismutable(x)`; used when [`defaultvalue`](@ref) of [`Optional`](@ref) results in [`get`](@ref).
"""
_copy(x) =
    if ismutable(x)
        copy(x)
    else
        x
    end

# a String is mutable in julia
# ismutable("a")
_copy(x::AbstractString) = x

Base.get(parser::Optional, sequence, till, after, i, state::NoMatch) = 
    _copy(parser.default)

Base.get(parser::Optional, sequence, till, after, i, state) = 
    get(parser.parser,sequence, till, after, i, state)

function Base.get(parser::Repeat, sequence, till, after, i, state::Vector)
    r = Vector{result_type(parser.parser)}(undef,length(state))
    i_ = i
    for (p,s) in enumerate(state)
        after_ = rightof(sequence,i_,parser.parser,s)
        @inbounds r[p] = get(parser.parser, sequence, till, after_, i_, s)
        i_=after_
    end
    r
end

function Base.get(parser::Repeat, sequence, till, after, i, state::Int)
    r = Vector{result_type(parser.parser)}(undef,state)
    i_=i
    s=MatchState()
    for p in 1:state
        after_ = rightof(sequence,i_,parser.parser,s)
        @inbounds r[p] = get(parser.parser,sequence, till, after_, i_, s)
        i_=after_
    end
    r
end


# Base.get(parser::Sequence, sequence, till, after, i, state::MatchState) =
#     get(parser, sequence, till, after, i, ( MatchState() for i in 1:length(parser.parts)) )
# function Base.get(parser::Sequence, sequence, till::Int, after::Int, i::Int, state)
#     r = Vector{Any}(undef,length(parser.parts))
#     i_::Int = i
#     for (p,s) in enumerate(state)
#         after_ = rightof(sequence,i_,parser.parts[p],s)
#         r[p] = get(parser.parts[p],sequence, till, after_, i_, s)
#         i_=after_
#     end
#     1
#     tuple(r...)
# end
@generated function get(parser::Sequence{pts,sts}, sequence, till::Int, after::Int, posi::Int, states) where {pts,sts}
    fpts = fieldtypes(pts)
    spts = Type[ Union{Nothing,state_type(t)} for t in fpts ]
    n = length(fpts)
    subresult = Symbol[ gensym(:r) for i in fpts ]
    part = Symbol[ gensym(:part) for i in fpts ]
    pposi = Symbol[ gensym(:pos) for i in 1:(n+1) ]
    substate = Symbol[ gensym(:s) for i in fpts ]
    init = if states<:MatchState
        [
            quote
            $(substate[i])::MatchState = MatchState()
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(t,p)) in enumerate(zip(spts,fpts))
        ]
    else
        [
            quote
            $(substate[i])::$t = states[$i]
            @inbounds $(part[i])::$p = parser.parts[$i]
            $(pposi[i])::Int = 0
            end
            for (i,(t,p)) in enumerate(zip(spts,fpts))
        ]
    end
    parseparts = [
        quote
        $(pposi[i+1]) = rightof(sequence,$(pposi[i]),$(part[i]),$(substate[i]))
        $(subresult[i]) = get($(part[i]),sequence, till, $(pposi[i+1]), $(pposi[i]), $(substate[i]))
        end
        for (i,t) in enumerate(fpts)
    ]
    R = quote
        $(pposi[end])::Int = after
        $(init...)
        $(pposi[1]) = posi
        $(parseparts...)
        tuple($(subresult...))
    end
    R
end

## Base.get(x::Palimdrome, str, state) = SubString(str,state...)
## TODO: dispatch Base.get(x::CombinedParser, str, till::Int, after::Int, posi::Int, state) = Base.get(x, str, after, posi, state)
## TODO: dispatch Base.get(x::CombinedParser, str, after::Int, posi::Int, state) = Base.get(x, str, posi, state)
## TODO: dispatch Base.get(x::CombinedParser, str, posi::Int, state) = Base.get(x, str, state)
