############################################################
## get methods
## - reorganise result_type here?

function Base.get(
    parser::W, sequence, till,
    after, i, state
) where {W <: WrappedParser}
    get(parser.parser, sequence, till, after, i, state)
end

function Base.get(
    parser::W, sequence, till,
    after, i, state
) where {W <: LookAround}
    after_ = nextind(sequence,i,parser.parser,state)
    get(parser.parser, sequence, till,
        after_, i, state)
end

function Base.get(parser::AbstractToken,
                  sequence, till,
                  after, i, state)
    parser isa CombinedParser && error("define Base.get(parser::$(typeof(parser)), sequence, till, after, i, state)")
    state.state
end

"""
    Base.get(parser::AbstractToken{Nothing}, sequence, till, after, i, state)

Default method for parser types returning nothing
"""
Base.get(
    parser::AbstractToken{<:Union{Nothing,Always,NegativeLookahead,AtStart,AtEnd,NegativeLookbehind}},
    sequence, till, after,
    i, state) =
    parser

function Base.get(t::PositiveLookbehind,
                  str, till,
                  after, i, state)
    rseq = revert(str)
    get(t.parser, rseq, till,
        after,
        reverse_index(rseq,prevind(rseq,i)),
        state)
end

function Base.get(
    x::Union{AbstractString,JoinSubstring,ConstantParser{<:Any,<:AbstractString}},
    sequence, till,
    after, i, state)
    li = prevind(sequence,after)
    li<i ? "" : @inbounds SubString(sequence,i,li)
end

function Base.get(
    parser::Union{Char,NIndexParser{1,Char},ConstantParser{1,Char}},
    sequence, till,
    after, i, state)::Char
    @inbounds sequence[i]
end

# Base.get(parser::ConstantParser, sequence, till, after, i, state) = parser.parser




"""
    Base.get(parser::Transformation, a...)

Constant value `parser.transform` fallback method.
"""
function Base.get(parser::JoinSubstring{<:NamedParser}, sequence, till, after, i, state)
    parser.parser.name => get(parser.parser.parser, sequence, till, after, i, state)
    
end

function Base.get(parser::FlatMap, sequence, till, after, i, state)
    li = nextind(sequence,i,parser.left,tuple_pos(state))
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

function Base.get(parser::Optional,
                  sequence, till,
                  after, i, state)
    if state === None()
        parser.default
    else
        get(parser.parser,sequence, till, after, i, state)
    end
end

function Base.get(parser::Repeat,
                  sequence, till,
                  after, i, state::Vector)
    r = Vector{result_type(parser.parser)}(undef,length(state))
    i_=i
    for (p,s) in enumerate(state)
        after_=nextind(sequence,i_,parser.parser,s)
        @inbounds r[p] = get(parser.parser,sequence, till, after_, i_, s)
        i_=after_
    end
    r
end

function Base.get(parser::Repeat,
                  sequence, till,
                  after, i, state::Int)
    r = Vector{result_type(parser.parser)}(undef,state)
    i_=i
    s=MatchState()
    for p in 1:state
        after_=nextind(sequence,i_,parser.parser,s)
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
#         after_=nextind(sequence,i_,parser.parts[p],s)
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
        $(pposi[i+1]) = nextind(sequence,$(pposi[i]),$(part[i]),$(substate[i]))
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

export Transformation
"""
    Transformation{T}(f::Function, p_) where {T}
    map_at(f::Function, p, a...)
    map_at(f::Function, Tc::Type, p, a...)
    Base.map(f::Function, Tc::Type, p::ParserTypes, a...)
    Base.map(f::Function, p::ParserTypes, a...)

Parser transforming result of a wrapped parser. 
`a...` is passed as additional arguments to `f` (at front .
"""
@auto_hash_equals struct Transformation{F,P,S,T} <: WrappedParser{P,S,T}
    transform::F
    parser::P
    Transformation{T}(transform, p_) where {T} =
        let p = parser(p_)
            new{typeof(transform),typeof(p),state_type(p),T}(transform, p)
        end
    Transformation{T}(transform, p_::NamedParser) where {T} =
        let p = p_.parser
            tp = new{typeof(transform),typeof(p),state_type(p),T}(transform, p)
            with_name(p_.name,tp)
        end
end
deepmap_parser(f::Function,mem::AbstractDict,x::Transformation,a...;kw...) =
    get!(mem,x) do
        Transformation{result_type(x)}(
            x.transform,
            deepmap_parser(f,mem,x.parser,a...;kw...))
    end


"""
A Transformation{<:Constant} skips evaluation of get(.parser).
"""
struct Constant{T}
    value::T
end
function map_constant(transform, p::AbstractToken)
    T=typeof(transform)
    Transformation{T}(Constant(transform), p)
end
"""
    parser(constant::Pair{<:ParserTypes})
    map_constant(constant, parser::AbstractToken)

A parser mapping matches of `parser` `x.first` to `constant` `x.second`.
"""
parser(constant::Pair{<:ParserTypes}) =
    map_constant(constant.second, parser(constant.first))
"""
    Base.get(parser::Transformation{<:Constant}, a...)

does not evaluate `get(parser.transform,...)`.
"""
function Base.get(parser::Transformation{<:Constant}, sequence, till, after, i, state)
    parser.transform.value
end

function _string(io::IO,x::Constant)
    print(io,"Constant(")
    show(io,x.value)
    print(io,")")
end
_string(io::IO,x::Function) = print(io,x)
children(x::Transformation) = children(x.parser)
function print_constructor(io::IO,x::Transformation)
    print_constructor(io,x.parser)
    print(io," |> map(")
    _string(io,x.transform)
    print(io,")")
end

"""
    Base.get(parser::Transformation{<:Function}, a...)

Function call `parser.transform(get(parser.parser,a...),i)`.
"""
function Base.get(parser::Transformation{<:Function}, sequence, till, after, i, state)
    v = get(parser.parser, sequence, till, after, i, state)
    parser.transform(v,i)
end

export IndexAt
struct IndexAt{I}
    i::I
end
# @inline Base.getindex(x,i::IndexAt) = getindex(x,i.i...)
_string(io::IO,x::IndexAt) = print(io,"IndexAt(",x.i,")")

"""
    Base.get(parser::Transformation{<:IndexAt}, a...)

`getindex(get(parser.parser,a...).parser.transform)`
"""
function Base.get(parser::Transformation{IndexAt{I}}, sequence, till, after, i, state) where {I <: Integer}
    v = get(parser.parser,sequence, till, after, i, state)
    v[parser.transform.i]
end
function Base.get(parser::Transformation{IndexAt{Is}}, sequence, till, after, i, state) where {Is <: Union{Tuple, Vector, UnitRange}}
    tuple(get(parser.parser,sequence, till, after, i, state)[parser.transform.i]...)
end


function _iterate(parser::Transformation, sequence, till, posi, next_i, state)
    r = _iterate(parser.parser, sequence, till, posi, next_i, state )
end

function infer_result_type(f::Function,Tc::Type,p::ParserTypes,onerror::AbstractString,ts::Type...)
    Ts = Base.return_types(f, tuple(result_type(p),ts...))
    isempty(Ts) && error("transformation type signature mismatch $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    ( length(Ts) > 1 || Any <: first(Ts) ) && return Tc ##error(onerror*"  $f$(tuple(result_type(p),ts...))::$Ts<:$Tc")
    T = first(Ts)
    if T <: Tc
        T
    else
        @warn "type mismatch $f$(tuple(result_type(p),ts...))::$T<:$Tc"
        Tc
    end
end

export map,map_at 
"""
    map_at(f::Function, p, a...)
    map_at(f::Function, Tc::Type, p, a...)

Parser transforming result of a wrapped parser. 
`a...` is passed as additional arguments to `f`.

See also: [`map`](@ref), [`Transformation`](@ref)
"""
function map_at(f::Function, Tc::Type, p, a...)
    T = infer_result_type(f,Tc,p,"call map_at(function,type,parts...)",Int,typeof.(a)...)
    Transformation{Tc}((v,i) -> (f((v), i, a...)), p)
end
function map_at(f::Function, p, a...)
    T = infer_result_type(f,Any,p,"call map(function,parts...)",Int,typeof.(a)...)
    Transformation{T}((v,i) -> (f(v, i, a...)), p)
end
@deprecate instance_at(a...) map_at(a...)

export Index
Index = map_at((v,i) -> i, Always())

import Base: map
"""
    map(f::Function, p::AbstractToken, a...)

Parser matching `p`, transforming parsing results (`x`) with function `f(x,a...)`.

See also: [`map_at`](@ref), [`Transformation`](@ref)
"""
function Base.map(f::Function, p::AbstractToken, a...)
    T = infer_result_type(f,Any,p,"call seq(function,type,parts...)",typeof.(a)...)
    Transformation{T}((v,i) -> (f(v, a...)), p)
end

"""
    map(T::Type, p::AbstractToken, a...)

Parser matching `p`, transforming `p`s parsing result with constructor `T(x,a...)`.

See also: [`map_at`](@ref) [`get`](@ref), [`Transformation`](@ref)
"""
function Base.map(Tc::Type, p::AbstractToken, a...)
    Transformation{Tc}((v,i) -> Tc(a..., v), p)
end

"""
    map(index::IndexAt, p::AbstractToken, a...)
    map(constant, p::AbstractToken, a...)

Parser matching `p`, transforming `p`s parsing results to `getindex(x,index)` or `constant`.

See also: [`map_at`](@ref) [`get`](@ref), [`Transformation`](@ref)

"""
function Base.map(index::IndexAt{<:Integer}, p::AbstractToken)
    T=result_type(p)    
    Transformation{fieldtype(T,index.i)}(index, p)
end
function Base.map(index::IndexAt{<:UnitRange}, p::AbstractToken)
    T=Tuple{fieldtypes(result_type(p))[index.i]...}
    Transformation{T}(index, p)
end
##instance(Tc::Type, p::ParserTypes, a...)
##    instance(Tc::Type, p::ParserTypes)
function instance(Tc::Type, p::ParserTypes, a...)
    Transformation{Tc}((v,i) -> Tc(a..., v), p)
end
function instance(Tc::Type, p::ParserTypes)
    Transformation{Tc}((v,i) -> convert(Tc,v), p)
end
function Base.map(f::Function, Tc::Type, p::AbstractToken, a...)
    T = infer_result_type(f,Tc,p,"call seq(function,type,parts...)",typeof.(a)...)
    Transformation{Tc}((v,i) -> (f(v, a...)), p)
end
Base.map(f::typeof(identity), p::AbstractToken) = p


@deprecate map(T::Type, f::Function, p::AbstractToken, a...) map(f,T,p,a...)
@deprecate instance(f::Function,p,a...) map(f,parser(p),a...)

