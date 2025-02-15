"""
Parsers that do not consume any input can inherit `Assertion`.
!!! note
    TODO: allow to keep state and return wrapped get
"""
abstract type Assertion <: CombinedParser end
@inline state_type(::Type{<:Assertion}) =
    MatchState
@inline _leftof(str,i,parser::Assertion,x...) = i
@inline _rightof(str,i,parser::Assertion,x...) = i
result_type(x::Assertion, sequence) = typeof(x)

"""
    Base.get(parser::Assertion{MatchState, <:Assertion}, sequence, till, after, i, state)

Most assertions return the assertion parser as a result 
([`AtStart`](@ref), [`AtEnd`](@ref),  
[`Always`](@ref), [`Never`](@ref), 
[`NegativeLookahead`](@ref), [`NegativeLookbehind`](@ref)).
"""
Base.get(parser::Assertion, sequence, till, after, i, state) =
    parser

export AtStart, AtEnd
"""
    AtStart()

Parser succeding if and only if at index 1 with `result_type` `AtStart`.

```jldoctest
julia> AtStart()
re"^"

```
"""
struct AtStart <: Assertion end
iterate_state(parser::AtStart, sequence, till, posi, next_i, state::Nothing) =
    next_i == 1 ? (next_i, MatchState()) : nothing

"""
    AtEnd()

Parser succeding if and only if at last index with `result_type` `AtEnd`.

```jldoctest
julia> AtEnd()
re"\$"

```
"""
struct AtEnd <: Assertion end
iterate_state(parser::AtEnd, sequence, till, posi, next_i, state::Nothing) =
    next_i > till ? (next_i, MatchState()) : nothing

export Never
"""
    Never()

Assertion parser matching never.

```jldoctest
julia> Never()
re"(*FAIL)"

```
"""
struct Never <: Assertion end
iterate_state(x::Never,str,posi, next_i,till,state::Nothing) =
    nothing


export Always
"""
    Always()

Assertion parser matching always and not consuming any input.
Returns `Always()`.

```jldoctest
julia> Always()
re""

```
"""
struct Always <: Assertion
end
iterate_state(parser::Always, str, till, posi, next_i, s::Nothing) =
    next_i, MatchState()


@inline iterate_state(t::Union{AtStart,AtEnd,Never,Always}, str, till, posi, next_i, state::MatchState) = nothing

"""
An assertion with an inner parser, like WrappedParser interface.
"""
abstract type WrappedAssertion <: Assertion end


export PositiveLookahead
"""
    PositiveLookahead(parser)

Parser that succeeds if and only if `parser` succeeds, but consumes no input.
The match is returned.
Useful for checks like "must be followed by `parser`, but don't consume its match".

```jldoctest
julia> la=PositiveLookahead("peek")
re"(?=peek)"

julia> parse(la*AnyChar(),"peek")
("peek", 'p')

```
"""
@auto_hash_equals struct PositiveLookahead{P} <: WrappedAssertion
    parser::P
    PositiveLookahead(p_,reversed=true) =
        let p = parser(p_)
            new{typeof(p)}(p)
        end
end

@inline state_type(::Type{PositiveLookahead{P}}) where P =
    Tuple{Int,state_type(P)}

result_type(x::PositiveLookahead, sequence; kw...) =
    result_type(x.parser, sequence; kw...)

function iterate_state(t::PositiveLookahead, str, till, posi, next_i, state)
    r = iterate_state(t.parser, str, till, posi, tuple_pos(state,posi), tuple_state(state))
    if r === nothing
        nothing
    else
        next_i, r
    end
end
function Base.get(parser::PositiveLookahead, sequence, till, after, i, state)
    after_ = tuple_pos(state)
    get(parser.parser, sequence, till, after_, i, tuple_state(state))
end


export NegativeLookahead
"""
    NegativeLookahead(parser)

Parser that succeeds if and only if `parser` does not succeed, but consumes no input.
`parser` is returned as match.
Useful for checks like "must not be followed by `parser`, don't consume its match".

```jldoctest
julia> la = NegativeLookahead("peek")
re"(?!peek)"

julia> parse(la*AnyChar(),"seek")
(re"(?!peek)", 's')

```
"""
@auto_hash_equals struct NegativeLookahead{P} <: WrappedAssertion
    parser::P
    NegativeLookahead(p_,reversed=true) =
        let p = parser(p_)
            new{typeof(p)}(p)
        end
end
@inline state_type(::Type{NegativeLookahead{P}}) where P =
    MatchState
function iterate_state(t::NegativeLookahead, str, till, posi, next_i, state::Nothing)
    r = iterate_state(t.parser, str, till, posi, next_i, nothing)
    if r === nothing
        next_i,MatchState()
    else
        nothing
    end
end


@inline iterate_state(t::NegativeLookahead, str, till, posi, next_i, state::MatchState) = nothing

export Lookahead

"""
    Lookahead(does_match::Bool, p)

[`PositiveLookahead`](@ref) if `does_match==true`, 
[`NegativeLookahead`](@ref) otherwise.
"""
function Lookahead(does_match::Bool, p_)
    p = parser(p_)
    if does_match
        PositiveLookahead(p)
    else
        NegativeLookahead(p)
    end
end

@deprecate look_ahead(does_match,p) Lookahead(does_match, p)

