"""
Parsers that do not consume any input can inherit `Assertion{S,T}`.
!!! note
    TODO: allow to keep state and return wrapped get
"""
abstract type Assertion{S,T} <: CombinedParser{S,T} end
@inline _leftof(str,i,parser::Assertion,x...) = i
@inline _rightof(str,i,parser::Assertion,x...) = i
@inline _iterate(t::Assertion{MatchState}, str, till, posi, next_i, state::MatchState) = nothing

"""
    Base.get(parser::Assertion{MatchState, <:Assertion}, sequence, till, after, i, state)

Most assertions return the assertion parser as a result 
([`AtStart`](@ref), [`AtEnd`](@ref),  
[`Always`](@ref), [`Never`](@ref), 
[`NegativeLookahead`](@ref), [`NegativeLookbehind`](@ref)).
"""
Base.get(parser::Assertion{MatchState, <:Assertion}, sequence, till, after, i, state) =
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
struct AtStart <: Assertion{MatchState,AtStart} end
regex_string(x::AtStart) = "^"
_iterate(parser::AtStart, sequence, till, posi, next_i, state::Nothing) =
    next_i == 1 ? (next_i, MatchState()) : nothing

print_constructor(io::IO, x::AtStart) = print(io,"AtStart")

"""
    AtEnd()

Parser succeding if and only if at last index with `result_type` `AtEnd`.

```jldoctest
julia> AtEnd()
re"\$"

```
"""
struct AtEnd <: Assertion{MatchState,AtEnd} end
regex_string(x::AtEnd) = "\$"
_iterate(parser::AtEnd, sequence, till, posi, next_i, state::Nothing) =
    next_i > till ? (next_i, MatchState()) : nothing
print_constructor(io::IO, x::AtEnd) = print(io,"AtEnd")



export Never
"""
    Never()

Assertion parser matching never.

```jldoctest
julia> Never()
re"(*FAIL)"

```
"""
struct Never <: Assertion{MatchState,Never} end
regex_prefix(x::Never) = "(*"
regex_inner(x::Never) = "FAIL"
regex_suffix(x::Never) = ")"
_iterate(x::Never,str,posi, next_i,till,state::Nothing) =
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
struct Always <: Assertion{MatchState,Always}
end
Base.show(io::IO,x::Always) = print(io,"re\"\"")
children(x::Union{Never,Always}) = tuple()
regex_prefix(x::Always) = ""
regex_inner(x::Always) = ""
regex_suffix(x::Always) = ""
_iterate(parser::Always, str, till, posi, next_i, s::Nothing) =
    next_i, MatchState()
##_iterate(parser::Never, str, till, posi, next_i, s) = nothing


Base.show(io::IO, x::Union{AtStart,AtEnd,Never,Always}) =
    print(io,"re\"",regex_string(x),"\"")


"""
An assertion with an inner parser, like WrappedParser interface.
"""
abstract type WrappedAssertion{S,T} <: Assertion{S,T} end
children(x::WrappedAssertion) = (x.parser,)
regex_suffix(x::WrappedAssertion) = regex_suffix(x.parser)*")"
regex_inner(x::WrappedAssertion) = regex_string(x.parser)

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
@auto_hash_equals struct PositiveLookahead{S,T,P} <: WrappedAssertion{S,T}
    parser::P
    PositiveLookahead(p_,reversed=true) =
        let p = parser(p_)
            new{Tuple{Int,state_type(p)},result_type(p),typeof(p)}(p)
        end
end
regex_prefix(x::PositiveLookahead) = "(?="*regex_prefix(x.parser)
function _iterate(t::PositiveLookahead, str, till, posi, next_i, state)
    r = _iterate(t.parser, str, till, posi, tuple_pos(state,posi), tuple_state(state))
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
@auto_hash_equals struct NegativeLookahead{P} <: WrappedAssertion{MatchState,NegativeLookahead{P}}
    parser::P
    NegativeLookahead(p_,reversed=true) =
        let p = parser(p_)
            new{typeof(p)}(p)
        end
end
regex_prefix(x::NegativeLookahead) = "(?!"*regex_prefix(x.parser)
function _iterate(t::NegativeLookahead, str, till, posi, next_i, state::Nothing)
    r = _iterate(t.parser, str, till, posi, next_i, nothing)
    if r === nothing
        next_i,MatchState()
    else
        nothing
    end
end

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

