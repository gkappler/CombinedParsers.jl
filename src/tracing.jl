using AbstractTrees
import AbstractTrees: children
import AbstractTrees: print_tree, printnode
@nospecialize

using CombinedParsers


export TracingStat
struct TracingStat
    successes::Dict{Tuple{Int,Int},Int}
    failures::Dict{Int,Int}
end
TracingStat() = TracingStat(Dict{Tuple{Int,Int},Int}(),Dict{Int,Int}())
last_failure(x::TracingStat) = maximum(keys(x.failures); init=0)
last_success(x::TracingStat) = maximum(keys(x.successes); init=(0,0))
last_attempt(x) = max(last_failure(x),last_success(x)[2])


function Base.merge!(x::TracingStat, y::TracingStat)
    for (k,c) in y.successes
        x.successes[k] = get(x.successes, k, 0) + c
    end
    for (k,c) in y.failures
        x.failures[k] = get(x.failures, k, 0) + c
    end
    x
end
function Base.merge!(x::TracingStat, y::Nothing)
    x
end

function Base.empty!(x::TracingStat)
    empty!(x.successes)
    empty!(x.failures)
    x
end


struct Tracer{P<:CombinedParser,S} <: WrappedParser{P}
    parser::P
    stat::S
    function Tracer(p,s)
        tracing_type = p isa ConstantParser ? ConstantParser : CombinedParser
        new{tracing_type,typeof(s)}(p, s)
    end
end
@inline state_type(::Type{<:Tracer{P,<:Any}}) where P <: CombinedParser = state_type(P)
@inline state_type(::Type{<:Tracer{CombinedParser,<:Any}}) = Any
function _deepmap_parser(f::Function,mem::AbstractDict,x::Tracer,a...;kw...)
          ## construct replacement, e.g. if P <: WrappedParser
    f(x.stat)
    x
end
last_failure(x::Tracer{<:CombinedParser,<:TracingStat}) =  last_failure(x.stat)
last_success(x::Tracer{<:CombinedParser,<:TracingStat}) =  last_success(x.stat)
last_failure(x::CombinedParser) = 0
last_success(x::CombinedParser) = (0,0)




export tracer
function tracer(stat::Type, x::CombinedParser)
    #maybe_tracer(x::Union{ NamedParser,LeafParser }) = Tracer(x, stat())
    #maybe_tracer(x) =  x
    maybe_tracer(x) =
        if x isa Union{ NamedParser,LeafParser }
            with_effect(trace_effect, x,
                        stat())
        else
            x
        end
    with_effect(trace_effect, deepmap_parser(maybe_tracer, Dict(),x),
                stat())
end
tracer(x::CombinedParser) =
    tracer(TracingStat, x)


function Base.empty!(t::CombinedParser)
    function _empty!(x::Tracer)
        Base.empty!(x.stat)
        x
    end
    _empty!(x) =  x
    deepmap_parser(_empty!, t)
end

function trace_effect(s,start,after,state,stat::TracingStat)
    if state == nothing
        stat.failures[start] = get(stat.failures, start, 0) + 1
    else
        stat.successes[(start,after)] =
            get(stat.successes,
                (start, after),
                0) + 1
    end
end
TracerTypes = Union{Tracer, Tracer{<:CombinedParser,<:TracingStat}, SideeffectParser{Tuple{TracingStat}}}

stat(x::SideeffectParser{<:Tuple{<:TracingStat}})  = x.args[1]
stat(x) = nothing
stat(x::Tracer) = x.stat

function merge!_tracing_stats(p,ch)
    outer = TracingStat()
    merge!(outer, stat(p))
    for c in ch
        if nodevalue(c) isa Tuple
            merge!(outer, nodevalue(c)[1])
        end
    end
    (outer, p), ch
end

function printnode(io::IO, x::Tuple{<:TracingStat,<:CombinedParser};kw...)
    printnode(io, x[2];kw...)
end

function tracing_stat_attempted_at_postion(pos, delta=5)
    function inrange(start::Int,stop=start)
        start-delta<=pos && pos<=stop+delta
    end
    function inrange((start,stop),)
        start-delta<=pos && pos<=stop+delta
    end
    
    c -> begin
        r = (any(inrange, keys(nodevalue(c)[1].failures)) || any(inrange, keys(nodevalue(c)[1].successes)))
        if r
        end
        r
    end
end

can_collapse(x::Tuple{<:Any, <:CombinedParser}) = true #can_collapse(x[2])

property_default(t::NamedTuple, p, d) = hasproperty(t,p) ? getproperty(t,p) : d
property_default(f::Function, t, p) = hasproperty(t,p) ? getproperty(t,p) : f()
function AbstractTrees.print_tree(io::IO, tp::TracerTypes; trace_pos = nothing,  printnode_kw = (delta = 5,),maxdepth = 20, kw...)
    tree = tree_deepmap(merge!_tracing_stats, tp)
    trace_pos = trace_pos === nothing ? last_attempt(nodevalue(tree)[1]) : trace_pos
    printstyled(io,"Parsing attempts at", color=:magenta)
    printstyled(io," [$trace_pos].\n", color=:light_red)
    tree´ = tree_deepmap(
        (p,ch) ->
            (p, collect(filter(tracing_stat_attempted_at_postion(trace_pos,property_default(printnode_kw, :delta, 5)),
                               ch))),
        tree)
    print_tree(IOContext(io, :compact => true), ChainableTree(tree´);
               printnode_kw=(pos = trace_pos, printnode_kw...), maxdepth = maxdepth,  kw...)
end



function escape_string_styled(colorf::Function, io::IO, s::AbstractString; esc=(), keep = ())
    a = Iterators.Stateful(s)
    for (i::Int,c::AbstractChar) in enumerate(a)
        if c in esc
            printstyled(io, '\\'; color=treecolor.pcre_escape)
            printstyled(io, c; color=colorf(:escaped, i, c))
        elseif c in keep
            printstyled(io, c; color=colorf(:unescaped, i, c))
        elseif isascii(c)
            if c == '\0'
                printstyled(io, Base.escape_nul(peek(a)::Union{AbstractChar,Nothing}); color=colorf(:escaped, i, c))
            elseif c == '\e'
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "e"; color=colorf(:escaped, i, c))
            elseif c == '\\'
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "\\"; color=colorf(:escaped, i, c))
            elseif '\a' <= c <= '\r'
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "abtnvfr"[Int(c)-6]; color=colorf(:escaped, i, c))
            elseif isprint(c)
                printstyled(io, c; color=colorf(:unescaped,i,c))
            else
                printstyled(io, "\\x", string(UInt32(c), base = 16, pad = 2); color=colorf(:escaped, i, c))
            end
        elseif !Base.isoverlong(c) && !Base.ismalformed(c)
            if isprint(c)
                printstyled(io, c; color=colorf(:unescaped, i, c))
            else
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                c <= '\x7f'        ? printstyled(io, "x", string(UInt32(c), base = 16, pad = 2); color=colorf(:escaped, i, c)) :
                    c <= '\uffff'      ? printstyled(io, "u", string(UInt32(c), base = 16, pad = Base.need_full_hex(peek(a)::Union{AbstractChar,Nothing}) ? 4 : 2); color=colorf(:escaped, i, c)) :
                    printstyled(io, "U", string(UInt32(c), base = 16, pad = Base.need_full_hex(peek(a)::Union{AbstractChar,Nothing}) ? 8 : 4); color=colorf(:escaped, i, c))
            end
        else # malformed or overlong
            u = bswap(reinterpret(UInt32, c)::UInt32)
            while true
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "x", string(u % UInt8, base = 16, pad = 2); color=colorf(:escaped, i, c))
                (u >>= 8) == 0 && break
            end
        end
    end
end



function print_constructor(io::IO,x::TracerTypes; sequence = nothing, pos = nothing, delta = 100, kw...)
    function colorf(style, index, char)
        effi = firsti + index -1 # effective index
        r = :none
        for (m,c) in pairs(stat(x).successes)
            if effi >= m[1] && effi < m[2]
                if pos >= m[1] && pos < m[2]
                    return treecolor.match
                else
                    r = treecolor.previous_match
                end
            else#if effi >= m[1]-delta && effi < m[2]+delta
                #r = treecolor.outside_match
            end
        end
        r != :none && return r
        for (m,c) in pairs(stat(x).failures)
            if effi == m
                if pos == m[1] 
                    return treecolor.nomatch
                else
                    r = treecolor.previous_nomatch
                end
            end
        end                
        r != :none && return r
        treecolor.outside_match
    end
    if sequence !== nothing && pos !== nothing
        firsti = max(1,prevind(sequence,pos,delta))
        lasti = if lastindex(sequence)<pos
            lastindex(sequence)
        else
            min(lastindex(sequence),nextind(sequence,pos,delta))
        end
        escape_string_styled(colorf,io, sequence[firsti:lasti]*if lastindex(sequence) == lasti
                                 "\$"
                             else
                                 ""
                             end)
        
        
    else
        printstyled(io, constructor_name(x), color=treecolor.julia_structure)
    end
end

mutable struct BenchmarkStat
    success_count::Int
    failure_count::Int
    first_time_ns::Int
    total_time_ns::Int
end
BenchmarkStat() = BenchmarkStat(0,0,0,0)

function Base.empty!(x::BenchmarkStat)
    x.success_count = 0
    x.failure_count = 0
    x.first_time_ns = 0 
    x.total_time_ns = 0
    x
end

function print_constructor(io::IO, x::Tracer{<:Any,BenchmarkStat}; kw...) 
    printstyled(io," [ ")
    printstyled(io,stat(x).success_count, color=:green)
    printstyled(io,", ")
    printstyled(io,stat(x).failure_count, color=:red)
    printstyled(io,", ")
    printstyled(io,stat(x).total_time_ns, "ns")
    printstyled(io," ]")
end


@inline Base.@propagate_inbounds function iterate_state(parser::Tracer{<:CombinedParser, BenchmarkStat}, sequence, till, posi,after,state)
    time_ns_before = Base.time_ns()
    ps = iterate_state(parser.parser, sequence, till,posi,after,state)
    time_ns_after = Base.time_ns()
    if parser.stat.failure_count + parser.stat.success_count == 0 
        parser.stat.first_time_ns = time_ns_after - time_ns_before
    else
        parser.stat.total_time_ns = parser.stat.total_time_ns + time_ns_after - time_ns_before
    end
    if ps === nothing
        parser.stat.failure_count = parser.stat.failure_count + 1
    else
        parser.stat.success_count = parser.stat.success_count + 1
    end
    ps
end

AbstractTrees.children(x::WrappedParser{<:Tracer}) =  [ x.parser ]
@specialize

