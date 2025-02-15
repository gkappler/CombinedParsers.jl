using AbstractTrees
import AbstractTrees: children
import AbstractTrees: print_tree, printnode
@nospecialize

using CombinedParsers
struct Tracer{P<:CombinedParser,S} <: WrappedParser{P}
    parser::P
    stat::S
    Tracer(p,s) = new{tracing_type(p),typeof(s)}(p, s)
end
tracing_type(p) = p isa ConstantParser ? ConstantParser : CombinedParser
@inline state_type(::Type{<:Tracer{P,<:Any}}) where P <: CombinedParser = state_type(P)
@inline state_type(::Type{<:Tracer{CombinedParser,<:Any}}) = Any
function _deepmap_parser(f::Function,mem::AbstractDict,x::Tracer,a...;kw...)
    f(x.stat)
    x
end

export tracer
function tracer(stat::Type, x::CombinedParser)
    maybe_tracer(x::Union{ NamedParser,LeafParser }) = Tracer(x, stat())
    maybe_tracer(x) =  x
    Tracer(deepmap_parser(maybe_tracer, x), stat())
end
tracer(x::CombinedParser) =
    tracer(TracingStat, x)


export TracingStat
struct TracingStat
    successes::Dict{Tuple{Int,Int},Int}
    failures::Dict{Int,Int}
end
TracingStat() = TracingStat(Dict{Tuple{Int,Int},Int}(),Dict{Int,Int}())
last_failure(x::TracingStat) = maximum(keys(x.failures); init=0)
last_success(x::TracingStat) = maximum(keys(x.successes); init=(0,0))
last_failure(x::Tracer{<:CombinedParser,<:TracingStat}) =  last_failure(x.stat)
last_success(x::Tracer{<:CombinedParser,<:TracingStat}) =  last_success(x.stat)
last_failure(x::CombinedParser) = 0
last_success(x::CombinedParser) = (0,0)
last_attempt(x) = max(last_failure(x),last_success(x)[2])

function Base.merge!(x::TracingStat, ys...)
    for y in ys
        for (k,c) in y.successes
            x.successes[k] = get(x.successes, k, 0) + c
        end
        for (k,c) in y.failures
            x.failures[k] = get(x.failures, k, 0) + c
        end
    end
    x
end

function Base.empty!(x::TracingStat)
    empty!(x.successes)
    empty!(x.failures)
    x
end

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

stat(x::SideeffectParser{Tuple{T}}) where T = x.args[1]
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

property_or(t::NamedTuple, p, d) = hasproperty(t,p) ? getproperty(t,p) : d
property_or(f::Function, t, p) = hasproperty(t,p) ? getproperty(t,p) : f()

function AbstractTrees.print_tree(io::IO, tp::Tracer; trace_pos = nothing,  printnode_kw = (delta = 5,),kw...)
    tree = tree_deepmap(merge!_tracing_stats, tp)
    trace_pos = trace_pos === nothing ? last_attempt(nodevalue(tree)[1]) : trace_pos
    printstyled("Parsing attempts at", color=:magenta)
    printstyled(" [$trace_pos].\n", color=:light_red)
    tree´ = tree_deepmap(
        (p,ch) ->
            (p, collect(filter(tracing_stat_attempted_at_postion(trace_pos,property_or(printnode_kw, :delta, 5)),
                               ch))),
        tree)
    print_tree(IOContext(io, :compact => true), ChainableTree(tree´);
               printnode_kw=(pos = trace_pos, printnode_kw...), kw...)
end

using StyledStrings
import StyledStrings: Face, SimpleColor
function print_constructor(io::IO,x::Tracer{<:CombinedParser,<:TracingStat}; sequence = nothing, pos = nothing, delta = 7, kw...)
    if VERSION>=v"1.11"
        if sequence !== nothing && pos !== nothing
            1
            s= Base.AnnotatedString(
                sequence,
                vcat([(firstindex(sequence):lastindex(sequence), :face => :bright_black)],
                     [(m[1]:m[2], :face => pos >= m[1] && pos <= nextind(sequence,m[2]) ? :success : :previous_success)
                      for (m,c) in pairs(x.stat.successes)
                          if pos >= m[1]-delta && pos <= nextind(sequence,m[2])+delta
                              ],
                     [ (m:m, :face => :failure)
                       for (m,c) in pairs(x.stat.failures) if pos == m    ]))
            StyledStrings.withfaces(:success=>Face(foreground=SimpleColor(0,50,0), weight = :bold, background=SimpleColor(0,200,0)),
                                    :failure=>Face(foreground=SimpleColor(50,0,0), weight = :bold, background=SimpleColor(200,0,0)),
                                    :previous_success=>Face(foreground=SimpleColor(0,25,0), background=SimpleColor(0,75,0), weight = :bold)) do
                                        print(io,
                                              s[max(1,pos-delta):min(end,pos+delta)])
                                    end
        else
        end
    else
        s,f = if sequence !== nothing && pos !== nothing
            [ (m,c)
              for (m,c) in pairs(x.stat.successes)
                  if pos >= m[1]-delta && pos <= nextind(sequence,m[2])+delta
                      ],
            [ (m,c)  for (m,c) in pairs(x.stat.failures) if pos == m    ]
        else
            [],[]
        end
        if !isempty(s)
            sort!(s)
            m,c = s[end]
            printstyled(io, "", sequence[max(1,m[1]-delta):prevind(sequence,m[1])]; color=:light_black)
            inmatch = pos <= m[2]
            printstyled(io, sequence[m[1]:m[2]]; color=inmatch ? :green : 158, underline = inmatch, bold = inmatch)
            printstyled(io, "[$(m[2])]"; color= inmatch ? :green : 158)
            isempty(f) && printstyled(io, sequence[nextind(sequence,m[2]):min(end,m[2]+delta)]; color=:magenta)
        end

        if !isempty(f)
            m,c = f[1]
            if isempty(s)
                printstyled(io, "", sequence[max(1,m-delta):prevind(sequence,m)]; color=:light_black)
            end
            if m<=lastindex(sequence)
                printstyled(io, sequence[m]; color=:light_red, underline = true, bold = true)
                isempty(s) && printstyled(io, "[$(m)]"; color=:red, )
                printstyled(io, sequence[nextind(sequence,m):min(end,m+delta)]; color=:magenta)
            else
                printstyled(io, "\$"; color=:light_red, underline = true, bold = true)
                isempty(s) && printstyled(io, "[$(m)]"; color=:red, )
                printstyled(io, " (end of string)"; color=:magenta)
            end
        end
        if isempty(s) && isempty(f)
            printstyled(io, constructor_name(x), color=treecolor.julia_structure)
        end
        printstyled(io,x.stat.successes; color = :green)
        printstyled(io,x.stat.failures; color = :magenta)
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
    printstyled(io,x.stat.success_count, color=:green)
    printstyled(io,", ")
    printstyled(io,x.stat.failure_count, color=:red)
    printstyled(io,", ")
    printstyled(io,x.stat.total_time_ns, "ns")
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

