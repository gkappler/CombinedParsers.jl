
testdir = joinpath(dirname(pathof(CombinedParsers)),"../test/")
docdir = joinpath(dirname(pathof(CombinedParsers)),"../docs/src/")
include(joinpath(testdir, "pcretest-parser.jl"))

tests_string=read(joinpath(testdir,"testoutput1"),String);
tests = parse(tests_parser, tests_string)[1];

ignore_idx = Dict{Int,String}(
    14 => "",          ## conditions
    69 => "Testing a PCRE regex for PCRE regex syntax. Omitted in test because of very long compile time.",
    70 => "Testing a PCRE regex for PCRE regex syntax. Omitted in test because of very long compile time.", ## DEFINE
    135 => "",
    210 => "",
    664 => "Omitted in test because of very long compile time.", ## slooow
    706 => "Omitted in test because of very long compile time.", ## slooow
    887 => "do I misunderstand recursive backreferences https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19?",
    1139 => "todo: ignorecase in backreference",
    1146 => "case-ignoring backreferences",
    1139 => "DUPNAMES",
    1138 => "DUPNAMES",
    1173 => ""
)

optimize_idx = []




import Test: record, finish
using Test: AbstractTestSet, Result, Pass, Fail, Error
using Test: get_testset_depth, get_testset
struct CustomTestSet <: Test.AbstractTestSet
    description::AbstractString
    results::Vector
    # constructor takes a description string and options keyword arguments
    CustomTestSet(desc) = new(desc, [])
end

record(ts::CustomTestSet, child::AbstractTestSet) = push!(ts.results, child)
record(ts::CustomTestSet, res::Result) = push!(ts.results, res)
function finish(ts::CustomTestSet)
    # just record if we're not the top-level parent
    if get_testset_depth() > 0
        record(get_testset(), ts)
    end
    ts
end


unsupported=Dict{Any,Any}()
errors=Any[]
testresults = @testset CustomTestSet "pcre testset 1" begin    
    for (i,tt) in enumerate(tests)
        tr = @testset "$i" begin
            if !haskey(ignore_idx,i) && !in(i,optimize_idx)
                nam = string(tt[2].pattern)[1:min(end,20)]
                try
                    println(i)
                    @pcre_testset tt[2] false
                catch e
                    ##sleep(.1)
                    if e isa UnsupportedError
                        @warn "unsupported" e.message tt[2].pattern
                        ##readline()
                        global unsupported
                        push!(get!(()->Any[],unsupported,e.message),(i,tt[2]))
                        (results =[],)
                    else
                        @warn "error in $i" tt[2].pattern exception=e
                        ##readline()
                        global errors
                        push!(errors,(i,tt[2]))
                    end
                end
            end
        end
        if !isempty(tr.results)
            @warn "error in $i $(tt[2].pattern)"
            global err_idx,err_test=i,tt[2]
            global errors
            push!(errors,(i,tt[2]))
            ##readline()
        end
    end
end

using DataStructures
successes = SortedDict()
failures = SortedDict()
for ts in testresults.results
    i = parse(Int,ts.description)
    nsuccess = sum([ t isa Pass for t in ts.results ])
    nfailed = length(ts.results) - nsuccess
    if nfailed == 0
        successes[i] = nsuccess
    else
        successes[i] = nsuccess
        failures[i] = nfailed
    end
end

n_successes, n_failed = sum(values(successes)), sum(values(failures))
n_patterns = (
    success = length(successes),
    failed = length(failures),
    skipped = length(ignore_idx),
    unsupported = sum(length.(values(unsupported))))

io=stdout


function print_testset(io,prefix,t,testdef; pad="   ")
    pat,opt = t.pattern
    pat = replace(pad*"/"*pat, "\n" => "\n"*pad)*"/"*opt
    println(io,"$pad```\n$pat\n$pad```\n$pad$prefix\n")
    testseqs = vcat([ "`"*s.sequence*"`" for s in t.test],"`".*t.tests_nomatch .* "`, no match")
    for (j,tr) in enumerate(testdef.results)
        if (j>1)
            s = replace.(testseqs[j-1], "\n" => "\\n")
            result = tr isa Pass ? "ok" : "failed"
            println(io,pad*"- $s $result")
        end
    end
end

open(joinpath(docdir,"man","pcre-compliance.md"),"w") do io

  println(io,"""
# Compliance with the PCRE test set
The test set is downloaded from [the PCRE source repository](https://github.com/rurban/pcre/blob/master/testdata/testoutput1).
The PCRE test output is parsed with [a `CombinedParser`](https://github.com/gkappler/CombinedParsers.jl/blob/master/test/pcretest-parser.jl).

## Overview
$n_successes successful tests on $(n_patterns.success) patterns.
$n_failed failed tests on $(n_patterns.failed) patterns.

##### Performance Overview:

Points represent PCRE pattern tests. The axis represent benchmark times with 
`Regex` (x axis) and `Regcomb` (y axis), both on a log10 scale.



##### Benchmark ratios histogram:

### Unsupported

$(n_patterns.unsupported) unsupported patterns were omitted for the following reasons:
""")

    for (n,r) in sort([ s.first => length(s.second) for s in unsupported ])
        println(io,"- `$n` failed on $r")
    end

    println(io,"""

### Skipped
$(n_patterns.skipped) patterns were skipped for the following reasons:
""")
    for (i,r) in ignore_idx
        t = tests[i][2]
        print_testset(io,"(#$i) skipped, $r.\n",t,testresults.results[i], pad="")
    end

    println(io,"""

## Failed Tests
$n_failed failed tests on $(n_patterns.failed) patterns.
""")
    
    for (i,r) in failures
        t = tests[i][2]
        print_testset(io,"(#$i) failed $r times:\n",t,testresults.results[i], pad="")
    end

    println(io,"""
## Compliance examples
$n_successes successful tests on $n_patterns.success patterns.

""")
    
    for (i,r) in successes
        t = tests[i][2]
        print_testset(io,"(#$i) succeeded $(r-1) times:\n",t,testresults.results[i],pad="")
    end

end


