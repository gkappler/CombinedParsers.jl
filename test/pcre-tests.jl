
testdir = joinpath(dirname(pathof(CombinedParsers)),"../test/")
docdir = joinpath(dirname(pathof(CombinedParsers)),"../docs/src/")
include(joinpath(testdir, "pcretest-parser.jl"))

tests_string=read(joinpath(testdir,"testoutput1"),String);
tests = pcre_tests(tests_string)[1];

tt=tests[14]
@pcre_testset tt[2] true

ignore_idx = Dict{Int,String}(
    ## 14 => "unicode escape in test parser needs to support \"\\u81\".",
    69 => "because of very long compile time. The complex pattern parses email adresses",
    70 => "because of very long compile time. The complex pattern parses email adresses",
    89 => "CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.",
    90 => "CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.",
    135 => "escaped characters needs investigation.",
    664 => "because of very long matching time for `Repeat(Repeat1('a'))` when there is no match. Avoid such patterns, contributions optimizing these cases are also very welcome.", ## slooow
    887 => raw"""do I misunderstand [recursive backreferences](https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19)?

```@repl session
pattern = "(?P<abn>(?P=abn)xxx|)+"
match(Regex(pattern),"xxx")
match(Regcomb(pattern),"xxx")
```

IMO the pcre behaviour is confusing after considering the logic of expanding the repeat
```@repl session
pattern = "(?P<abn>(?P=abn)xxx|)((?P=abn)xxx|)"
match(Regex(pattern),"xxx")
match(Regcomb(pattern),"xxx")
```



```@repl session
parse(re"(\1xxx|)+","xxx")
match(r"(\1xxx|)+","xxx")
parse(re"(|\1xxx)+", "xxx")
parse(r"(|\1xxx)+", "xxx")
```

""",
    ## 104 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    ## 260 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    ## 261 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    ## 562 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    524 => "Captures in Lookaheads need refactoring. Low prio because instead of using backreference, `FlatMap` is recommended in `CombinedParsers.jl`.",
    527 => "Capture backtracking needs refactoring. Low prio because instead of using captures, `parse` is recommended in `CombinedParsers.jl`.",
    528 => "Capture backtracking needs refactoring. Low prio because instead of using captures, `parse` is recommended in `CombinedParsers.jl`.",
    625 => "\n!!! note\n    Lookahead in Lookbehind needs refactoring.",
    ## 1146 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    1173 => "because of very long compile time. The complex pattern parses PCRE regex syntax."
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
        nfailed = sum([ !(t isa Pass) for t in tr.results ])
        if nfailed>0
            @warn "error in $i $(tt[2].pattern)"
            global err_idx,err_test=i,tt[2]
            global errors
            push!(errors,(i,tt[2]))
            ##readline()
        end
    end
end

unsupportednos = Set(
    vcat([ first.(v) for v in values(unsupported) ]...)
)
successes = Dict()
failures = Dict()
for ts in testresults.results
    i = parse(Int,ts.description)
    nsuccess = sum([ t isa Pass for t in ts.results ])
    nfailed = length(ts.results) - nsuccess
    if nfailed == 0
        if nsuccess > 0
            successes[i] = (nsuccess, length(ts.results))
        end
    elseif !(i in unsupportednos)
        failures[i] = (nfailed,length(ts.results))
    end
end

n_successes, n_failed = sum(getindex.(values(successes),1)), sum(getindex.(values(failures),1))
n_patterns = (
    success = length(successes),
    failed = length(failures),
    skipped = length(ignore_idx),
    unsupported = sum(length.(values(unsupported))))
io=stdout

benchmarks = [
    # (range_Regcomb = (33.0, 28515.5), range_Regex = (83.0, 682.5), proportion_better = 0.5093632958801498, mean_ratio = 2.7263015758568434, ratio_mean = 1.2951982070322912)
    "benchmark-2020-06-29_10h29-4304c235feb678ca0ea7902a1a296dd2dace0ae4.json" =>
    (range_Regcomb = (34.0, 19704.0), range_Regex = (86.0, 521.0), proportion_better = 0.5917602996254682, mean_ratio = 1.2631184139535836, ratio_mean = 0.8598574284168594)
][end].second

function print_testset(io,prefix,t,testdef; pad="   ")
    pat,opt = t.pattern
    pat = replace(pad*"/"*pat, "\n" => "\n"*pad)*"/"*opt
    println(io,"\n$pad```\n$pat\n$pad```\n$pad$prefix")
    testseqs = vcat([ "`"*s.sequence*"`" for s in t.test],"`".*t.tests_nomatch .* "` no match")
    for (j,tr) in enumerate(testdef.results)
        if (j>1)
            s = Base.escape_string(testseqs[j-1])
            result = tr isa Pass ? "✅" : "❌"
            println(io,pad*"- $result $s")
        end
    end
end

open(joinpath(docdir,"man","pcre-compliance.md"),"w") do io
  println(io,"""
# Compliance with the PCRE test set

PCRE features supported by `@re_str` 
- ✅ sequences, alternations (`|`), repetitions (`*`,`+`,`{n}`, `{min,}`, `{min,max}`), optional matches (`?`)
- ✅ escaped characters and generic character types
- ✅ character ranges (`[]`)
- ✅ non-capturing groups
- ✅ capturing groups, backreferences, subroutines (all by index, relative index and name)
- ✅ simple assertions (`\\A`, `\\z`, `\\Z`, `\\b`, `\\B`, `^`, `\$`)
- ✅ lookaheads and lookbehinds
- ✅ atomic groups
- ✅ lazy repetitions
- ✅ conditional expressions
- ✅ internal and pattern options setting
- ✅ comments

PCRE functionality that is currently not supported:
- ❌ Capture groups in lookbehinds.
- ❌ Lookaheads within lookbehinds.
- ❌ ACCEPT, SKIP, COMMIT, THEN, PRUNE, \\K
```@setup session
using CombinedParsers
using CombinedParsers.Regexp
```

## PCRE Unit Tests

CombinedParsers.jl is tested and benchmarked against the PCRE C library testset.
The PCRE test output is 

- downloaded from [the PCRE source repository](https://github.com/rurban/pcre/blob/master/testdata/testoutput1), 
- parsed with [a `CombinedParser`](https://github.com/gkappler/CombinedParsers.jl/blob/master/test/pcretest-parser.jl), to 
- run tests/benchmarks on `Base.Regex` and `CombinedParsers.Regexp.Regcomb`.

(Note: tests are relaxed for some cases allowing empty captures (`""`) for unset captures (`nothing`).

$n_successes successful tests on $(n_patterns.success) patterns
(See [list of compliant patterns](pcre-compliance-succeeded.md)).

$n_failed failed tests on $(n_patterns.failed) patterns
(See [list of failed patterns](pcre-compliance-failed.md)).

$(n_patterns.unsupported) unsupported patterns were omitted for the following reasons:
    """)
    for (n,r) in sort([ s.first => length(s.second) for s in unsupported ])
        println(io,"- `$n` excluded $r patterns.")
    end
    println(io,"""


## Performance Comparison with C PCRE:

The PCRE C backend of `@r_str` has arrived at a widely optimized codebase after decades of improvements.
C PCRE2 optimized is among the fastest regex libraries ([second behind Rust](https://github.com/mariomka/regex-benchmark/tree/optimized), running [mariomka](https://github.com/mariomka)'s benchmark will position CombinedParser among its competition).

Although CombinedParsers.jl is a very young package that will be optimized further, 
`@re_str` pure Julia Regcomb is often competitive with PCRE `@r_str` Regex.


PCRE benchmarks have a range between $(benchmarks.range_Regex[1])ns to $(benchmarks.range_Regex[2])ns.
CombinedParsers benchmarks range between $(benchmarks.range_Regcomb[1])ns to $(benchmarks.range_Regcomb[2])ns.
$(round(benchmarks.proportion_better*100))% of benchmarks are faster with CombinedParsers compared to PCRE.
The average ratio of `time_Recomb/time_Regex` is $(round(benchmarks.mean_ratio,digits=2)).


These benchmarkin results are for the first 100 test patterns in the PCRE test set, comparing `match(Regex(pattern,flags),s)` with `_iterate(Regcomb(pattern,flags),s)`.
![](log_btimes.png)


Benchmark timings for regular expression construction and matching comparing `Regex` (x axis) and `Regcomb` (y axis), both on a log10 scale.
Points represent an individual benchmark for a pattern construction or match.
Cases with `CombinedParsers` being faster than the C library PCRE are paint green, slower cases are red.

Benchmark ratios histogram:
![](log_btime_ratio_histogram.svg)


The histograms of ratios of `time_Regcomb/time_Regex` on a log scale demonstrate that `CombinedParser` implementation is competitive.
Worst cases are investigated for further optimization [in this IJulia notebook](https://github.com/gkappler/CombinedParsers.jl/blob/master/benchmark/benchmarks.ipynb).

Next steps in optimization are
- caching codeunit lengths of matches for backtracking.
- memoization of sub-parsings.

""")
end

open(joinpath(docdir,"man","pcre-compliance-failed.md"),"w") do io
    println(io,"""\n
    # Failed PCRE Tests
    $n_failed failed tests on $(n_patterns.failed) patterns.
    """)
    println(io,"""\n
## Skipped
$(n_patterns.skipped) patterns were skipped for the following reasons:
""")
    for (i,r) in sort(collect(ignore_idx))
        t = tests[i][2]
        pat,opt = t.pattern
        @show lastindex(pat)
        println(io,"\n---")
        if lastindex(pat)>100
            print(io,"\n\n(no $i) skipped, $r\n")
        else
            print_testset(io,"\n(no $i) skipped, $r\n",t,testresults.results[i], pad="")
        end
    end
    println(io,"""\n
## Failed tests
""")
    for (i,(r,n)) in sort(collect(failures))
        if n>0
            t = tests[i][2]
            println(io,"\n---")
            print_testset(io,r==0 ? "\n(no $i) failed to compile" : "(no $i) failed  $(r-1) of $(n-1) times:",t,testresults.results[i], pad="")
        end
    end
end

open(joinpath(docdir,"man","pcre-compliance-succeeded.md"),"w") do io
    println(io,"""
# PCRE Compliance
$n_successes successful tests on $(n_patterns.success) patterns.\n
""")
    for (i,(r,n)) in sort(collect(successes))
        t = tests[i][2]
        println(io,"\n---")
        print_testset(io,"(no $i) succeeded $(r-1) of $(n-1) times:\n",t,testresults.results[i],pad="")
    end
end

