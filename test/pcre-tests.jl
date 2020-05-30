
testdir = joinpath(dirname(pathof(CombinedParsers)),"../test/")
docdir = joinpath(dirname(pathof(CombinedParsers)),"../docs/src/")
include(joinpath(testdir, "pcretest-parser.jl"))

tests_string=read(joinpath(testdir,"testoutput1"),String);
tests = parse(tests_parser, tests_string)[1];

tt=tests[14]
@pcre_testset tt[2] true

ignore_idx = SortedDict{Int,String}(
    14 => "unicode escape in test parser needs to support \"\\u81\".",
    69 => "because of very long compile time. The complex pattern parses email adresses",
    70 => "because of very long compile time. The complex pattern parses email adresses",
    89 => "CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.",
    90 => "CombinedParsers brackets use unicode character ranges. Avoid such patterns for now, contributions very welcome.",
    135 => "escaped characters needs investigation.",
    664 => "because of very long matching time for `Repeat(Repeat1('a'))` when there is no match. Avoid such patterns, contributions optimizing these cases are also very welcome.", ## slooow
    887 => """do I misunderstand [recursive backreferences](https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19)?

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

""",
    104 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    260 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    261 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    562 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
    1146 => "case-ignoring backreferences still need to be done, contributions are very welcome,",
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
    println(io,"\n---\n$pad```\n$pat\n$pad```\n$pad$prefix")
    testseqs = vcat([ "`"*s.sequence*"`" for s in t.test],"`".*t.tests_nomatch .* "` no match")
    for (j,tr) in enumerate(testdef.results)
        if (j>1)
            s = replace.(testseqs[j-1], "\n" => "\\n")
            result = tr isa Pass ? "☑" : "☐"
            println(io,pad*"- $result $s")
        end
    end
end

open(joinpath(docdir,"man","pcre-compliance.md"),"w") do io
  println(io,"""
# Compliance with the PCRE test set
!!! note 
    The `@re_str` supports the following PCRE features
    - ☑ fundamentals: sequences, alternations, repetitions optional, matches (`*`,`+`,`{n}`, `{min,}`, `{min,max}`, `?`)
    - ☑ escaped characters and generic character types
    - ☑ character ranges (`[]`)
    - ☑ non-capturing groups,
    - ☑ capturing groups, backreferences, subroutines (all by index, relative index and name)
    - ☑ atomic groups
    - ☑ lazy repetitions
    - ☑ conditional expressions
    - ☑ internal and pattern options setting
    - ☑ simple assertions (`\\A`, `\\z`, `\\Z`, `\\b`, `\\B`, `^`, `\$`), 
    - ☑ lookaheads and lookbehinds
    - ☑ comments
    PCRE functionality that is currently not supported:
    - ☐ capture groups in lookbehinds.
    - ☐ ACCEPT, SKIP, COMMIT, THEN, PRUNE, \\K
```@setup session
using CombinedParsers
using CombinedParsers.Regexp
```
The test set is downloaded from [the PCRE source repository](https://github.com/rurban/pcre/blob/master/testdata/testoutput1).
The PCRE test output is parsed with [a `CombinedParser`](https://github.com/gkappler/CombinedParsers.jl/blob/master/test/pcretest-parser.jl).
## Test Overview
$n_successes successful tests on $(n_patterns.success) patterns
(See [list of compliant patterns](pcre-compliance-succeeded.html)).\n
$n_failed failed tests on $(n_patterns.failed) patterns
(See [list of failed patterns](pcre-compliance-failed.html)).
### Performance Overview:
CombinedParsers is a very young package that will be optimized further, 
but already `@re_str` pure Julia regular expression parsing is competitive with `@r_str` with the PCRE C backend which has arrived at a widely optimized codebase after decades of improvements.\n
Benchmark timings for regular expression construction and matching comparing `Regex` (x axis) and `Regcomb` (y axis), both on a log10 scale:\n\n
![](log_btimes.png)\n\n
Points represent PCRE an individual benchmark. 
PCRE benchmarks have a range between 82ns to 518ns.
CombinedParsers benchmarks have a ~20-times larger range between 33ns to 140μs.
### Benchmark ratios histogram:
![](log_btime_ratio_histogram.svg)\n\n

The equal are histograms of log10-ratios of `time_Regcomb/time_Regex` shows that `CombinedParser` implementation is competitive, with speeds up to 10x faster in best cases and only rarely 100x slower in worst cases.
Worst cases are investigated for improving [in this IJulia notebook](...).

## Unsupported
$(n_patterns.unsupported) unsupported patterns were omitted for the following reasons:
""")
    for (n,r) in sort([ s.first => length(s.second) for s in unsupported ])
        println(io,"- `$n` failed on $r patterns.")
    end
    println(io,"""\n
## Skipped
$(n_patterns.skipped) patterns were skipped for the following reasons:
""")
    for (i,r) in ignore_idx
        t = tests[i][2]
        pat,opt = t.pattern
        @show lastindex(pat)
        if lastindex(pat)>100
            print(io,"\n\n(#$i) skipped, $r\n")
        else
            print_testset(io,"(#$i) skipped, $r\n",t,testresults.results[i], pad="")
        end
    end
end

open(joinpath(docdir,"man","pcre-compliance-failed.md"),"w") do io
    println(io,"""\n
# Failed PCRE Tests
$n_failed failed tests on $(n_patterns.failed) patterns.
""")
    for (i,r) in failures
        t = tests[i][2]
        print_testset(io,"(#$i) failed $r times:",t,testresults.results[i], pad="")
    end
end

open(joinpath(docdir,"man","pcre-compliance-succeeded.md"),"w") do io

    println(io,"""
# PCRE Compliance
$n_successes successful tests on $n_patterns.success patterns.

""")
    
    for (i,r) in successes
        t = tests[i][2]
        print_testset(io,"(#$i) succeeded $(r-1) times:\n",t,testresults.results[i],pad="")
    end

end


