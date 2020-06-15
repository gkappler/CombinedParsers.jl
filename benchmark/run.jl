##cd(joinpath(dirname(pathof(CombinedParsers)),"../docs/"))
push!(LOAD_PATH,"../src")
using Pkg
Pkg.activate(".")

testpath = joinpath("..","test")

using CombinedParsers
using CombinedParsers.Regexp
using BenchmarkTools

@info "defining pcre testset parser"
include(joinpath(testpath,"pcretest-parser.jl"))

testfile = joinpath(testpath,"testoutput1")
@info "loading pcre testset $testfile"
tests_string=read(testfile,String);
tests = parse(tests_parser, tests_string)[1];

using DataStructures
ignore_idx = SortedDict{Int,String}(
    ## 14 => "unicode escape in test parser needs to support \"\\u81\".",
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


suite = BenchmarkGroup()
i,tt=1,tests[59]
p = tt[2].pattern
s = tt[2].test[1].sequence
for (i,tt) in Iterators.take(enumerate(tests),100)
    if !haskey(ignore_idx,i)
        ts = tt[2]
        name = string(ts.pattern)
        @info "setting up $i: $name"
        nam = string(ts.pattern)[1:min(end,20)]
        bs = suite["$i"] = BenchmarkGroup([name])
        p = ts.pattern
        pc = try
            pc_ = optimize(Regcomb(p...))
            bs[["Regcomb","create"]] = @benchmarkable Regcomb($p...)
            pc_
        catch e
            @warn "ignore Regcomb $i" exception=e
            nothing
        end
        pcre = try
            pc_ = Regex(p...)
            bs[["Regex","create"]] = @benchmarkable Regex($p...)
            pc_
        catch
            nothing
        end
        ##(j,test_seq) = first( enumerate(ts.test))
        for (j,test_seq) in enumerate(ts.test)
            s = test_seq.sequence
            pcre != nothing && try
                bs[["Regex","match", "$j"]] = @benchmarkable match($pcre,$s)
            catch
                @warn "ignore Regex $i, $j" s
            end
            pc != nothing && try
                s_ = CombinedParsers.Regexp.SequenceWithCaptures(s,pc) 
                _iterate(pc,s_)
                bs[["Regcomb","match", "$j"]] = @benchmarkable _iterate($pc,$s_)
            catch
                @warn "ignore Regcomb $i, $j" s
            end
        end
        for (j,s) in enumerate(ts.tests_nomatch)
            pcre != nothing && try
                bs[["Regex", "nomatch", "$j"]] = @benchmarkable match($pcre,$s)
            catch
                @warn "ignore Regex $i, $j" s
            end
            pc != nothing && try
                s_ = CombinedParsers.Regexp.SequenceWithCaptures(s,pc) 
                _iterate(pc,s_)
                bs[["Regcomb", "nomatch", "$j"]] = @benchmarkable _iterate($pc,$s_)
            catch
                @warn "ignore Regcomb $i, $j" s
            end
        end
    end
end

@info "tune!"
paramsfile = "benchmark-params.json"
if isfile(paramsfile)
    loadparams!(suite, BenchmarkTools.load(paramsfile)[1],:evals,:samples)
else
    tune!(suite, verbose=true)
    BenchmarkTools.save(paramsfile,params(suite))
end

results = run(suite, verbose=true,seconds=1)

using StructArrays, DataFrames
StructArray([ (pattern=t[1][1], string = t[1][4], code=t[1][2], trial = t[2])
              for t in leaves(median(results))
              if t[1][3]=="match" ]) |> DataFrame

import Dates
datetimenow = Dates.format(Dates.now(),"Y-mm-dd_HHhMM")
resultfile = "benchmark-$datetimenow.json"
BenchmarkTools.save(joinpath(dirname(pathof(CombinedParsers)),"..","benchmark",resultfile),results)



