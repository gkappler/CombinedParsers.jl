##cd(joinpath(dirname(pathof(CombinedParsers)),"../docs/"))
push!(LOAD_PATH,"../src")
using Pkg
Pkg.activate(".")
using CombinedParsers
using CombinedParsers.Regexp
using BenchmarkTools

testpath = joinpath("..","test")

@info "defining pcre testset parser"
include(joinpath(testpath,"pcretest-parser.jl"))

testfile = joinpath(testpath,"testoutput1")
@info "loading pcre testset $testfile"
tests_string=read(testfile,String);
tests = parse(tests_parser, tests_string)[1];


suite = BenchmarkGroup()
i,tt=1,tests[59]
p = tt[2].pattern
s = tt[2].test[1].sequence
for (i,tt) in enumerate(tests)
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
        catch
            nothing
        end
        pcre=try
            bs[["Regex","create"]] = @benchmarkable Regex($p...)
            Regex(p...)
        catch
            nothing
        end
        ##(j,test_seq) = first( enumerate(ts.test))
        for (j,test_seq) in enumerate(ts.test)
            s = test_seq.sequence
            pc != nothing && try
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
            pc != nothing && try
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



