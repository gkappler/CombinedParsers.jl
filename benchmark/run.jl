using Pkg
Pkg.activate(joinpath(dirname(pathof(CombinedParsers)),"..","benchmark"))
using CombinedParsers
using CombinedParsers.Regexp
using BenchmarkTools

testpath = joinpath(dirname(pathof(CombinedParsers)),"..","test")

@info "defining pcre testset parser"
include(joinpath(testpath,"pcretest-parser.jl"))

testfile = joinpath(testpath,"testoutput1")
@info "loading pcre testset $testfile"
tests_string=read(testfile,String);
tests = parse(tests_parser, tests_string)[1];




ignore_idx = optimize_idx = [14,    ## conditions
                             69,70, ## DEFINE
                             210,
                             664, ## slooow
                             706, ## slooow
                             1173] 


suite = BenchmarkGroup()
i,tt=1,tests[59]
p = tt[2].pattern
s = tt[2].test[1].sequence
for (i,tt) in Iterators.take(enumerate(tests),100)
    if !in(i,ignore_idx) && !in(i,optimize_idx)
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
            pcre !== nothing && ( bs[["Regex","match", "$j"]] = @benchmarkable match($pcre,$s) )
            pc != nothing && try
                s_ = CombinedParsers.Regexp.SequenceWithCaptures(s,pc) 
                _iterate(pc,s_)
                bs[["Regcomb","match", "$j"]] = @benchmarkable _iterate($pc,$s_)
            catch
                @warn "ignore $i, $j" s
            end
        end
        for (j,s) in enumerate(ts.tests_nomatch)
            pcre !== nothing && ( bs[["Regex", "nomatch", "$j"]] = @benchmarkable match($pcre,$s) )
            pc != nothing && try
                s_ = CombinedParsers.Regexp.SequenceWithCaptures(s,pc) 
                _iterate(pc,s_)
                bs[["Regcomb", "nomatch", "$j"]] = @benchmarkable _iterate($pc,$s_)
            catch
                @warn "ignore $i, $j" s
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






suite = BenchmarkGroup()
suite["1"] = @benchmarkable [ 1+i for i in 1:10]
suite["2"] = @benchmarkable [0,2][3]

BenchmarkTools.run(suite)




p = parser("1234")
dump(p)
s = "1234"

@btime _iterate(p,s)

f(p,s,n=10000) = for i in 1:n; match(p,s); end
f_iter(p,s,n=10000) = for i in 1:n; _iterate(p,s); 1; end
pat = "a"^3
r = Regex(pat)
p = !Regcomb(pat)
pp=optimize(p) 
ppp=p.parser
s = "a"^3

using Profile
Profile.clear()
@profile f(p,s,1000000)
@profile f_iter(p,s,100000000)
@profile f_iter(ppp,s,10000000)



pp
optimize(p)


@btime CombinedParsers.Regexp.SequenceWithCaptures(s,p)

@btime match(r,s);
@btime match(p,s)
@btime _iterate(ppp,s)
@btime match(ppp,s)

using ProfileView
ProfileView.view()

c="aa"
ss = SubString(s,10,100)
@edit startswith(ss,c)

using PProf

pprof()
PProf.kill()


