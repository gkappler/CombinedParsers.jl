using CombinedParsers
using Test

@testset "CombinedParsers.jl" begin
    include("test-parser.jl")
    include("test-re.jl")
    include("pcretest-parser.jl")
end

import TextParse
import TextParse: Numeric
import Dates
@testset "TextParse" begin
    @test parser(Numeric(Int)) isa CombinedParser
    @test integer_base(10)("10:")==10 ## once a bug with overflowing '9'+1
    @test !(TextParse.Numeric(Int) isa CombinedParser)
    @test parse(("some " * Numeric(Int))[2], "some 100") == 100
    Dates.DateFormat.(("y-m-d","d.m.y"))
    CombinedParsers.DateParser("y-m-d","d.m.y")
    schedule = ("on " * CombinedParsers.DateParser("y-m-d","d.m.y"))[2]
    @test parse(schedule, "on 12.12.2020") == Dates.Date(2020,12,12)
    @test parse(schedule, "on 2020-12-12") == Dates.Date(2020,12,12)
end



