using CombinedParsers
using Test

@testset "CombinedParsers.jl" begin
    include("test-parser.jl")
    include("test-re.jl")
    include("pcretest-parser.jl")
end



