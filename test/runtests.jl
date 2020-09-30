using CombinedParsers
using Test

@testset "CombinedParsers.jl" begin
    include("test-reverse.jl")
    include("test-parser.jl")
    include("test-re.jl")
    include("pcretest-parser.jl")
end



