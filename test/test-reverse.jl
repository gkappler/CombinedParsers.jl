
@testset "Reverse" begin
    s = "abc"
    rs = CombinedParsers.Reverse(s)
    for n in 1:5
        i = 0
        @test_throws BoundsError prevind(s,i,n)
        @test_throws BoundsError prevind(rs,i,n)
        for i in 1:4
            @test prevind(s,i,n) == prevind(rs,i,n)
        end
        for i in 0:3
            @test nextind(s,i,n) == nextind(rs,i,n)
        end
        i = 5
        @test_throws BoundsError nextind(s,i,n)
        @test_throws BoundsError nextind(rs,i,n)
    end
end
