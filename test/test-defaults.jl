@testset "char" begin
    @test parse(CombinedParsers.word_char(),with_options(Base.PCRE.CASELESS,"a")) =='a'
    @test parse(CombinedParsers.Regexp.char(),"A") == CombinedParsers.ConstantParser('A')
    @test parse(CombinedParsers.Regexp.char(),"\\^") == CombinedParsers.ConstantParser('^')
    @test parse(CombinedParsers.Regexp.pcre_parser(),"^") == AtStart()
    @test tryparse(char,"^") === nothing
end
@testset "numbers" begin

    # ##@btime _iterate(pattern,".")
    # ## @btime _iterate(pattern,"\\N")
    # @test parse(integer_base(8),"765")==501
    # @test parse(integer_base(10),"765")==765
    # @test parse(integer_base(16),"765")==1893
    # @test tryparse(integer_base(16),"")===nothing
    # @test parse(Sequence(v->Char(v[2]),"\\x{",integer_base(16),"}"),"\\x{10}") == '\x10'
    # @test parse(Repeat(escaped_character),raw"\a\t\r\n") == collect("\a\t\r\n")
    # @test parse(parser(parse(escaped_character,"\\o{100}")),"@")=='@'
    # @test parse(parser(parse(escaped_character,"\\x10")),"\U0010")=='\x10'
    # @test parse(parser(parse(escaped_character,"\\x{010}")),"\U0010")=='\x10'
    # @test parse(escaped_character,"\\t")==('\t')
    # @test parse(escaped_character,"\\x{0065}") == ('e')
    # @test parse(integer_base(8,3,3),"100")==64
end
