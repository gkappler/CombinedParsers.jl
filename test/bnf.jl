using CombinedParsers
using CombinedParsers.BNF
using CombinedParsers.Regexp
using Test

@testset "BNF" begin
    @test CombinedParsers.BNF.meta_identifier("abc") == :abc
    @test CombinedParsers.BNF.special_sequence("? ASCII char 32 ?") == "ASCII char 32"
    @test CombinedParsers.BNF.syntactic_primary("\"1b\"") == parser("1b")
    @test CombinedParsers.BNF.syntactic_factor("\"1b\"") == parser("1b")
    @test CombinedParsers.BNF.definitions_list("3 * \"aa\", \"B\"",log=true) == Repeat(3,3,"aa") * "B"
    @test parse(CombinedParsers.BNF.syntax_rule,"twelve = \"1\", \"2\" ;") == with_name(:twelve, Sequence("1","2"))
    @test CombinedParsers.BNF.definitions_list("digit excluding zero, { digit }") == Sequence(substitute(Symbol("digit excluding zero")),
                                                                                              Repeat(substitute(:digit)))
    
    @test CombinedParsers.BNF.definitions_list("\"0\" | [ \"-\" ], natural number") == Either("0", Optional("-") * substitute("natural number"))

    bnf = parse(ebnf,
        """
        digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
        digit                = "0" | digit excluding zero ;
        natural number       = digit excluding zero, { digit } ;
        integer              = "0" | [ "-" ], natural number ;
        """)

    @test bnf[:integer]("120") == ("", ("1", [ "2", "0" ]))
    @test deepmap(MatchedSubSequence, bnf, :integer)[:integer]("120") == "120"

    @test ebnf("""
    twelve a                          = "1", "2" ;
    """, log=true) isa NamedParser

    @test ebnf("""
    twelve                          = "1", "2" ; one hundred twelve = "1", twelve;
    """, log=true) isa Either

    @test ebnf("""
    twelve                          = "1", "2" ;    two hundredone                 = "2", "0", "1" ;
    """, log=true) isa Either

end
