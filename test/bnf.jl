using CombinedParsers
using CombinedParsers.BNF
using Test

@testset "BNF & EBNF Grammars" begin

    @testset "1. Terminals and Meta-Identifiers" begin
        # Meta-identifiers allow letters, digits, and spaces/dashes internally
        @test parse(CombinedParsers.BNF.meta_identifier, "my-rule_1") == Symbol("my-rule_1")
        @test parse(CombinedParsers.BNF.meta_identifier, "natural number") == Symbol("natural number")
        
        # Terminal strings support both single and double quotes
        @test parse(CombinedParsers.BNF.syntactic_primary, "'hello'")("hello") == "hello"
        @test parse(CombinedParsers.BNF.syntactic_primary, "\"world\"")("world") == "world"
        
        # Special sequences are mapped to raw Strings but resolve to Never() in structural generation
        @test parse(CombinedParsers.BNF.special_sequence, "? ASCII char 32 ?") == "ASCII char 32"
    end

    @testset "2. Sequences, Alternations, and Grouping" begin
        # Sequence: A, B
        seq = ebnf"""rule = "A", "B" ;"""
        @test seq[:rule]("AB") == ("A", "B")
        
        # Alternation: A | B
        alt = ebnf"""rule = "A" | "B" ;"""
        @test alt[:rule]("A") == "A"
        @test alt[:rule]("B") == "B"
        
        # Grouping: (A | B), C
        grp = ebnf"""rule = ("A" | "B"), "C" ;"""
        @test grp[:rule]("AC") == ("A", "C")
        @test grp[:rule]("BC") == ("B", "C")
    end

    @testset "3. Optionals and Repetitions" begin
        # Optionals: [ A ]
        opt = ebnf"""rule = [ "A" ] ;"""
        @test opt[:rule]("A") == "A"
        @test ismissing(opt[:rule]("")) # Optionals fallback to missing or default values
        
        # Zero or more: { A }
        rep = ebnf"""rule = { "A" } ;"""
        @test rep[:rule]("AAA") == ["A", "A", "A"]
        @test rep[:rule]("") == String[]
        
        # Exact repetition: 3 * "A"
        exact = ebnf"""rule = 3 * "A" ;"""
        @test exact[:rule]("AAA") == ["A", "A", "A"]
        @test_throws ArgumentError parse(exact[:rule], "AA")
    end

    @testset "4. Comments and Whitespace" begin
        # EBNF comments (* ... *) are ignored safely during parsing
        grammar = ebnf"""
        rule = "A" (* inline comment *), 
               (* multiline 
                  comment *) "B" ;
        """
        @test grammar[:rule]("AB") == ("A", "B")
    end

    @testset "5. Exceptions (-)" begin
        # Exception patterns are matched but presently parsed conditionally without active negative-lookaheads.
        grammar = ebnf"""
        rule = "A" - "B" ;
        """
        @test grammar[:rule]("A") == "A"
    end

    @testset "6. AST Transformations (deepmap)" begin
        bnf = ebnf"""
        digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
        digit                = "0" | digit excluding zero ;
        natural number       = digit excluding zero, { digit } ;
        integer              = "0" | [ "-" ], natural number ;
        """

        # Tuples natively represent the AST structure of Sequences
        @test bnf[:integer]("120") == ("", ("1", ["2", "0"]))
        
        # `deepmap` applies a mapping function (MatchedSubSequence) cleanly over the requested cyclic node
        extractor = deepmap(MatchedSubSequence, bnf, :integer)
        @test extractor[:integer]("120") == "120"
        @test extractor[:integer]("-42") == "-42"
    end

    @testset "7. Warth's Algorithm: Left-Recursion Resolution" begin
        # The parser structure is internally modified dynamically enabling robust PEG left-recursion. 
        math_grammar = ebnf"""
        expr   = expr, "+", term | term ;
        term   = term, "*", factor | factor ;
        factor = "1" | "2" | "3" ;
        """
        
        math_parser = deepmap(MatchedSubSequence, math_grammar, NamedParser)

        @test math_parser[:factor]("3") == "3"
        @test math_parser[:term]("1*2*3") == "1*2*3"
        
        # Standard Left-recursive multi-depth matching expands natively
        @test math_parser[:expr]("1+2*3+1") == "1+2*3+1"
        
        # Ensures that Warth's iteration loops securely terminate correctly on matching failures
        @test_throws ArgumentError parse(math_parser[:expr], "1+2*")
    end
end
