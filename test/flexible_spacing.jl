using Test
using CombinedParsers
using CombinedParsers: _tokenize_code

@testset "Flexible Whitespace Trie Parser" begin
    
    @testset "1. Core Tokenization (_tokenize_code)" begin
        # Basic assignment
        @test _tokenize_code("a = 1") == ["a", "=", "1"]
        # Punctuation separation without spaces
        @test _tokenize_code("foo(bar,baz)") == ["foo", "(", "bar", ",", "baz", ")"]
        # Multi-char operators break into single chars (mathematically safe for Trie)
        @test _tokenize_code("x == y") == ["x", "=", "=", "y"]
        # Trailing/leading whitespace is dropped
        @test _tokenize_code("  \n return x \t ") == ["return", "x"]
        # Unicode support (isletter handles unicode natively)
        @test _tokenize_code("α_1 + β") == ["α_1", "+", "β"]
    end

    @testset "2. Whitespace Fuzzing & Matching" begin
        # The baseline snippet
        orig_code = "foo(bar, baz)"
        parser = FlexibleTokens(orig_code)

        # Exact match
        @test match(parser, "foo(bar, baz)").match == "foo(bar, baz)"
        
        # Compressed whitespace
        @test match(parser, "foo(bar,baz)").match == "foo(bar,baz)"
        
        # Expanded / newline whitespace (Hallucination simulation)
        hallucinated_target = "foo( \n\t bar  ,\nbaz  )"
        @test match(parser, hallucinated_target).match == hallucinated_target

        # Partial match failure (should return nothing)
        @test match(parser, "foo(bar, qux)") === nothing
    end

    @testset "3. Multi-Snippet Trie Capability" begin
        # The true power of the Trie: searching multiple snippets simultaneously in O(1) string length time
        snippets = [
            "x = 1",
            "y = 2",
            "z = 3"
        ]
        parser = FlexibleTokens(snippets)

        @test match(parser, "  x=1  ").match == "x=1"
        @test match(parser, "y   = \n 2").match == "y   = \n 2"
        @test match(parser, "z=3").match == "z=3"
        @test match(parser, "a = 4") === nothing
    end

    @testset "4. Native replace() Integration" begin
        # This tests the "Causal Sieve" replacing logic directly
        
        source_file = """
        function calculate()
            let x = 10
                y = foo( a,b )
                return x + y
            end
        end
        """

        # 4a. Replace with whitespace variances
        snippet_to_find = "y = foo(a, b)"
        new_code = "y = bar(a, b, c)"
        
        patched_file = replace(source_file, FlexibleTokens(snippet_to_find) => new_code)
        
        @test occursin(new_code, patched_file)
        @test !occursin("foo", patched_file)
        
        # 4b. Ensure surrounding text remains perfectly untouched
        @test occursin("let x = 10\n", patched_file)
        @test occursin("\n        return x + y", patched_file)
    end
    
    @testset "5. Edge Cases" begin
        # Empty string handling
        @test _tokenize_code("") == String[]
        @test match(FlexibleTokens(""), "anything") === nothing || match(FlexibleTokens(""), "anything").match == ""
        
        # Single token hallucination
        @test match(FlexibleTokens("return"), "   return   ").match == "return"
    end
end
