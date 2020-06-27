import CombinedParsers: ParserTypes
using CombinedParsers.Regexp

import CombinedParsers.Regexp: char, integer_base, escape_sequence, escaped_character
@testset "char" begin
    ##@test match(parse(char,with_options(Base.PCRE.CASELESS,"A")) =='a'
    @test parse(char,"A") == CharIn('A')
    @test parse(char,"\\^") == CharIn('^')
    @test tryparse(char,"^") === nothing
    @test parse(escape_sequence(),raw"\Q[].\E")=="[]."
    ##@btime _iterate(pattern,".")
    ## @btime _iterate(pattern,"\\N")
    @test parse(integer_base(8),"765")==501
    @test parse(integer_base(10),"765")==765
    @test parse(integer_base(16),"765")==1893
    @test parse(integer_base(16),"")==0
    @test parse(Sequence(v->Char(v[2]),"\\x{",integer_base(16),"}"),"\\x{10}") == '\x10'
    @test parse(Repeat(escaped_character),raw"\a\t\r\n") == collect("\a\t\r\n")
    @test parse(parser(parse(escaped_character,"\\o{100}")),"@")=='@'
    @test parse(parser(parse(escaped_character,"\\x10")),"\U0010")=='\x10'
    @test parse(parser(parse(escaped_character,"\\x{010}")),"\U0010")=='\x10'
    @test parse(escaped_character,"\\t")==('\t')
    @test parse(escaped_character,"\\x{0065}") == ('e')
    @test parse(integer_base(8,3,3),"100")==64
    match(r"\100","@")
    match(r"\x100","@")
    match(r"\o{100}","@")
    match(r"\x1","\U0010")
end

import CombinedParsers.Regexp: @test_pcre
@testset "char groups" begin
    @test_pcre "\\d+" "1123"
    @test_pcre "\\D+" "abcd"
    @test_pcre "\\R" "\n"
    @test_pcre "\\W" " "
end

import CombinedParsers.Regexp: bracket_char, bracket, pcre_options
@testset "brackets" begin
    @test parse(bracket_char,"a")=='a'
    @test parse(bracket,"[a-z]")==CharIn('a':'z')
    @test parse(bracket,with_options(Base.PCRE.EXTENDED_MORE,"[a-z ]"))==CharIn('a':'z')
    @test parse(bracket,"[\x3f-\x5F]")==CharIn('?':'_')
    @test parse(parser(']'),"]")==']'
    @test parse(parse(bracket,"[]abc]"),"]")==']'
    @test parse(parse(bracket,"[]-_abc-]"),"]")==']'
    @test match(re"[-]abc]","a") === nothing
    @test parse(re"[]abc-]","-")=='-'
    @test parse(re" [a- z]"xxx,"b")=='b'
    @test tryparse(re" [a- z]"xx,"b")==nothing
    @test tryparse(re"[a- z]","b")==nothing
    @test parse(re"[a- z]"xx,"b")=='b'
    ##@test match(r" [a-z]"xxx,"b")==match(re" [a-z]"xxx,"b")
    @test result_type(bracket) == CombinedParser
    @test parse(bracket,"[a\\E]") == CharIn('a')
    ##parse(bracket,with_options("i","[^]a]"))
    @test_throws ArgumentError Regcomb("[")
end



parse(re"\v(?#ss=)","\n")




import CombinedParsers.Regexp: option_sequences,skip_whitespace_on, ParserWithCaptures
@testset "internal options" begin
    @test match(r"(?xx)[a- x]","b")==match(re"(?xx)[a- x]","b")    
    @test parse(pcre_options,"i")==Base.PCRE.CASELESS
    @test parse(parse(option_sequences,"a|b(?i)a"),"bA")==('b','A')
    @test re" a"x == CharIn('a')
   
    
@testset "comments" begin
    ##@test parse(comment_par,"(?# comment)") == with_log("comment",Always())
    @test match(re"(?# comment)a","a").match == "a"
    ## match(re"(?# comment)a","a").match == "a"
    
    ## parse(seq(parse(skip_whitespace_and_comments,with_options("x"," (?#xxx) (?#yyy) "))...),"a")
## push!(pattern,lazy(opt(comment_par)));

## parse_all(lazy(opt(comment_par)),"(?#a)")

end

end

import CombinedParsers: Repeat_max
import CombinedParsers.Regexp: quantified, repetition, sequence
@testset "repetitions and optional" begin
    @test parse(Optional('a'),"b")===missing
    @test parse(Optional('a'),"a")==='a'
    @test parse(repetition,"{1,}") == (1,Repeat_max)
    @test parse(repetition,"*") == (0,Repeat_max)
    @test parse(repetition,"+") == (1,Repeat_max)
    @test parse(repetition,"{3}") == (3,3)
    @test parse(repetition,"?") == (0,1)
    @test match(parse(quantified,"a*"),"aaab").match=="aaa"
    @test parse(parse(quantified,"a*?"),"aa")==Char[]
    ## lazy support
    @test_pcre "a*?(abc)?" "abc"
    ## lazy rep
    @test_pcre "a(?:b|(c|e){1,2}?|d)+?(.)" "ace"
    @test parse(parse(quantified,"a*"),"aa")==Char['a','a']
    @test parse(quantified,"a?")|>regex_string == "a?"
    parse(quantified,"a{3}")
    parse(quantified,with_options(Base.PCRE.EXTENDED,"a {3}"))
    parse(parse(sequence,"ab*"), "abbb")
    pp=parse(sequence,"ab*")
    ##@btime _iterate(pp, "abbb")
    ##@btime match(r"ab*","abbb")
    @test parse(quantified,with_options(Base.PCRE.CASELESS,"a*?")) == Lazy(Repeat(CharIn('a','A')))
end



import CombinedParsers.Regexp: alternation
@testset "alternations" begin
    parse(alternation,"[-]abc\\]")
    parse(alternation,"a|b")
    parse(alternation,
          with_options(Base.PCRE.EXTENDED|Base.PCRE.EXTENDED_MORE,
                       "a {3}bc | d | [a - e]# comment?"))
    @test parse(Sequence(2,AtStart(),alternation,AtEnd()),"")==Sequence()
end

import CombinedParsers.Regexp: captured, subpattern, atomic_group, backreference, subroutine
@testset "sequences, captures" begin
    match(parse(captured,"(ab)"),"ab")
    parse(captured,"()")    
    pp = parse(captured,"(?<a>ab)")
    parse(parse(captured,"(ab)"), "ab")
    parse(parse(subpattern,"(?:ab)"), "ab")
    @test parse(atomic_group,"(?>ab*)")|>regex_string == "(?>ab*)"
    @test parse(backreference,"(?P=ab)").name == :ab
    @test parse(backreference,"\\1").index == 1
    @test parse(backreference,"\\g-1").index == -1
    @test parse(subroutine,"(?-1)").delta == Symbol("-")
    @test_pcre "((((a)(b))))(c)" "abc" true
    @test_pcre "(a)|(d)" "a" true
    @test_pcre "(a)|(d)" "d" true
    @test_pcre "(1a(2b?)*)*0" "1a1a21a2b22b0" true
    @test_pcre "(1a(2b?)*)*0" "1a1a21a2b22b0" true
    @test_pcre "(ab)*c" "ababc" true
    @test_pcre "^(ab)*c\$" "ababc" true
    @test_pcre "(?<ab>foo)\\1" "foofoo"
    re"(?<ab>foo)(?P=ab)" 
    @test_pcre "(?<ab>foo)(?P=ab)" "foofoo" true
    re"(?<ab>foo|bar)(?1)"
    @test_pcre "(?<ab>foo|bar)(?1)" "foobar" true
    @test_pcre "(ab)(?<ab>foo)\\g-1" "abfoofoo" true
    ## recursive back references
    @test_pcre "(a|b\\1)+" "aaa" true
    @test_pcre "(a|b\\1)+" "aba" true
    @test_pcre "(a|b\\1)+" "ababbaa" true
    @test_pcre "(ab)(?<ab>foo)\\g-1\\g-2" "abfoofooab" true
    @test_pcre "(se|respo)nse and ((?1)nse)" "sense and response" true
    re"(?<a>ab)(?#comment)"
    @test_pcre "^ab*(?<ab>c)" "ac"
    @test_pcre raw"\Q \Ea" " a" true "x"
    @test_pcre "a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz" "abcxyzpqrrrabbxyyyypqAzz" true
end                 



@testset "look around" begin
    @test_pcre "ab*(?<=ab)c" "abc" true
end

@test_pcre "(ab|a|b)+c" "abbabc" true

@test regex_string(AtEnd())=="\$"

@test_pcre "^(a|b|abc)+c\$" "abcbabc"


@test_pcre "^aaa(?<!c)b" "aaab"

m = match(PositiveLookahead("a"), "aaab")
@test m.start == 1
@test m.stop == 1
@test m.state == CombinedParsers.MatchState()
@test get(m) == "a"
