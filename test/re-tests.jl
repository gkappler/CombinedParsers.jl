cd("/home/gregor/dev/julia")
using Pkg
Pkg.activate("CombinedParsers")
using CombinedParsers
import CombinedParsers: ParserTypes
using CombinedParsers.Regexp
using BenchmarkTools
using Test


include("/home/gregor/dev/julia/ParserAlchemy/test/pcretest-parser.jl");

parse(log_names(pcre_parser),"(?:[a-z])(b)*\\1")


@testset "test parsing" begin
    parse(pcre_parser,"(a)")
    @test parse(integer,"09")==9
    @test parse(comment_or_empty,
                "# This set of tests is for features that are compatible with all versions of\n\n \t \n") ==
    ["# This set of tests is for features that are compatible with all versions of\n","\n", " \t \n"]
    @test parse(skip_whitespace_and_comments,with_options(Base.PCRE.EXTENDED,"# some comment   \n")) == [ with_log("some comment",Always()) ]
    @test parse(skip_whitespace_and_comments,"# some comment   \n") == []
    @test parse(match_test,"""
                    abc
                 0: abc
                """) == (sequence="abc", expect = [(i=0,result="abc")])
    @test parse(match_test,"""
                    abc
                 0: \xff
                """) == (sequence="abc", expect = [(i=0,result="\xff")])
    @test parse(testspec,
                raw"""
                /^abc$/m
                    abc
                 0: abc
                    qqq\nabc
                 0: abc
                    abc\nzzz
                 0: abc
                    qqq\nabc\nzzz
                 0: abc
                """
                ).pattern.x=="^abc\$"
    @test parse(rep(seq(comment_or_empty,
                        testspec)),"""
                /a(*F:X)b/
                    abc
                No match, mark = X
                """)[1][2].pattern=="a(*F:X)b"
    test_pcre"""
            /\Aabc\z/m
                abc
             0: abc
            \= Expect no match
                abc\n   
            No match
                qqq\nabc
            No match
                abc\nzzz
            No match
                qqq\nabc\nzzz
            No match

            /(?|(abc)|(xyz))/B
               >abc<
               >xyz<
            """
end


tests_string=read("/home/gregor/dev/pcre/testdata/testoutput1",String);
tests = parse(tests_parser, tests_string);




## check specific case
err_idx = 989 
err_test=tests[1][err_idx][2];x=err_test.pattern
pp=Regcomb(x)
match(pp,err_test.test[1].sequence)
@pcre_testset err_test true





##parse(quantified, with_options("x","b #c\n*"))
##parse(alt( ( with_log("$i",e;nomatch=true) for (i,e) in enumerate(pattern.options) )...), "(?-i)")


##parse(quantified,with_options(Base.PCRE.CASELESS,"a*?"))
result_type(bracket)
parse(bracket,"[a\\E]")
##parse(bracket,with_options("i","[^]a]"))
parse(pattern,"[")


## push!(pattern,seq('\\', AnyChar()) do v; parser(v[2]); end);

## parse(skip_whitespace_on(Base.PCRE.EXTENDED),with_options("x"," a"))|>dump
## parse(seq(parse(skip_whitespace_and_comments,with_options("x"," (?#xxx) (?#yyy) "))...),"a")

## push!(pattern,lazy(opt(comment_par)));

## parse_all(lazy(opt(comment_par)),"(?#a)")


parse(skip_whitespace_on(Base.PCRE.EXTENDED),with_options("x","  "))

parse(skip_whitespace_on(0),"\v(?#ss=)")




parse(
    after(with_log("left",alt("a","ab"))) do v
    parser(v)
    end,
    "abab")
# parse(pattern,with_options(Base.PCRE.MULTILINE,"^"))


# alt(
#     ,
#     skip_comment,
#     comment_par,
#     rep("\\E") => Always())


# function f(v)
#     with_log(v[5],Always())
# end
# map(f,comment_par)
# ParserAlchemy.infer_result_type(f,Any,comment_par,"")
# parse(parse(comment_par,"(?#abc)"),"")
# Tuple{Char,Char,Char,Array{Char,1},SubString})
# result_type(comment_par)

parse(sequence_with_options,"(?i:abc)")

@testset "comments" begin
    ##@test parse(comment_par,"(?# comment)") == with_log("comment",Always())
    @test match(re"(?# comment)a","a").match == "a"
    ## match(re"(?# comment)a","a").match == "a"
end

@testset "char" begin
    ##@test match(parse(char,with_options(Base.PCRE.CASELESS,"A")) =='a'
    @test parse(char,"A") == CharIn('A')
    @test parse(char,"\\^") == CharIn('^')
    @test parse(char,"^") === nothing
    @test parse(escape_sequence(),raw"\Q[].\E")=="[]."
    ##@btime _iterate(pattern,".")
    ## @btime _iterate(pattern,"\\N")
    @test parse(character_base(8),"765")==501
    @test parse(character_base(10),"765")==765
    @test parse(character_base(16),"765")==1893
    @test parse(character_base(16),"")==0
    @test parse(seq("\\x{",character_base(16),"}",transform=v -> Char(v[2])),"\\x{10}") == '\x10'
    @test parse(rep(escaped_character),raw"\a\t\r\n") == collect("\a\t\r\n")
    @test parse(parser(parse(escaped_character,"\\o{100}")),"@")=='@'
    @test parse(parser(parse(escaped_character,"\\x10")),"\U0010")=='\x10'
    @test parse(parser(parse(escaped_character,"\\x{010}")),"\U0010")=='\x10'
    @test parse(escaped_character,"\\t")==('\t')
    @test parse(escaped_character,"\\x{0065}") == ('e')
    @test parse(character_base(8,3,3),"100")==64
    match(r"\100","@")
    match(r"\x100","@")
    match(r"\o{100}","@")
    match(r"\x1","\U0010")
end

@testset "char groups" begin
    @test_pcre "\\d+" "1123"
    @test_pcre "\\D+" "abcd"
    @test_pcre "\\R" "\n"
    @test_pcre "\\W" " "
end

match(re"(ab)","ab")

@testset "brackets" begin
    @test parse(bracket_char,"a")=='a'
    @test parse(bracket,"[a-z]")==CharIn('a':'z')
    @test parse(bracket,with_options(Base.PCRE.EXTENDED_MORE,"[a-z ]"))==CharIn('a':'z')
    @test parse(bracket,"[\x3f-\x5F]")==CharIn('?':'_')
    @test parse(parser(']'),"]")==']'
    @test parse(parse(bracket,"[]abc]"),"]")==']'
    @test parse(parse(bracket,"[]-_abc-]"),"]")==']'
    parse(alternation,"[-]abc\\]")
    @test match(re"[-]abc]","a") === nothing
    @test parse(re"[]abc-]","-")=='-'
    @test parse(re" [a- z]"xxx,"b")=='b'
    @test parse(re" [a- z]"xx,"b")==nothing
    @test parse(re"[a- z]","b")==nothing
    @test parse(re"[a- z]"xx,"b")=='b'
    ##@test match(r" [a-z]"xxx,"b")==match(re" [a-z]"xxx,"b")
end

@testset "internal options" begin
    @test match(r"(?xx)[a- x]","b")==match(re"(?xx)[a- x]","b")    
    @test parse(pcre_options,"i")==Base.PCRE.CASELESS
    @test parse(parse(option_sequences,"a|b(?i)a"),"bA")==('b','A')
end

@testset "repetitions and optional" begin
    @test parse(opt('a'),"b")===missing
    @test parse(opt('a'),"a")==='a'
    @test tokenize(repetition,"{1,}") == (1,typemax(Int))
    @test parse(repetition,"*") == (0,typemax(Int))
    @test parse(repetition,"+") == (1,typemax(Int))
    @test parse(repetition,"{3}") == (3,3)
    @test parse(repetition,"?") == (0,1)
    @test match(parse(quantified,"a*"),"aaab").match=="aaa"
    @test parse(parse(quantified,"a*?"),"aa")==Char[]

    ## lazy support
    @test_pcre "a*?(abc)?" "abc"
    ## lazy rep
    @test_pcre "a(?:b|(c|e){1,2}?|d)+?(.)" "ace"
    @test parse(parse(quantified,"a*"),"aa")==Char['a','a']
    @test tokenize(quantified,"a?")|>regex_string == "a?"
    parse(quantified,"a{3}")
    parse(quantified,with_options(Base.PCRE.EXTENDED,"a {3}"))
    parse(parse(sequence,"ab*"), "abbb")
    pp=parse(sequence,"ab*")
    ##@btime _iterate(pp, "abbb")
    ##@btime match(r"ab*","abbb")
end



@testset "alternations" begin
    parse(alternation,"a|b")
    parse(alternation,
          with_options(Base.PCRE.EXTENDED|Base.PCRE.EXTENDED_MORE,
                       "a {3}bc | d | [a - e]# comment?"))
    @test parse(seq(AtStart(),alternation,AtEnd(), transform=2),"")==seq()
    ## push!(pattern,alternation) ## stackoverflow
end

@testset "sequences, captures" begin
    match(parse(captured,"(ab)"),"ab")
    parse(captured,"()")    
    pp = parse(captured,"(?<a>ab)")
    @btime match(pp,"ab")
    parse(parse(captured,"(ab)"), "ab")
    parse(parse(subpattern,"(?:ab)"), "ab")
    parse(atomic_group,"(?>ab*)")|>regex_string
    parse(backreference,"(?P=ab)")|>dump
    parse(backreference,"\\1")|>dump
    parse(backreference,"\\g-1")|>dump
    parse(subroutine,"(?-1)")|>dump
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
    pp = seq('a',rep('b'),NegativeLookbehind(seq('b','b')),rep('b'),'c')
    parse(pp, "abbbc")
    parse(map_parser(revert,pp), "cbbba")


    pp = seq('a',rep('b'),NegativeLookbehind(seq('b')),rep('b'),'c')
    parse(pp, "abbbc")
    map_parser(revert,pp)

    @test_pcre "ab*(?<=ab)c" "abc" true

    Regcomb("ab*(?<=ab)c","")
end



## last value in captures in rep
@btime match(r"(?:(a.))*","abac")

# should work? see
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC16
match(r"(?J)(?:(?<n>foo)|(?<n>bar))\k<n>","foofoo")




parse_all(re"a*(abc)?", "abc")


@test_pcre "the quick brown fox" "What the quick brown fox"











ignore_idx = [14, 15, 32, 35, 39, 40, 69, 70,75, 81,97,98,
              101,104,112,117,118,119,120,121,129,135,140,154,156,185,186,188,189,190,191,192,
              210,238,239,240,241,242,243,244,245,246:261...,275,277:280...,
              314,315,319,323:326..., 329,392,
              430,431,435,436,482,493,494,495,
              518,531:559...,562,573:580...,583:594...,598,
              608:610...,619,627:629...,647,652,653,655:658...,660,662,663,665,666,668,669,675:681...,693:699...,
              704,709,713:717...,719:726...,728,755:756...,770,771,783,784,794:798...,
              800,804:808...,811,814:817...,828,830:839...,842:890...,
              904,910,913,914,915,918:960...,962:985...,988,
              1000:1035...,1037,1039:1085...,1091,1092,1094,
              1102:1106...,1108:1111...,1113:1118...,1120,1121,1124,1126,1132,1134:1140...,1142,1145,1146,1148,1149,1152:1155...,1158,1159,1160,1167:1170...,1172,1173,1174,1176:1182...,1188:1232...,1236,1239,1240,1243,1246,1248:1252...
              ]
optimize_idx = [664,
                705,707]

ignore_idx = optimize_idx = [14,    ## conditions
                             69,70, ## DEFINE
                             210,
                             664, ## slooow
                             706, ## slooow
                             1173] 
unsupported=Dict{Any,Any}()
errors=Any[]
@testset "pcre testset 1" begin    
    for (i,tt) in enumerate(tests[1])
        if !in(i,ignore_idx) && !in(i,optimize_idx)
            nam = string(tt[2].pattern)[1:min(end,20)]
            tr = @testset "$i $(nam)" begin
                try
                    println(i)
                    @pcre_testset tt[2] true
                catch e
                    ##sleep(.1)
                    if e isa UnsupportedError
                        @warn "unsupported" e.message tt[2].pattern
                        ##readline()
                        global unsupported
                        push!(get!(()->Any[],unsupported,e.message),(i,tt[2]))
                        (results =[],)
                    else
                        @warn "error in $i" tt[2].pattern exception=e
                        ##readline()
                        global errors
                        push!(errors,(i,tt[2]))
                    end
                end
            end
            if !isempty(tr.results)
                @warn "error in $i $(regex_string(tt[2].pattern))"
                global err_idx,err_test=i,tt[2]
                global errors
                push!(errors,(i,tt[2]))
                ##readline()
            end
        end
    end
end



@test_pcre "(|\\1xxx)+" "xxx" true
@test_pcre "(\\1xxx|)+" "xxx" true
@test_pcre "(?P<abn>\\g{abn}xxx|)+" "xxx" true

re"(\1xxx|)+"

issues = Dict(
    887 => "do I misunderstand recursive backreferences https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19?",
    1146 => "case-ignoring backreferences",
    1139 => "DUPNAMES",
    1138 => "DUPNAMES",
    579:580 => "internal option s does not carry over end of subexpressions as e.g. i"
)

[ s.first => length(s.second) for s in unsupported ]
##(err_idx,err_test) = pop!(unsupported[ "\\B" ])

(err_idx,err_test) = pop!(errors);
x=err_test.pattern
err_idx = 1148
err_test=tests[1][err_idx][2];x=err_test.pattern
@pcre_testset err_test true


x
pp=Regcomb(x)
Regex(x)

subtest_idx=1
s = err_test.test[subtest_idx].sequence
err_test.test[subtest_idx].expect

mm=match(log_names(pp),s)
parse(pp,s)
getfield(mm,1).captures
match(Regex(x),s)
ParserAlchemy.flagstring(x.flags)
using StringEncodings

encode("-","ASCII")

decode(collect(UInt8(0x2d):UInt8(0x2f)),"ASCII")

collect('+':'\U002f')

s = err_test.tests_nomatch[subtest_idx]

_iterate(pp,s)
parse(pp,s)
mm.match|>lastindex
getfield(mm,1).captures


tests[1][6][2].test

i=1245 ## capture backref
i=525 ## capture backref
570
572
826
827
903
1163
1241
1242
i=1247

i=6
tt=tests[1][i]
ts=tt[2]
ttt=@pcre_testset tt[2] true
pp=Regcomb(ts.pattern)
match(pp,"abc\n ")
pp=Regex(ts.pattern)
match(pp,"abc\n ")

ttt=@testset "pcre testset 1" begin
    @test true
    @test false
end
ttt |> dump

err_idx,=i,tt
readline()
end


# I would like to do a short survey, can you please raise your hand if you
#     - have written a regex in your life?
#     If your hand is not up, you will know how to do that after this presentation.
#         Keep your hands up if you have ever rolled your own domain specific language with a parser generator or custom parser.
#     If your hand is not up, you will know how to do that after this presentation.
#         Please show your hand if you have been
#             if you delegate writing parsers.
#     If your hand is not up, you might even prefer to do write one your own in the future.

using PyCall

re_tests_py=read("CombinedParsers/test/re_tests.py",String)
re_tests = pyeval(re_tests_py,SUCCEED=1,FAIL=2,SYNTAX_ERROR=3);
length(re_tests)
e = re_tests[188]
using Test

R=@testset "python regex" begin
    for (ei,e) in enumerate(re_tests)
        ## @testset "$ei $(e[1])"
        begin
            if e[3] == 3
                @test_throws Exception Regex(e[1])
            elseif e[3] == 2
                @test match(Regex(e[1]),e[2]) === nothing
            elseif e[3] == 1
                m = match(Regex(e[1]),e[2])
                if m!==nothing
                    try
                        names = Base.PCRE.capture_names(m.regex.regex)
                        combined = pyeval(e[4];
                                          found=m.match,
                                          (Symbol("g$i") => (c===nothing ? "None" : c)
                                           for (i,c) in enumerate(m.captures))...,
                                          (Symbol(names[i]) => (c===nothing ? "None" : c)
                                           for (i,c) in enumerate(m.captures)
                                           if haskey(names,i))...)
                        @test combined==e[5]
                    catch ex
                        @test e[1] == e[2]
                    end
                else
                    @test e[1] == e[2]
                end
            end
        end
    end
end

