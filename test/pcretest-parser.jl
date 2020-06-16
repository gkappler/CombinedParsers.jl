using Test
using CombinedParsers.Regexp
import CombinedParsers.Regexp: at_linestart, whitespace_char, integer, character_base, escaped_character
import CombinedParsers.Regexp: pcre_options, with_options, parse_options, @test_pcre

charparser = Either(
    ## not handled in escaped_character, but backreference, if a capture with number (in decimal) is defined
    Sequence('\\',character_base(8,1,3)) do v
    Char(v[2])
    end,
    escaped_character,
    AnyChar())


unescaped=map(Repeat_until(
    AnyChar(), Sequence(Repeat(' '),'\n');
    wrap=JoinSubstring)) do v
        join(parse(Repeat(charparser),v))
    end;
comment_or_empty = Repeat(
    JoinSubstring(Either(Sequence(at_linestart,'#',Repeat_until(AnyChar(),'\n')),
                         Sequence(at_linestart,Repeat_until(whitespace_char,'\n')))));




@test parse(unescaped,"A\\123B\n") == "ASB"
@with_names begin
    match_test = Sequence(Repeat1(' '),
                     :sequence => unescaped,
                     :expect => Repeat(Sequence(
                         Repeat(' '),
                         :i => Either(integer,"MK"),':',
                         Repeat(' '),
                         :result => unescaped))
                     );

    testspec = Sequence(
        :pattern => after(CharIn("/'\""),Any) do s
            Repeat_until(
                AnyChar(),
                Sequence(3, NegativeLookbehind('\\'),
                         s, Repeat_until(AnyChar(),
                                         Sequence(Repeat(whitespace_char), '\n'),
                                         wrap=JoinSubstring)),
                true; wrap=JoinSubstring)
            end,
        :test => Repeat(match_test),
        :tests_nomatch => Optional(
            Sequence(2, Optional("\\= Expect no match",Repeat_until(AnyChar(), '\n'; wrap=JoinSubstring)),
                Repeat(Sequence(2,
                        Repeat1(' '),
                        unescaped,
                        Optional(Sequence("No match",
                                Repeat_until(AnyChar(), '\n'; wrap=JoinSubstring)))
                        ))))
    );
end;

tests_parser = Sequence(Repeat(Sequence(comment_or_empty,
                           testspec)),
                   comment_or_empty,
                   AtEnd());



is_expected(pc_match::Nothing,expect) = isempty(expect)
function is_expected(pc_match,expect)
    ##@show expect
    for e in expect
        r = e.result == "<unset>" ? nothing : e.result
        trimstring(r) != trimstring(if e.i==0
                                    pc_match.match
                                    elseif e.i isa Integer
                                    pc_match.captures[e.i]
                                    else ## MK unsupported
                                    r ##""
                                    end) && return false
    end
    return true
end


macro pcre_testset(tt,log=false)
    quote
        let ts = $(tt), name = ts.pattern[1]
            ## println(ts)
            p =  with_options(parse_options(ts.pattern[2]), ts.pattern[1])
            pcre=try
                Regex(p)
            catch
                nothing
            end
            ## pc  =CombinedParsers.indexed_captures(set_options(ts.pattern.flags,parse(alternation,ts.pattern)))
            pc  = try
                Regcomb(p)
            catch e
                e isa UnsupportedError && rethrow(e)
                nothing
            end
            @test pcre===nothing && isempty(ts.test) && isempty(ts.tests_nomatch) ? pc === nothing : pc !== nothing
            ## println(pc)
            ##test_seq = ts.test[1]
            if pc !== nothing 
                for (i,test_seq) in enumerate(ts.test)
                    s = test_seq.sequence
                    re_match = pcre === nothing ? nothing : match(pcre,s)
                    pc_match = match(pc,s)
                    expectations = tuple( ( e.i=>e.result for e in test_seq.expect )... )
                    if !is_expected(pc_match,test_seq.expect) || $log
                        @info "testing r\"$name\" on $i, \"$(test_seq.sequence)\"" expectations pc_match re_match
                    end
                    @test is_expected(pc_match, test_seq.expect)
                    if re_match !== nothing
                        !is_expected(re_match,test_seq.expect) && @warn "pcre failed test r\"$name\" on $i, \"$(test_seq.sequence)\"" expectations pc_match re_match
                    end
                end
                for (i,s) in enumerate(ts.tests_nomatch)
                    if $log
                        @info "testing nomatch r\"$name\" on $i, \"$s\""
                    end
                    re_match = pcre === nothing ? nothing : match(pcre,s)
                    pc_match = match(pc,s)
                    @test pc_match === nothing
                    if re_match !== nothing
                        @warn "pcre '$(name)' matches $i $s" re_match pc_match
                    end
                end
            end
        end
    end |> esc
end

macro test_pcre_str(x)
    quote
        for tt in parse(tests_parser,$x)[1]
            p =  with_options(parse_options(tt[2].pattern[2]), tt[2].pattern[1])
            @testset "$(tt[2].pattern)" begin
                @pcre_testset tt[2] true
            end
        end
    end |> esc
end


import CombinedParsers.Regexp: skip_whitespace_and_comments
@testset "test parsing" begin
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
                ).pattern[1]=="^abc\$"

    Regcomb(raw"^ (?:(?<A>A)|(?'B'B)(?<A>A)) (?('A')x) (?(<B>)y)$","x,dupnames")
    test_pcre"""
/^ (?:(?<A>A)|(?'B'B)(?<A>A)) (?('A')x) (?(<B>)y)$/x,dupnames
    Ax
 0: Ax
 1: A
    BAxy 
 0: BAxy
 1: <unset>
 2: B
 3: A
"""

    re"(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\12\123"

    
    test_pcre"""
/(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\12\123/
    abcdefghijk\12S
 0: abcdefghijk\x0aS
 1: a
 2: b
 3: c
 4: d
 5: e
 6: f
 7: g
 8: h
 9: i
10: j
11: k
"""
    
    test_pcre"""
/^$/
    
 0: 
\= Expect no match
    A non-empty line
No match
"""
           
    test_pcre"""
/^[\w][\W][\s][\S][\d][\D][\b][\n][\c]][\022]/
    a+ Z0+\x08\n\x1d\x12
 0: a+ Z0+\x08\x0a\x1d\x12
"""
    
    re"abc\0def\00pqr\000xyz\0000AB"
    test_pcre"""
/abc\0def\00pqr\000xyz\0000AB/
    abc\0def\00pqr\000xyz\0000AB
 0: abc\x00def\x00pqr\x00xyz\x000AB
    abc456 abc\0def\00pqr\000xyz\0000ABCDE
 0: abc\x00def\x00pqr\x00xyz\x000AB
"""

    let at0 = ['@','0']
        @test parse(Repeat(charparser),raw"\1000") == at0
        @test parse(Repeat(charparser),raw"\x400") == at0
        @test parse(Repeat(charparser),raw"\x40\x30") == at0
        @test parse(Repeat(charparser),raw"\100\x30") == at0
        @test parse(Repeat(charparser),raw"\100\060") == at0
        @test parse(Repeat(charparser),raw"\100\60") == at0
    end
    parse(Repeat(charparser),raw"abcdefghijk\12S")
    parse(Repeat(charparser),raw"abcdefghijk\x0aS")

    parse(Repeat(charparser),raw"abc\010de")
    parse(Repeat(charparser),raw"abc\x08de")

    @test parse(Repeat(charparser),raw"a\1d") == ['a','\1','d']
test_pcre"""/abc[\1]de/
    abc\1de
 0: abc\x01de
"""

test_pcre"""/abc[\10]de/
    abc\010de
 0: abc\x08de
"""

    test_pcre"""/abcd\t\n\r\f\a\e\071\x3b\$\\\?caxyz/
    abcd\t\n\r\f\a\e9;\$\\?caxyz
 0: abcd\x09\x0a\x0d\x0c\x07\x1b9;$\\?caxyz
"""
    
    test_pcre"""
/(abc)\1000/
    abc\x400
 0: abc@0
 1: abc
    abc\x40\x30
 0: abc@0
 1: abc
    abc\1000
 0: abc@0
 1: abc
    abc\100\x30
 0: abc@0
 1: abc
    abc\100\060
 0: abc@0
 1: abc
    abc\100\60
 0: abc@0
 1: abc
"""
    
    @test parse(Repeat(Sequence(comment_or_empty,
                        testspec)),"""
                /a(*F:X)b/
                    abc
                No match, mark = X
                """)[1][2].pattern[1]=="a(*F:X)b"
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
    @test_pcre "the quick brown fox" "What the quick brown fox"

    ## test parsing of \/ in pattern (not ending pattern)
    test_pcre"""
    /<tr([\w\W\s\d][^<>]{0,})><TD([\w\W\s\d][^<>]{0,})>([\d]{0,}\.)(.*)((<BR>([\w\W\s\d][^<>]{0,})|[\s]{0,}))<\/a><\/TD><TD([\w\W\s\d][^<>]{0,})>([\w\W\s\d][^<>]{0,})<\/TD><TD([\w\W\s\d][^<>]{0,})>([\w\W\s\d][^<>]{0,})<\/TD><\/TR>/is
      <TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>
     0: <TR BGCOLOR='#DBE9E9'><TD align=left valign=top>43.<a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)</a></TD><TD align=left valign=top>Lega lstaff.com</TD><TD align=left valign=top>CA - Statewide</TD></TR>
     1:  BGCOLOR='#DBE9E9'
     2:  align=left valign=top
     3: 43.
     4: <a href='joblist.cfm?JobID=94 6735&Keyword='>Word Processor<BR>(N-1286)
     5: 
     6: 
     7: <unset>
     8:  align=left valign=top
     9: Lega lstaff.com
    10:  align=left valign=top
    11: CA - Statewide
    """
end

@test_throws UnsupportedError test_pcre"""
/(?:(?>([ab])))+a=/aftertext
    =ba=
 0: ba=
 0+ 
 1: b
"""


##parse(quantified, with_options("x","b #c\n*"))
##parse(alt( ( with_log("$i",e;nomatch=true) for (i,e) in enumerate(pattern.options) )...), "(?-i)")


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

## parse(sequence_with_options,"(?i:abc)")




## last value in captures in rep
## @btime match(r"(?:(a.))*","abac")

# should work? see
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC16
## match(r"(?J)(?:(?<n>foo)|(?<n>bar))\k<n>","foofoo")




## parse_all(re"a*(abc)?", "abc")


