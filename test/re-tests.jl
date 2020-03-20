cd("/home/gregor/dev/julia")
using Pkg
Pkg.activate("ParserAlchemy")
using ParserAlchemy
import ParserAlchemy: ParserTypes
using BenchmarkTools
using Test

@test parse(rep(rep('a')),"a") == Any[Any['a']]

integer = Numeric(Int)
parse(integer,"09")==9

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC2
# affect . ^ $
# alt("(*CR)" => with_doc(CharNotIn('\r'), "carriage return"),
#     "(*LF)" => with_doc(CharNotIn('\r'), "linefeed"),
#     "(*CRLF)" => with_doc("carriage return, followed by linefeed"),
#     "(*ANYCRLF)" => with_doc("any of the three above"),
#     "(*ANY)" => with_doc("all Unicode newline sequences"))

#(*NO_AUTO_POSSESS)
#(*LIMIT_MATCH=d)
#(*LIMIT_RECURSION=d)

bsr = atomic(alt(Union{Char,String},"\r\n",CharIn('\n','\x0b','\f','\r','\U0085', '\U2028','\U2029'))); # backslash R (BSR)
whitespace = " \t\U0085\U200E\U200F\U2028\U2029"

## recursive pattern alternatives
at_linestart = alt(AtStart(),PositiveLookbehind(bsr))
lineend   = alt(AtEnd(),bsr)
at_lineend   = alt(AtEnd(),PositiveLookahead(bsr))
pattern = alt(ParserTypes,
              on_options(Base.PCRE.MULTILINE, '^' => at_linestart),
              '^' => AtStart(),
              on_options(Base.PCRE.MULTILINE, '$' => at_lineend),
              '$' => AtEnd()
              );



pcre_options = rep1(UInt32,
    alt(
        'i' => Base.PCRE.CASELESS,
        'm' => Base.PCRE.MULTILINE,
        'U' => Base.PCRE.UNGREEDY,
        'J' => Base.PCRE.DUPNAMES,
        's' => Base.PCRE.DOTALL,
        'x' => Base.PCRE.EXTENDED),
    transform=(v,i) -> |(v...))

testspec = seq(
    '/',rep_until(
        AnyChar(),
        seq('/',opt(pcre_options,default=UInt32(0)),'\n',
            transform=2),
        true; wrap=JoinSubstring),
    rep(seq(
        "    ",
        rep_until(AnyChar(), '\n'; wrap=JoinSubstring),
        rep(seq(rep(' '),integer,':',rep(' '),
                rep_until(AnyChar(), '\n'; wrap=JoinSubstring),
                transform=(v,i) -> (i=v[2],result=v[5]))),
        transform=(v,i) -> (
            sequence = Meta.parse("\""*v[2]*"\""), # v[2],
            expect = v[3])
    )),
    opt(seq("\\= Expect no match\n",
            rep(seq(
                "    ",
                rep_until(AnyChar(), '\n'; wrap=JoinSubstring),
                "No match\n",
                transform=2
            )),
            transform=2)),
    transform = (v,i) -> (
        pattern = (with_options(reverse(v[2])...)),
        tests = v[3],
        tests_nomatch = v[4])
);


tests_string=read("/home/gregor/dev/pcre/testdata/testoutput1",String);

@time _iterate(rep(seq(
    rep(alt(seq(at_linestart,'#',rep_until(AnyChar(),'\n',wrap=JoinSubstring)),
            seq(at_linestart,rep_until(CharIn(whitespace),'\n',wrap=JoinSubstring)))))),
              raw"""
# This set of tests is for features that are compatible with all versions of
# Perl >= 5.10, in non-UTF mode. It should run clean for the 8-bit, 16-bit, and
# 32-bit PCRE libraries, and also using the perltest.sh script.
    
#forbid_utf
#newline_default lf any anycrlf
#perltest

""")


_iterate(seq(at_linestart,'#',rep_until(AnyChar(),'\n',wrap=JoinSubstring)),
         "# This set of tests is for features that are compatible with all versions of\n")

# parse(pattern,with_options(Base.PCRE.MULTILINE,"^"))

skip_blindspace =
    parser( alt(
        on_options(Base.PCRE.EXTENDED,
                   rep(CharIn(whitespace))),
        Always())
            => Always());

skip_comment =
    on_options(Base.PCRE.EXTENDED,
               seq('#',rep(CharIn(whitespace)),
                   rep_until(
                       AnyChar(),
                       seq(rep(CharIn(whitespace)),alt(bsr,AtEnd())),
                       wrap = JoinSubstring
                   ),
                   transform=(v,i) -> with_log(v[3],Always())
                   ));

using Test
@test parse(skip_comment,with_options(Base.PCRE.EXTENDED,"# some comment   \n")) == with_log("some comment",Always())
@test parse(skip_comment,"# some comment   \n") === nothing

pushfirst!(pattern,skip_comment);

comment_par = seq(
    '(', '?', '#',rep(CharIn(whitespace)),
    rep_until(
        AnyChar(),
        seq(rep(CharIn(whitespace)),')'),
        wrap = JoinSubstring
    ),
    transform=(v,i) -> with_log(v[5],Always())
);

@test parse(comment_par,"(?# comment)") == with_log("comment",Always())
@test match(r"(?# comment)a","a").match == "a"
## match(re"(?# comment)a","a").match == "a"

push!(pattern,comment_par);

# https://www.regular-expressions.info/refcharacters.html
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC4
char = let meta_chars = raw"\^$.[|()?*+{"
    alt(Char,
        CharNotIn([ c for c in meta_chars]),
        seq('\\', CharIn(Set(vcat([m for m in meta_chars],whitespace))),
            transform=2),
        transform=(v,i) -> convert(Char,v)
        )
end;

@test parse(char,with_options(Base.PCRE.CASELESS,"A")) =='a'
@test parse(char,"A") =='A'
@test parse(char,"\\^") =='^'
@test parse(char,"^") === nothing


push!(pattern,char);




# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5
escape_sequence = seq(String,"\\Q",rep_until(ParserAlchemy.any(),"\\E"),
                      transform=(v,i)->join(v[2]));
_iterate(escape_sequence,raw"\Q[].\E")
push!(pattern,escape_sequence);

match(r"\Q \Ea"x," a")

# https://www.regular-expressions.info/refbasic.html
dot = alt(
    on_options(Base.PCRE.DOTALL,'.') => AnyChar(), ## todo: allow \n matching context 
    '.' => CharNotIn('\n'), ## todo: allow \n matching context 
    "\\N" => CharNotIn('\n')
)
@btime _iterate(pattern,".")
@btime _iterate(pattern,"\\N")
push!(pattern,dot);


# The horizontal space characters are:
horizontal_space=(
    '\U0009', # "Horizontal tab (HT)"),
    '\U0020', # "Space"),
    '\U00A0', # "Non-break space"),
    '\U1680', # "Ogham space mark"),
    '\U180E', # "Mongolian vowel separator"),
    '\U2000', # "En quad"),
    '\U2001', # "Em quad"),
    '\U2002', # "En space"),
    '\U2003', # "Em space"),
    '\U2004', # "Three-per-em space"),
    '\U2005', # "Four-per-em space"),
    '\U2006', # "Six-per-em space"),
    '\U2007', # "Figure space"),
    '\U2008', # "Punctuation space"),
    '\U2009', # "Thin space"),
    '\U200A', # "Hair space"),
    '\U202F', # "Narrow no-break space"),
    '\U205F', # "Medium mathematical space"),
    '\U3000' # "Ideographic space"))
)

# The vertical space characters are:
vertical_space=(
    '\U000A', # "Linefeed (LF)"),
    '\U000B', # "Vertical tab (VT)"),
    '\U000C', # "Form feed (FF)"),
    '\U000D', # "Carriage return (CR)"),
    '\U0085', # "Next line (NEL)"),
    '\U2028', # "Line separator"),
    '\U2029') # "Paragraph separator"))



hex_digit = CharIn('A':'F','a':'f','0':'9')
function character_base(base,mind=0,maxd=1000)
    dig = if base == 16
        hex_digit
    elseif base == 8
        CharIn('0':'7')
    elseif base ==10
        CharIn('0':'9')
    else
        error()
    end
    rep(Int,dig,(mind,maxd),
        transform=(v,i) -> isempty(v) ? 0 : parse(Int,join(v),base=base))
end


parse(character_base(8),"765")
parse(character_base(10),"765")
parse(character_base(16),"765")
parse(character_base(16),"")


parse(seq("\\x{",character_base(16),"}",transform=(v,i) -> Char(v[2])),"\\x{10}")

word=CharIn(UnicodeClass("L","N"),'_')

non_word=CharNotIn(UnicodeClass("L","N"),'_')

simple_assertions =
    seq(
        '\\',
        alt(
            'A' => AtStart(),
            'z' => AtEnd(),
            'Z' => PositiveLookahead(seq(opt(bsr),AtEnd())),
            'b' => alt(
                PositiveLookahead(seq(AtStart(),word)),
                PositiveLookahead(seq(word,AtEnd())),
                seq(PositiveLookbehind(word),PositiveLookahead(non_word)),
                seq(PositiveLookbehind(non_word),PositiveLookahead(word))
            ),
            # 'B' => alt(
            #     NegativeLookahead(seq(AtStart(),word)),
            #     NegativeLookahead(seq(word,AtEnd())),
            #     seq(NegativeLookbehind(word),PositiveLookahead(non_word)),
            #     seq(NegativeLookbehind(non_word),PositiveLookahead(word))
            # ),
        ),
        transform=2)
push!(pattern,simple_assertions);

push!(pattern,parser( "\\R" => bsr ));

escaped_character = 
    seq('\\',
        alt(Char,
            'a' => ('\a'), #   \a        alarm, that is, the BEL character (hex 07)
            seq(Char,'c',AnyChar(), transform=(v,i)->error("not supported \\c$(v[2])")), #   \cx       "control-x", where x is any ASCII character
            'e' => ('\e'), #   \e        escape (hex 1B)
            'f' => ('\f'), #   \f        form feed (hex 0C)
            'n' => ('\n'), #   \n        linefeed (hex 0A)
            'r' => ('\r'), #   \r        carriage return (hex 0D)
            't' => ('\t'), #   \t        tab (hex 09)
            seq(Char,'0',character_base(8,0,2), transform=(v,i)->(Char(v[2]))), #   \0dd      character with octal code 0dd
            seq(Char,character_base(8,3,3), transform=(v,i)->(Char(v[1]))), ## todo: backreference, if a capture with number (in decimal) is defined #   \ddd      character with octal code ddd, or back reference
            seq(Char,'o','{',character_base(8),'}', transform=(v,i)->(Char(v[3]))), #   \o{ddd..} character with octal code ddd..
            seq(Char,'x','{',character_base(16),'}', transform=(v,i)->(Char(v[3]))), #   \xhh      character with hex code hh
            seq(Char,'x',character_base(16,0,2), transform=(v,i)->(Char(v[2]))), #   \x{hhh..} character with hex code hhh.. (non-JavaScript mode)
            seq(Char,'h',character_base(16,4,4), transform=(v,i)->(Char(v[2]))), #   \uhhhh    character with hex code hhhh (JavaScript mode only)
        ),
        transform=2);
push!(pattern,escaped_character);

generic_character_type =
    seq(
        '\\',
        alt(
            'd' => CharIn('0':'9'), # "any decimal digit"),
            'D' => CharNotIn('0':'9'), # "any character that is not a decimal digit"),
            'h' => CharIn(horizontal_space...), # "any horizontal white space character"),
            'H' => CharNotIn(horizontal_space...), # "any character that is not a horizontal white space character"),
            's' => CharIn(horizontal_space...,vertical_space...), # "any white space character"),
            'S' => CharNotIn(horizontal_space...,vertical_space...), # "any character that is not a white space character"),
            'v' => CharIn(vertical_space...), # "any vertical white space character"),
            'V' => CharNotIn(vertical_space...), # "any character that is not a vertical white space character"),
            'w' => word, # "any "word" character"),
            'W' => non_word, # "any "non-word" character"),
        ),
        transform=2);

parse(parser(parse(escaped_character,"\\100")),"@")
parse(parser(parse(escaped_character,"\\o{100}")),"@")

parse(parser(parse(escaped_character,"\\x10")),"\U0010")
parse(parser(parse(escaped_character,"\\x{010}")),"\U0010")

parse(escaped_character,"\\t")

using Test
@test parse(escaped_character,"\\x{0065}") == ('e')
_iterate(escaped_character,"\\x{0065}")

parse(character_base(8,3,3),"100")
push!(pattern,generic_character_type);




Char(64)
match(r"\100","@")
match(r"\x100","@")
match(r"\o{100}","@")
parse(parser(parse(escaped_character,"\\100")),"@")
match(r"\x1","\U0010")
"\x10"[1]



skip_whitespace_on(flags) =
    alt(on_options(flags,rep(CharIn(whitespace...))=>Always()),
        Always())

# https://www.regular-expressions.info/posixbrackets.html#class

bracket_char = let bracket_meta_chars = raw"][\^-"
    seq(Char,
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE),
        alt(
            CharNotIn([ c for c in bracket_meta_chars]),
            CharNotIn([ c for c in bracket_meta_chars]),
            seq('\\', CharIn([ c for c in bracket_meta_chars]),
                transform=2),
            escaped_character
        ),
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE),
        transform=(v,i) -> convert(Char,v[2]))
end;

parse(bracket_char,"a")
parse(bracket_char,with_options(Base.PCRE.EXTENDED_MORE,"  a  "))

parse(opt('a'),"b")
_iterate(opt('a'),"b")

bracket=seq(CharMatcher,
            '[',opt('^')
            , opt(']')
            , rep(alt(CharMatcher,
                      seq("[:",
                          alt(CharMatcher,
                              "alnum" => CharIn(UnicodeClass("L","N")), # Xan
                              "alpha" => CharIn(UnicodeClass("L")),
                              ##"ascii" => CharIn(UnicodeClass("InBasicLatin")),
                              "blank" => CharIn(UnicodeClass("Zs"),'\t'),
                              "cntrl" => CharIn(UnicodeClass("Cc")),
                              "digit" => CharIn(UnicodeClass("Nd")),
                              "graph" => CharNotIn(UnicodeClass("Z","C")),
                              "lower" => CharIn(UnicodeClass("Ll")),
                              "print" => CharIn(UnicodeClass("C")),
                              "punct" => CharIn(UnicodeClass("P")),
                              "space" => CharIn(UnicodeClass("Z"),'\t','\r','\n','\v','\f'),
                              "upper" => CharIn(UnicodeClass("Lu")),
                              "word" => CharIn(UnicodeClass("L","Nl","Nd","Pc")),
                              "xdigit" => hex_digit,
                              ),
                          ":]",
                          transform=2),
                      generic_character_type,
                      seq(CharIn{StepRange{Char,Int}},bracket_char,'-',bracket_char, transform=(v,i)->CharIn(v[1]:v[3]))
                      , bracket_char))
            , ']'
            , transform = (v,i) -> if (@show v)[2]===missing
            CharIn(v[3],v[4]...)
            else
            CharNotIn(v[3],v[4]...)
            end);

using BenchmarkTools
@btime _iterate(bracket,"[a-z]")
parse(bracket,with_options(Base.PCRE.EXTENDED_MORE,"[a-z ]"))
parse(bracket,"[\x3f-\x5F]")
parse(parser(']'),"]")
parse(bracket,"[]abc]")
parse(bracket,with_options(Base.PCRE.EXTENDED_MORE,"[a- z]"))

pp = bracket;
s = with_options(Base.PCRE.EXTENDED_MORE,"[a- z]")


match(r"(?xx)[a- x]","b")
match(r"(?xx)[a- x]","b")

push!(pattern,bracket);




# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC17
repetition = alt(
    "+"=>(1,typemax(Int)),
    "*"=>(0,typemax(Int)),
    "?"=>(0,1),
    seq(Tuple{Int,Int},
        "{",integer,
        opt(",",opt(integer, default=typemax(Int)),
            transform_seq=2),"}",
        transform=(v,i) -> if v[3]===missing
        (v[2],v[2])
        else
        (v[2],v[3])
        end
        ));
parse(repetition,"{1,}")
tokenize(repetition,"{1,}")
parse(repetition,"*")
parse(repetition,"+")
parse(repetition,"{3}")
parse(repetition,"{1,}")
parse(repetition,"?")

quantified=seq(
    ParserTypes,
    skip_whitespace_on(Base.PCRE.EXTENDED),
    pattern,
    skip_whitespace_on(Base.PCRE.EXTENDED),
    opt(repetition, default=(1,1)),
    skip_whitespace_on(Base.PCRE.EXTENDED),
    opt('+'), # possessive quantifier
    skip_whitespace_on(Base.PCRE.EXTENDED)
) do v,i
    result = if v[4]==(1,1)
        parser(v[2])
    elseif v[4]==(0,1)
        opt(v[2])
    else
        rep(v[2],v[4])
    end
    if v[6]!==missing
        atomic(result)
    else
        result
    end
end;

tokenize(quantified,"a?")|>regex_string


_iterate(pattern,with_options(Base.PCRE.EXTENDED,"a"))|>dump
parse(quantified,"a{3}")
parse(quantified,with_options(Base.PCRE.EXTENDED,"a {3}"))

sequence = rep(ParserTypes,
               quantified) do v,i
                   length(v) ==1 ? v[1] : seq(v...)
               end;

parse(parse(sequence,"ab*"), "abbb")
pp=parse(sequence,"ab*")
@btime _iterate(pp, "abbb")
@btime match(r"ab*","abbb")


alternations = seq(Vector{ParserTypes},
                   sequence, rep(seq('|',sequence, transform=2)),
                   transform=(v,i) -> [v[1],v[2]...]);

alternation = instance(
    ParserTypes,
    alternations) do v,i
        length(v)==1 ? v[1] : alt(v...)
    end;
                                 

parse(alternation,"a|b")

parse(alternation,with_options(Base.PCRE.EXTENDED|Base.PCRE.EXTENDED_MORE,"a {3}bc | d | [a - e]# comment?"))


macro re_str(x)
    esc(quote
        println($x)
        r=parse(seq(AtStart(),alternation,AtEnd(), transform=2),$x)
        r === nothing && error("invalid regex")
        ParserAlchemy.indexed_captures(r)
        end)
end


macro test_pcre(pattern,seq,log=false)
    quote
        let name = string($seq)
            @testset "$name" begin
                pcre=Regex($pattern)
                pc  =ParserAlchemy.indexed_captures(parse(alternation,$pattern))
                pcre_m = match(pcre,$seq)
                pc_m = match(pc,$seq)
                if $log
                    println("testing r\"",$pattern,"\" on \"",$seq,"\"")
                    println(pc_m)
                    println(pcre_m)
                end
                if pcre_m === nothing
                    @test pcre_m == pc_m
                else
                    @test pcre_m.match==pc_m.match
                    @test pcre_m.captures==pc_m.captures
                end
            end
        end
    end |> esc
end

## push!(pattern,alternation) ## stackoverflow

# Atomic groups
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC18
atomic_group=seq(ParserTypes,
                 "(?>",alternation,")",
                 transform=(v,i)->atomic(v[2]));
parse(atomic_group,"(?>ab*)")|>regex_string
push!(pattern,atomic_group);
# sequences and alternation


name = seq(String,
           CharIn('a':'z','A':'Z'),rep(CharIn('0':'9','a':'z','A':'Z')),
           transform=(v,i)->v[1]*join(v[2]));


captured=seq(ParserTypes,
             "(",
             alt(seq("?<",name,'>', transform=2),
                 seq("?P<",name,'>', transform=2),
                 seq("?'",name,"'", transform=2),
                 ""),
             alternation,
             ")",
             transform=(v,i)->ParserAlchemy.capture(Symbol(@show v[2]),v[3]));
match(parse(captured,"(ab)"),"ab")
pp = parse(captured,"(?<a>ab)")
@btime match(pp,"ab")

parse(parse(captured,"(ab)"), "ab")
push!(pattern,captured);


re"(?<a>ab)(?#comment)"

## last value in captures in rep
@btime match(r"(?:(a.))*","abac")

# should work? see
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC16
match(r"(?J)(?:(?<n>foo)|(?<n>bar))\k<n>","foofoo")

subpattern=seq("(?:",alternation,")",
               transform=2);
parse(parse(subpattern,"(?:ab)"),
      "ab")
push!(pattern,subpattern);


lookaheads=seq("(?",
               alt(seq(ParserTypes,
                       '=',alternation,
                       transform=(v,i) -> look_ahead(true,v[2])),
                   seq(ParserTypes,
                       '!',alternation,
                       transform=(v,i) -> look_ahead(false,v[2]))),
               ")",
               transform=2);
push!(pattern,lookaheads);



pp = seq('a',rep('b'),NegativeLookbehind(seq('b','b')),rep('b'),'c')
parse(pp, "abbbc")
parse(map_parser(revert,pp), "cbbba")

lookbehinds=seq("(?<",
                alt(seq(ParserTypes,
                        '=',alternation,
                        transform=(v,i) -> look_behind(true,v[2])),
                    seq(ParserTypes,
                        '!',alternation,
                        transform=(v,i) -> look_behind(false,v[2]))),
                ")",
                transform=2);
push!(pattern,lookbehinds);


pp = seq('a',rep('b'),NegativeLookbehind(seq('b')),rep('b'),'c')
parse(pp, "abbbc")
map_parser(revert,pp)
 

@test_pcre "ab*(?<=ab)c" "abc" true
  

@test_pcre "((((a)(b))))(c)" "abc" true

@test_pcre "(a)|(d)" "a" true


@test_pcre "(1a(2b?)*)*0" "1a1a21a2b22b0" true

@test_pcre "(1a(2b?)*)*0" "1a1a21a2b22b0" true

@test_pcre "(ab)*c" "ababc" true


@test_pcre "^(ab)*c\$" "ababc" true


# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
backreference = instance(
    ParserTypes,
    alt(
        seq('\\',alt(Union{Int,String},
                     integer, ## todo: maybe octal char
                     seq('g',integer,transform=2),
                     seq("g{",integer,'}',transform=2),
                     seq("g{",name,'}',transform=2),
                     seq("k<",name,'>',transform=2),  # perl
                     seq("k'",name,'\'',transform=2), # 
                     ),
            transform=2),
        seq("(?P=",name,')',transform=2))) do v,i
            BackReference(v)
        end;
parse(backreference,"(?P=ab)")|>dump
parse(backreference,"\\1")|>dump
parse(backreference,"\\g-1")|>dump
push!(pattern,backreference);


# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
subroutine = seq("(?",alt('+','-',""),
                 integer,')',
                 transform=(v,i)->SubRoutine(nothing,Symbol(v[2]),v[3])
                 );
parse(subroutine,"(?-1)")|>dump
push!(pattern,subroutine);


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


@test_pcre "\\d+" "1123"
@test_pcre "\\D+" "abcd"

@test_pcre "\\R" "\n"
@test_pcre "\\W" " "

@test_pcre "^ab*(?<ab>c)" "ac"

@test_pcre "a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz" "abcxyzpqrrrabbxyyyypqAzz" true


parse_all(re"a*(abc)?", "abc")


## todo: support lazy
@test_pcre "a*?(abc)?" "abc"



@test_pcre "the quick brown fox" "What the quick brown fox"





## TODO: lazy rep
## @test_pcre "a(?:b|(c|e){1,2}?|d)+?(.)" "ace"
                  


parse(testspec,raw"""
/the quick brown fox/
    the quick brown fox
 0: the quick brown fox
    What do you know about the quick brown fox?
 0: the quick brown fox
\= Expect no match
    The quick brown FOX
No match
    What do you know about THE QUICK BROWN FOX?
No match
""")


tt = parse(testspec,
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
           )

@test_pcre tt.pattern tt.tests[1].sequence
@pcre_testset tt true

Regcomb(x::AbstractString) = @re_str(x)
Regcomb(x::ParserAlchemy.WithOptions{<:AbstractString}) = set_options(x.flags,@re_str(x))
macro pcre_testset(tt,log=false)
    quote
        let ts = $(tt), name = string(ts.pattern)
            ## println(ts)
            @testset "$name" begin
                pcre=Regex(ts.pattern)
                ## pc  =ParserAlchemy.indexed_captures(set_options(ts.pattern.flags,parse(alternation,ts.pattern)))
                pc  =Regcomb(ts.pattern)
                ## println(pc)
                for seq in ts.tests
                    pcre_m = match(pcre,seq.sequence)
                    pc_m = match(pc,seq.sequence)
                    if $log
                        println("testing r\"",name,"\" on \"",seq.sequence,"\"")
                        println(pc_m)
                        println(pcre_m)                    
                    end
                    if pcre_m === nothing
                        @test pcre_m == pc_m
                    else
                        @test pcre_m.match==pc_m.match &&
                            pcre_m.match.offset==pc_m.match.offset &&
                            pcre_m.match.ncodeunits==pc_m.match.ncodeunits
                        @test pcre_m.captures==pc_m.captures
                    end
                end
                for seq in ts.tests_nomatch
                    if $log
                        println("testing r\"",name,"\" on \"",seq,"\"")
                    end
                    pcre_m = match(pcre,seq)
                    pc_m = match(pc,seq)
                    @test pcre_m === nothing && pc_m===nothing
                end
            end
        end
    end |> esc
end

@pcre_testset parse(testspec,raw"""
    /^[\x3f-\x5F]+$/i
        WXY_^ABC
     0: WXY_^ABC
        WXY_^abc
     0: WXY_^abc
        wxy_^ABC
     0: wxy_^ABC
    """)

macro test_pcre_str(x)
    quote
        @pcre_testset parse(testspec,$x) true
    end |> esc
end

test_pcre"""
/^[\x3f-\x5F]+$/i
    WXY_^ABC
 0: WXY_^ABC
    WXY_^abc
 0: WXY_^abc
    wxy_^ABC
 0: wxy_^ABC
"""

test_pcre"""
/^[W-c]+$/i
    WXY_^abc
 0: WXY_^abc
    wxy_^ABC
 0: wxy_^ABC
"""

test_pcre"""
/the quick brown fox/
    the quick brown fox
 0: the quick brown fox
    What do you know about the quick brown fox?
 0: the quick brown fox
\= Expect no match
    The quick brown FOX
No match
    What do you know about THE QUICK BROWN FOX?
No match
"""

test_pcre"""
/abcd\t\n\r\f\a\e\071\x3b\$\\\?caxyz/
    abcd\t\n\r\f\a\e9;\$\\?caxyz
 0: abcd\x09\x0a\x0d\x0c\x07\x1b9;$\?caxyz
"""

pp = re"a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz"
match(pp, "abxyzpqrrrabxyyyypqAzz")

match(re"a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz",
      "abxyzpqrrrabxyyyypqAzz")

      ##"abxyzpqrrrabbxyyyypqAzz")
      "abxyzpqrrabbxyyyypqAzz")

test_pcre"""
/a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz/
    abxyzpqrrrabbxyyyypqAzz
 0: abxyzpqrrrabbxyyyypqAzz
    abxyzpqrrrabbxyyyypqAzz
 0: abxyzpqrrrabbxyyyypqAzz
    aabxyzpqrrrabbxyyyypqAzz
 0: aabxyzpqrrrabbxyyyypqAzz
    aaabxyzpqrrrabbxyyyypqAzz
 0: aaabxyzpqrrrabbxyyyypqAzz
    aaaabxyzpqrrrabbxyyyypqAzz
 0: aaaabxyzpqrrrabbxyyyypqAzz
    abcxyzpqrrrabbxyyyypqAzz
 0: abcxyzpqrrrabbxyyyypqAzz
    aabcxyzpqrrrabbxyyyypqAzz
 0: aabcxyzpqrrrabbxyyyypqAzz
    aaabcxyzpqrrrabbxyyyypAzz
 0: aaabcxyzpqrrrabbxyyyypAzz
    aaabcxyzpqrrrabbxyyyypqAzz
 0: aaabcxyzpqrrrabbxyyyypqAzz
    aaabcxyzpqrrrabbxyyyypqqAzz
 0: aaabcxyzpqrrrabbxyyyypqqAzz
    aaabcxyzpqrrrabbxyyyypqqqAzz
 0: aaabcxyzpqrrrabbxyyyypqqqAzz
    aaabcxyzpqrrrabbxyyyypqqqqAzz
 0: aaabcxyzpqrrrabbxyyyypqqqqAzz
    aaabcxyzpqrrrabbxyyyypqqqqqAzz
 0: aaabcxyzpqrrrabbxyyyypqqqqqAzz
    aaabcxyzpqrrrabbxyyyypqqqqqqAzz
 0: aaabcxyzpqrrrabbxyyyypqqqqqqAzz
    aaaabcxyzpqrrrabbxyyyypqAzz
 0: aaaabcxyzpqrrrabbxyyyypqAzz
    abxyzzpqrrrabbxyyyypqAzz
 0: abxyzzpqrrrabbxyyyypqAzz
    aabxyzzzpqrrrabbxyyyypqAzz
 0: aabxyzzzpqrrrabbxyyyypqAzz
    aaabxyzzzzpqrrrabbxyyyypqAzz
 0: aaabxyzzzzpqrrrabbxyyyypqAzz
    aaaabxyzzzzpqrrrabbxyyyypqAzz
 0: aaaabxyzzzzpqrrrabbxyyyypqAzz
    abcxyzzpqrrrabbxyyyypqAzz
 0: abcxyzzpqrrrabbxyyyypqAzz
    aabcxyzzzpqrrrabbxyyyypqAzz
 0: aabcxyzzzpqrrrabbxyyyypqAzz
    aaabcxyzzzzpqrrrabbxyyyypqAzz
 0: aaabcxyzzzzpqrrrabbxyyyypqAzz
    aaaabcxyzzzzpqrrrabbxyyyypqAzz
 0: aaaabcxyzzzzpqrrrabbxyyyypqAzz
    aaaabcxyzzzzpqrrrabbbxyyyypqAzz
 0: aaaabcxyzzzzpqrrrabbbxyyyypqAzz
    aaaabcxyzzzzpqrrrabbbxyyyyypqAzz
 0: aaaabcxyzzzzpqrrrabbbxyyyyypqAzz
    aaabcxyzpqrrrabbxyyyypABzz
 0: aaabcxyzpqrrrabbxyyyypABzz
    aaabcxyzpqrrrabbxyyyypABBzz
 0: aaabcxyzpqrrrabbxyyyypABBzz
    >>>aaabxyzpqrrrabbxyyyypqAzz
 0: aaabxyzpqrrrabbxyyyypqAzz
    >aaaabxyzpqrrrabbxyyyypqAzz
 0: aaaabxyzpqrrrabbxyyyypqAzz
    >>>>abcxyzpqrrrabbxyyyypqAzz
 0: abcxyzpqrrrabbxyyyypqAzz
\= Expect no match
    abxyzpqrrabbxyyyypqAzz
No match
    abxyzpqrrrrabbxyyyypqAzz
No match
    abxyzpqrrrabxyyyypqAzz
No match
    aaaabcxyzzzzpqrrrabbbxyyyyyypqAzz
No match
    aaaabcxyzzzzpqrrrabbbxyyypqAzz
No match
    aaabcxyzpqrrrabbxyyyypqqqqqqqAzz
No match
"""

test_pcre"""
/^(abc){1,2}zz/
    abczz
 0: abczz
 1: abc
    abcabczz
 0: abcabczz
 1: abc
\= Expect no match
    zz
No match
    abcabcabczz
No match
    >>abczz
No match
"""

test_pcre"""
/^(b+|a){1,2}c/
    bc
 0: bc
 1: b
    bbc
 0: bbc
 1: bb
    bbbc
 0: bbbc
 1: bbb
    bac
 0: bac
 1: a
    bbac
 0: bbac
 1: a
    aac
 0: aac
 1: a
    abbbbbbbbbbbc
 0: abbbbbbbbbbbc
 1: bbbbbbbbbbb
    bbbbbbbbbbbac
 0: bbbbbbbbbbbac
 1: a
\= Expect no match
    aaac
No match
    abbbbbbbbbbbac
No match
"""

test_pcre"""
/^(ba|b*){1,2}?bc/
    babc
 0: babc
 1: ba
    bbabc
 0: bbabc
 1: ba
    bababc
 0: bababc
 1: ba
\= Expect no match
    bababbc
No match
    babababc
No match
"""

test_pcre"""
/^[ab\]cde]/
    athing
 0: a
    bthing
 0: b
    ]thing
 0: ]
    cthing
 0: c
    dthing
 0: d
    ething
 0: e
\= Expect no match
    fthing
No match
    [thing
No match
    \\thing
No match
"""

test_pcre"""
/^[]cde]/
    ]thing
 0: ]
    cthing
 0: c
    dthing
 0: d
    ething
 0: e
\= Expect no match
    athing
No match
    fthing
No match
"""


test_pcre"""
/^[^ab\]cde]/
    fthing
 0: f
    [thing
 0: [
    \\thing
 0: \
\= Expect no match
    athing
No match
    bthing
No match
    ]thing
No match
    cthing
No match
    dthing
No match
    ething
No match
"""

match(re"^[\x3f-\x5F]+$","WXY_^abc")


macro R_str(s)
    s
end

Meta.parse("\""*tt.tests[2].sequence*"\"")


parse(,
      " 0: abc
 1: a
")


pcre_option_start = seq(
    "(?",
    alt(seq(pcre_options,
            opt(seq('-',pcre_options,transform=2), default=UInt32(0))),
        seq(Tuple{UInt32,UInt32},'-',pcre_options,transform=(v,i) -> (UInt32(0),v[2]))),
    transform=2
)
        

parse(pcre_option_start,"(?x-s") |>dump

parse(pcre_option_start,"(?-i") |>dump

r"(?-iJ)a"

option_sequences = seq(
    opt(alternation),
    rep(after(seq(pcre_option_start,opt(')')),ParserTypes) do l
        l === missing && return "?"
        ## @show l
        instance((v,i) -> ParserTypes[ set_options(l[1]..., p) for p in v ],
                 Vector{ParserTypes},
                 set_options(l[1]...,
                             l[2] === missing ?
                             seq(alternations,')',transform=1) : alternations)
                 )
        end
        )) do v,i
            l,ro=v
            for i in 1:length(ro)
                ## @show ro[i]
                pushfirst!(ro[i],l)
                l = pop!(ro[i])
                ## @show l,r
            end
            ## @show ro
            alt( (seq(r...) for r in ro)...,l)
        end;

parse(parse(option_sequences,"abc(?i)BD|e"),
      "E")

parse(option_sequences,"abc(?i)BD|e")

match(parse(option_sequences,
            with_options(Base.PCRE.EXTENDED,"   ^    a   (?# begins with a)  b\\sc (?# then b c) \$ (?# then end)")),
      "ab c")

match(parse(option_sequences,"a(?i)aa|bb(?-i)cc|dd"),"bBcc")

match(r"(a(?i)a|bc|d)Bc","aABcd")
parse(parse(option_sequences,"a(?i)a|b(?-i)c|d"),"Bc")
match(r"a(?i)a|b(?-i)c|d","BC")

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC13
internal_options = seq(
    pcre_option_start,")",
    rep_stop(quantified,
             alt(')', '|'
                 #  Options apply to subpattern, 
                 #  (a(?i)b|c)
                 #  matches "ab", "aB", and "c".
                 #
                 #  Note, in PCRE, "
                 #  changes made in one alternative do carry on into
                 #  subsequent branches within the same subpattern. For
                 #  example,
                 #
                 #  (a(?i)b|c)
                 #
                 # matches "ab", "aB", "c", and "C", even though when
                 # matching "C" the first branch is abandoned before the
                 # option setting. This is because the effects of option
                 # settings happen at compile time. There would be some
                 # very weird behaviour otherwise."
                 )),
    transform=(v,i) -> with_options(|(v[2]...),seq(v[4]...))
    );



parse(parse(internal_options,"(?i)bcd|"),"Bcd")


push!(pattern,internal_options);

parse(parse(alternation,"(a(?i)b|c)"),"aB")

# I would like to do a short survey, can you please raise your hand if you
#     - have written a regex in your life?
#     If your hand is not up, you will know how to do that after this presentation.
#         Keep your hands up if you have ever rolled your own domain specific language with a parser generator or custom parser.
#     If your hand is not up, you will know how to do that after this presentation.
#         Please show your hand if you have been
#             if you delegate writing parsers.
#     If your hand is not up, you might even prefer to do write one your own in the future.

using PyCall

re_tests_py=read("ParserAlchemy/test/re_tests.py",String)
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



