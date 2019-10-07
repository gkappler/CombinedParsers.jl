
using FilingForest
using FilingForest.Tokens
using FilingForest.Parser
import FilingForest.Parser: tokenstring, word, delimiter, inline

@test tokenize(tokenstring, "") == Token[]
@test tokenize(tokenstring, "a") == Token[l"a"]

@test convert(NamedTuple{(:a,),Tuple{Int}},(a=2,b=1)) == (a=2,)

@test tokenize("") == Token[]
@test tokenize("test eines") ==  Token[l"test", delim" ",l"eines"]
@test tokenize(tokenstring,"test eines") ==  Token[l"test", delim" ",l"eines"]

@test tokenize("a") == [ Token(:literal, "a") ]
@test tokenize("a b") == [ l"a",  delim" ", l"b" ]
@test tokenize(".a b") == [ delim".", l"a",  delim" ", l"b" ]
@test tokenize(".aü öb") == [ delim".", l"aü",  delim" ", l"öb" ]


@test tokenize("/a") == [ delim"/", l"a" ]
@test tokenize("/a/b/") == [ delim"/", l"a", delim"/", l"b", delim"/" ]


## TODO

## ?? @test tokenize("<a=b>") == [ Token(:a, "b") ]
## ?? @test tokenize("<b>") == [ Token(:b) ]
## ?? @test tokenize("<b=test>") == [ Token(:b,"test") ]

## ?? @test tokenize("<b=test>.pdf", V) == [ Token(:b,"test"), (@T ext ".pdf") ]

## ?? @test tokenize("/a/<b>") == [ delim"/", l"a", delim"/", Token(:b) ]

if false
    (tokenize("years like 2012 are parsed"))

    @test tokenize(l"a.b") == [ l"a", Token(:ext,".b") ]
    @test tokenize("years are parsed like 2012")[end] == Token(:yyyy,"2012")


    @test (@T yyyy 2012) == Token(:yyyy,"2012")

    @test tokenize("invoice 2012.pdf") == [ l"invoice", delim" ",  (@T yyyy 2012), (@T ext ".pdf") ]
    @test tokenize("invoice 2012.pdf.pdf") ==
        [ l"invoice", delim" ",  (@T yyyy 2012), delim".", l"pdf", (@T ext ".pdf") ]


    @test tokenize("1.2.3.7z",V) == [ (@T version "1.2.3"), @T ext ".7z" ]
    @test tokenize("draft 99.pdf",V) ==
        [ l"draft", delim" ", (@T version "99"), @T ext ".pdf" ]
end
# adding pattern to Filing applies new pattern to Files
# adding File to Filing applies old pattern to File

# query Files matching a pattern

# query Files matching a pattern





import FilingForest.Parser: bracket_number, bracket_reference

@test (tokenize(bracket_number,"[1]")) ==
    Token(:number, "[1]")

@test (tokenize(bracket_reference,"[1, 2]")) ==
    Token(:reference, "[1, 2]")


