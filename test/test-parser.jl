

@test tokenize(seq("(", rep_until(alt(word,delimiter), ")")), "(balanced parenthesis)") ==
    ("(",["balanced", " ", "parenthesis"])

@testset "demo data parsing" begin
    1:3 == [1,2,3]
    "1-3" == [1,2,3]

    import TextParse
    import TextParse: Numeric

    int_range = seq(
        Vector{Int},
        # 1           # 2       # 3
        Numeric(Int), r" *- *", Numeric(Int);
        # use julia `:` syntax to collect UnitRange 
        transform=(v,i)-> collect(v[1]:v[3]))

    @test tokenize(int_range, "1-3") == 1:3
    @test tokenize(int_range, "8-11") == 8:11

    int = instance(
        Vector{Int},
        (x,i)->[x],
        TextParse.Numeric(Int))

    @test tokenize(int, "1") == [1]

    bracket_numbers = seq(
        Vector{Int},
        "[", alternate(
            alt(int_range, int),
            ## regex: allow whitespace
            r" *, *";
        ), "]",
        transform=(v,i) -> vcat(v[2]...))

    tokenize(bracket_numbers, "[1-3]")

    representation =
        """
    Name: Gottfried Mutbürger

    Adresse:
    Am Hang 19
    86653 Glauberg

    Adresse:
    Am Hang 19
    86653 Glauberg
    """

    data = ( name = "Gottfried Mutbürger",
             adresses = [
                 (street = "Am Hang", no = 19,
                  zip = 86653, city = "Glauberg")])

    tokenize(seq(NamedTuple,
                 :street => r"[[:alpha:] ]+", 
                 :no =>Numeric(Int)),
             "Am Hang 19")

    import CombinedParsers: inline, newline, rep1, whitenewline, whitespace
    person = seq(NamedTuple,
                 "Name: ", :name => inline, rep(newline),
                 :adresses => alternate(
                     seq(NamedTuple,"Adresse:",newline,
                         :street => r"[[:alpha:] ]+", 
                         :no =>Numeric(Int), newline,
                         :zip =>Numeric(Int), whitespace,
                         :city => r"[[:alpha:] ]+",
                         opt(whitenewline),
                         ),
                     newline
                 ));
    tokenize(person,representation)|>dump

end

@testset "html" begin
    import CombinedParsers.Tokens: simple_tokens
    inner = alt(AbstractToken, simple_tokens...);
    pushfirst!(inner,CombinedParsers.Tokens.html(r"^[[:alpha:]]+",inner));

    CombinedParsers.Tokens.html(r"^[[:alpha:]]+",inner)

    using BenchmarkTools
    @test tokenize(inner,"<a font=\"+1\">i<b>bold</b>j</a>") == 
        Node(:a, [Token(:font,"+1")], AbstractToken[
            l"i",
            Node(:b, Token[], AbstractToken[l"bold"]),
            l"j"])

    @test tokenize(inner,"<a font=+1/>") ==
        Node(:a, [Token(:font,"+1")], AbstractToken[])
end                  


@testset "continue options of last Either" begin
    import CombinedParsers: wdelim
    ok = alternate(
        seq(Token,
            word, opt(wdelim),"=", opt(wdelim),
            alt(r"^[0-9]+%",
                r"^[-+]?[0-9]+");
            transform = (v,i) -> Token(v[1], v[5]),
            ## log=true,
            ), wdelim)
    tokenize(ok, " size=10% class=1")

    notok = alternate(
        seq(Token,
            word, opt(wdelim),"=", opt(wdelim),
            alt(r"^[-+]?[0-9]+",r"^[0-9]+%");
            transform = (v,i) -> Token(v[1], v[5]),
            ## log=true,
            ), wdelim)
    tokenize(ok, " size=10% class=2")
end
