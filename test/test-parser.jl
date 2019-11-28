
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
    """

    data = ( name = "Gottfried Mutbürger",
             adresses = [
                 (street = "Am Hang", no = 19,
                  zip = 86653, city = "Glauberg")])

    tokenize(seq(NamedTuple,
                 :street => r"[[:alpha:] ]+", 
                 :no =>Numeric(Int)),
             "Am Hang 19")

    import ParserAlchemy: inline, newline, rep1
    person = seq(NamedTuple,
                 "Name: ", :name => inline, rep(newline),
                 :adresses => rep(NamedTuple,
                                  seq("Adresse:",newline,
                                      :street => r"[[:alpha:] ]+", 
                                      :no =>Numeric(Int), newline,
                                      :zip =>Numeric(Int), 
                                      :city => r"[[:alpha:] ]+";
                                      partial=true
                                      )
                                  ))
    tokenize(person,representation)

end


inner = alt(AbstractToken, WikitextParser.simple_tokens...)

pushfirst!(inner.els,ParserAlchemy.Tokens.html(inner))

using BenchmarkTools
tokenize(inner,"<a font=+1>b <b>x y z</b>c d</a>")
                  
