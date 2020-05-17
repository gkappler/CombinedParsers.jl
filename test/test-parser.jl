using CombinedParsers.Regexp
import CombinedParsers.Regexp: word, non_word
import CombinedParsers.Regexp: newline, inline, whitespace

@test parse(Sequence("(", Repeat_until(!Either(Repeat(word),!Repeat(non_word)), ")")), "(balanced parenthesis)") ==
    ("(",["balanced", " ", "parenthesis"])

@testset "demo data parsing" begin
    @test 1:3 == [1,2,3]
    @test "1-3" != [1,2,3]

    import TextParse
    import TextParse: Numeric

    int_range = Sequence(
        Numeric(Int), re" *- *", Numeric(Int)) do v
            collect(v[1]:v[3])
        end

    @test parse(int_range, "1-3") == 1:3
    @test parse(int_range, "8-11") == 8:11

    int = map(x->[x],
              TextParse.Numeric(Int))

    @test parse(int, "1") == [1]

    bracket_numbers = Sequence(
        Vector{Int},
        "[", alternate(
            Either(int_range, int),
            re" *, *";
        ), "]") do v
            vcat(v[2]...)
        end

    @test parse(bracket_numbers, "[1-3,9]")==[1,2,3,9]

    @test parse(Sequence(:street => !re"[[:alpha:] ]+",
                         " ",
                         :no =>Numeric(Int)),
                "Am Hang 19") == (street="Am Hang", no=19)
    
    import CombinedParsers.Regexp: inline, newline, whitespace
    
    person = Sequence("Name: ", :name => inline, Repeat(newline),
                      :adresses => alternate(
                          Sequence(
                              "Adresse:",newline,
                              :street => !re"[[:alpha:] ]+",
                              " ",
                              :no =>Numeric(Int), newline,
                              :zip =>Numeric(Int), whitespace,
                              :city => !re"[[:alpha:] ]+",
                              Repeat(Either(whitespace,newline)),
                          ),
                          Repeat1(newline)
                      ));

    representation =
        """
    Name: Gottfried Mutbürger

    Adresse:
    Am Hang 19
    86653 Glauberg

    Adresse:
    Allee 47
    80650 Zweistadt
    """

    data = ( name = "Gottfried Mutbürger",
             adresses = [
                 (street = "Am Hang", no = 19,
                  zip = 86653, city = "Glauberg"),
                 (street = "Allee", no = 47,
                  zip = 80650, city = "Zweistadt")])

    @test parse(person,representation) == data

end



export attribute_parser

attribute_parser =
    map_at(
        (v,i) -> (lowercase(v[1]) => "$(v[5])")::Pair{String,String},
        Sequence(
            !Repeat1(word), Optional(whitespace),"=", Optional(whitespace),
            Either(Sequence(2,"\"", Repeat_until(AnyChar(),"\"",wrap=JoinSubstring)),
                   Sequence(2,"'", Repeat_until(AnyChar(),"'",wrap=JoinSubstring)),
                   !re"[0-9]+%",
                   !re"[-+]?[0-9]+",
                   !Repeat1(word),
                   !re"#[0-9A-Fa-f]{6}")
        ));

attributes = alternate(attribute_parser, whitespace);

parse(attributes,"a = 1 b=6% font=\"+1asd\"")


@testset "continue options of last Either" begin
    @test parse(attributes, " size=10% class=1") == [ "size"=>"10%", "class"=>"1" ]
end

function html(tags::AbstractParser, inner::AbstractParser, attrs=attributes)
    html(result_type(inner), tags, attrs) do until
        Repeat_until(inner, until)
    end
end

function html(inner::Function, T::Type, tags::AbstractParser, attrs_parser=attributes)
    A = eltype(result_type(attrs_parser))
    function nested_html(x,)
        (tag,attrs) = x
        Either{Any}(map_at("/>") do v,i
                    (tag=tag, attrs=attrs, children=T[])
                    end,
                    Sequence(">",
                             inner(Sequence("</",tag,">"))) do v
                    (tag=tag, attrs=attrs, children=v[2])
                    end)
    end
    FlatMap{Any}(
        Sequence(
            2,
            "<",
            Sequence(
                tags,
                Optional(Sequence(
                    2,
                    Optional(whitespace),
                    attrs_parser,
                    Optional(whitespace))))),
    nested_html)
end


@testset "html" begin
    inner = Either{Any}(!Repeat(CharNotIn("<>")));
    pushfirst!(inner,html(!re"[[:alpha:]]+",inner,attributes));
    parse(inner,"<a font=1><b>b</b>a</a>")
    using BenchmarkTools
    @test parse(inner,"<a font=\"+1\">i<b>bold</b>j</a>") == 
        (tag="a", attrs=["font"=>"+1"], children=[
            "i",
            (tag="b", attrs=[], children=["bold"]),
            "j"])

    @test parse(inner,"<a font=+1/>") ==
        (tag="a", attrs=["font"=>"+1"], children=[])
end                  
