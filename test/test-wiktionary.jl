cd(expanduser("~/dev/julia/"))
using Pkg
Pkg.activate("wiktionary")
using FilingForest
using FilingForest.Parser
using FilingForest.OrgParser
using FilingForest.Tokens
using FilingForest.WikiParser
using Test

#include(joinpath(dirname(pathof(FilingForest)),"wiktionary.jl"))
import FilingForest.WikiParser: wiki_freelink, wiki_external_link

@testset "wikitext links" begin
    @test tokenize(wiki_freelink, "[[Test|1]]") ==
        Token(Symbol("Test"), "1")

    @test tokenize(wiki_freelink, "[[Test]]") ==
        Token(Symbol("wiktionary.de"), "Test")

    @test tokenize(wiki_freelink, "[[Texas|Lone Star State]]") ==
        Token(Symbol("Texas"), "Lone Star State")
    # @test tokenize(wiki_freelink, "[[Texas#Racism|Lone Star State]]") ==
    # (target="Texas",section="Racism",title="Lone Star State")
    tokenize(wiki_freelink, "[[File:wiki.png|thumb|Wikipedia logo]]")

    @test tokenize(wiki_external_link,
                   "[http://www.bictxt.de/Schule]") ==
                       Token("http://www.bictxt.de/Schule","")
    ## todo: bare urls
end
import FilingForest.WikiParser: wikitext, wikitextParser


import FilingForest.WikiParser: expand_numbers
@test tokenize(expand_numbers,"[1]") == ["1"]
@test tokenize(expand_numbers,"[1-3]") == ["1","2","3"]
@test tokenize(expand_numbers,"[1-3,10]") == ["1","2","3","10"]

tokenize(wikitext,
         "<
ref name=
\"
Georges
\"
>
{{Ref-Georges}}
<
/ref
>")


t = ":„Die Monatsnamen werden stark flektiert. Besonderheiten: Das Dativ-e tritt nicht mehr auf: ''im Januar'' (nicht: ''im Januare''). Auch der Genitiv ist im heutigen Sprachgebrauch häufig ohne Endung: ''des Januar[s],'' […]; […], ''des 12. Januar[s].'' […] Allgemein gilt: Die endungs- und artikellose Form des Monatsnamens steht vor allem dann, wenn ein Substantiv vorangeht: ''Anfang Januar,'' […]. Auch wenn die Monatsnamen als Apposition bei Monat stehen, bleiben sie ohne Flexionsendung: ''des Monats Januar.''“"
using BenchmarkTools
##@btime tokenize(wikitextParser, t)


@testset "wikitext templates" begin 
    @test tokenize(wikitextParser, "{{test}}")[1] == Line(Token[Token(:whitespace,"")],LineContent[Template("test")])
    x = tokenize(wiki_template(), "{{Ü-Tabelle|Ü-links|Ü-rechts}}")
end


x= tokenize(rep(wikitext),
            "token [http://bictxt.de] {{IPA|\n[[Test]]}}")

tokenize(rep(wikitext),
         "token [http://bictxt.de] {{Pl.}} ") 

import FilingForest.WikiParser: template_inner

tokenize(template_inner, "-\n") 
tokenize(wikitext, " -") |> dump

tokenize(wikitext, "([[Reich]]e)")

## todo:
tokenize(wikitext, "[[Kategorie][ Afrikaans]]")
# while !isempty(inner_templates.els)
#     pop!(inner_templates.els)
# end
# for t in  inner_template_names
#     push!(inner_templates.els, t)
# end


## this hack makes the parser recursive
result_type(wikitext)
import FilingForest.WikiParser: wiki_template
result_type(wiki_template(nothing))

## @test tokenize(heading(2), "== test ==\n") ==
##    (level =2, title=[l"test", delim" "])
##    (indent=[Token(:headline, "2")], tokens=[l"test", delim" "])



import FilingForest.WikiParser: template_inner
x = tokenize(template_inner,
":[1] Königtum, (königliche) Herrschaft, Regentschaft, Monarchie
:[2] Königreich
")
x[1] |> dump
x = tokenize(seq(
            wiki_template("Bedeutungen"),emptyline,
            template_inner;
            transform = 3),
"{{Bedeutungen}}
:[1] Königtum, (königliche) Herrschaft, Regentschaft, Monarchie
:[2] Königreich
")


import FilingForest.WikiParser: wiki_lines
tokenize(alternate(wiki_lines, emptyline),
         ":{{IPA}} {{Lautschrift|haˈloː}}\n")

tokenize(
    seq(
            wiki_template("Aussprache"),emptyline,
            alternate(wiki_lines, emptyline);
            transform = 3, partial=true),
"{{Aussprache}}
:{{IPA}} {{Lautschrift|haˈloː}}
:{{Hörbeispiele}} {{Audio|De-Hallo.ogg}}
:{{Reime}} {{Reim|oː|Deutsch}}
")

tokenize(seq(
            wiki_template("Aussprache"),emptyline,
            alternate(wiki_lines, emptyline);
            transform = 3),
         "{{Aussprache}}
:{{IPA}} {{Lautschrift|ˈspiːʃiːz}}, {{Pl.}} {{Lautschrift|ˈspiːʃiːz}}
:{{Hörbeispiele}} {{Audio|En-us-species.ogg|spr=us}}")
         


x= tokenize(seq(
            wiki_template("Beispiele"),emptyline,
            alternate(wiki_lines, emptyline);
    transform = 3),
            "{{Beispiele}}
:[1] {{Beispiele fehlen|spr=la}}
")


import FilingForest.WikiParser: heading
tokenize(heading(2), "== species ==")
tokenize(heading(2),
         "== species ({{Sprache|Englisch}}) ==")

tokenize(heading(3),
         "=== {{Wortart|Substantiv|Deutsch}}, {{f}} ===")


tokenize(wiki_freelink, "[[File:Backhandspring.ogv|miniatur|[9] vom [[turnen|Turnen]] [[kennen]] wir die ''Figur'' [[Flickflack]]]]")




familia = """
{{Siehe auch|[[família]]}}
== familia ({{Sprache|Latein}}) ==
=== {{Wortart|Substantiv|Latein}}, {{f}} ===

{{Latein Substantiv Übersicht|familia|famili|ae}}

{{Worttrennung}}
:fa·mi·lia, {{Pl.}} fa·mi·li·ae

{{Grammatische Merkmale}}
:[[Genitiv]] Singular nach [[pater]], [[mater]], [[filius]], [[filia]]: ''familiās''<ref name="Georges">{{Ref-Georges}}</ref>

{{Bedeutungen}}
:[1] das [[Haus]], der [[Hausstand]] (mit Sklaven)
:[2] das [[Geschlecht]], die [[Familie]]
:[3] die [[Dienerschaft]]

{{Herkunft}}
:zu dem Adjektiv ''{{Ü|la|famulus}}''<ref name="Georges" /> „dienend“; ursprüngliche Bedeutung: die Gesamtheit der unter einem ''[[dominus]]'' stehenden Sklaven, besonders als Angehörige und Teil der Familie, dann übertragen die ganze Hausgenossenschaft (Freie und Sklaven)

{{Beispiele}}
:[1] {{Beispiele fehlen|spr=la}}

==== {{Übersetzungen}} ====
{{Ü-Tabelle|Ü-links=
:{{Übersetzungen umleiten|1|Hausstand|}} {{m}}, {{Übersetzungen umleiten||Haus|}} {{m}}
:{{Übersetzungen umleiten|2|Geschlecht|}}, {{Übersetzungen umleiten||Familie|}}
:{{Übersetzungen umleiten|3|Dienerschaft|}}
|Ü-rechts=
}}

{{Referenzen}}
:[1–3] {{Wikipedia|spr=la|familia}}
:[1–3] {{Ref-Georges}}
:[1–3] {{Lit-Stowasser: Lateinisch-deutsches Schulwörterbuch|A=1994}}, Seite 202, Eintrag „familia“

{{Quellen}}
"""

r=tokenize(wikitextParser, familia)
v = tokenize(wiktionary_defs,r, delta=3)
v[1].defs[1][2]
v = wiki_meaning(v[1])[1][2]

using FilingForest.WikiParser: number_line, wikitextParser
tokenize(alt(wiki_lines...),":[1] ''kurz für:'' [[Schraubenmutter]], welche das Gegenstück zu der [[Schraube]] bildet\n")

mutter = read("FilingForest/test/mutter.wikitext",String);
r=tokenize(wikitextParser, mutter)
v = tokenize(wiktionary_defs,r, delta=3)
v[1].defs[1][2]
 wiki_meaning(v[1]) |> typeof
v[2]
