using FilingForest
using FilingForest.Tokens
using FilingForest.Parser
using FilingForest.OrgParser

############################################################
## parts of org entries
O = org(TODO=(;kw...)->(;kw...));

import FilingForest.OrgParser: content, tags
import FilingForest.Parser: whitespace, indentation, newline


import FilingForest.OrgParser: noheadline
@test tokenize(rep(Vector{String}, noheadline), " a\n* b") == [" a"]

import FilingForest.OrgParser: content, org_link, org_radio_target, named, block, drawer

@test tokenize(tags,":a:b:c:") == [ :a, :b, :c ]

import FilingForest.OrgParser: planning, drawer_contents, logbook
# Run the ORG parser.
tokenize(instance(AbstractString, (v,i) -> v[1], r"^[[<]([0-9]{4}-[0-9]{2}-[0-9]{2} ?[^]\n\n>]*?)[]>]"),
         "[2019-04-18 Do 16:01]")
tokenize(FilingForest.OrgParser.org_ts_regexp_both,
         "[2019-04-18 Do 16:01]")



@test tokenize(
    planning,
"""
SCHEDULED: [2019-04-18 Do 16:01]
""") == [ :SCHEDULED => "2019-04-18 Do 16:01" ]


@test tokenize(O.properties,"""
    :PROPERTIES:
    :a: 1
    :b: 2
    :END:
    """) == [ :a => "1", :b => "2" ]



@test tokenize(org_link,"[[file:~/tmp][label]]") ==
    Token("file:~/tmp" => "label")

@test tokenize(org_radio_target,"<<<abb>>>") ==
    Token(:id, "abb")

import FilingForest.Parser: tokenstring
import FilingForest.OrgParser: org_macro
p=splitter(org_radio_target,
           tokenstring;
         transform = seq_vcat)
tokenize(p,"<<<abb>>>")

@test tokenize(content,"<<<abb>>>") ==
    [ Token(:id, "abb") ]

@test (tokenize(content,"      Das Wichtigste Zuerst ")) isa Vector{LineContent}

@test tokenize(content, "a<<<abb>>>b") == [ l"a", Token(:id, "abb"), l"b" ]

@test tokenize(content, "ö[[file:~/tmp][label]]a") ==
    [ l"ö", Token("file:~/tmp" => "label"), l"a" ]

@test tokenize(content, "ö[[file:~/tmp][label]]a<<<a>>>b")  ==
    [ l"ö", Token("file:~/tmp" => "label"), l"a", Token(:id => "a"), l"b" ]

nl = Token(:whitespace,"\n")
@test tokenize(org().line_parser,"single\n") ==
              Line(LineContent[ Token(:whitespace,"")], LineContent[l"single", nl])

@test tokenize(org().line_parser,"- single\n") ==
    Line(LineContent[ Token(:list,"- ")], LineContent[l"single", nl])

@test tokenize(org().line_parser,"# single\n") ==
    Line(LineContent[ Token(:comment,"# ")], LineContent[l"single", nl])


import FilingForest.Parser: word, delimiter, instance, inline, tokenstring, newline, whitespace, indentation
import FilingForest.OrgParser: tags
## @test tokenize(content_line, "a<<abb>>b") == [ l"a", Token(:id, "abb"), l"b" ]
using Nullables


tokenize(alt(seq(:title => content, whitespace, :tags => tags, indentation, newline; combine=true),
             seq(:title => content, indentation, :tags => opt(tags; default=Symbol[]), newline)),
         "a :tag:\n")

@test (tokenize(org().headline,"** w1 w2 w3 :a:b:c:\n")).title_tags.title ==
    [ l"w1", delim" ", l"w2", delim" ", l"w3" ]

@test isequal(
    tokenize(org().headline,"** a :tag:\n"),
    (level="**", prio="B", todo=missing, title_tags=(title=LineContent[l"a"], tags=[:tag])))

@test isequal(
    tokenize(O.headline,"* TODO works :too:well:\n")
    , (level="*", prio="B", todo=:TODO, title_tags=(title=[l"works"], tags=[:too,:well])))


@test isequal(tokenize(O.headline,"** simply\n"),
              (level="**", prio="B", todo=missing,
               title_tags=(title=[l"simply"],
               tags=Symbol[])))



@test tokenize(org().comment, "# a b\n") ==
Line([ Token(:comment,"# ") ],
                  LineContent[l"a", delim" ", l"b", nl ])

@test tokenize(O.list_line, " - ein Punkt\n") ==
    Line([ Token(:list, " - ") ],
                      LineContent[l"ein", delim" ", l"Punkt", nl])

O=org();
tokenize(O.list_line, " 1. ein Punkt\n") 

@test tokenize(O.list_line, " 1. ein Punkt\n") ==
    Line([ Token(:list, " 1. ") ],
                      LineContent[l"ein", delim" ", l"Punkt", nl])
 tokenize(O.list_line, "   - 5) geschilderte Gefühle vom/von Fallgeber/in\n")

import FilingForest.OrgParser: content_line
@test tokenize(content_line,"eine Zeile\n") ==
    Line([ Token(:whitespace, "") ],
                      LineContent[l"eine", delim" ", l"Zeile", nl])


@test tokenize(O.line_parser,"eine Zeile\n") ==
    Line([ Token(:whitespace, "") ],
                      LineContent[l"eine", delim" ", l"Zeile",nl])


######################## general parser

import FilingForest.Parser: filename, extension
@test (tokenize(filename, "test.pdf") ) == [l"test", Token(:ext, ".pdf")]

import FilingForest.Parser: author_email
@test isequal(tokenize(author_email, "Anonymer Agent <cia@kgb.bnd>"),
              (name= "Anonymer Agent", email="cia@kgb.bnd"))



######################## untested



import FilingForest.OrgParser: body, line_parsers


@test tokenize(body,"eine Zeile\n") ==
    [ [ Line([ Token(:whitespace, "") ]
         , LineContent[l"eine", delim" ", l"Zeile", nl]) ] ]


@test tokenize(body,"eine Zeile\n") isa Vector{<:Vector{<:Line}}

@test tokenize(body,"eine Zeile\n") ==
    [ [ Line([ Token(:whitespace, "") ]
         , LineContent[l"eine", delim" ", l"Zeile", nl]) ] ]


@test tokenize(content, "a b c") == [l"a", delim" ", l"b",delim" ",l"c" ]

par = """
ein satz.
- listen
+ verschiedener
  * Formate


ein neuer paragraph
1. Aufzählungen
a. von a-z


Einer geht noch.
echt.
"""
## how are empty paragraphs counted?
@test ((tokenize(org().body, par))[1] |> length) == 5



@test tokenize(named, """#+NAME: present
#+begin_quote
 .... day
only
#+end_quote
""") == [ Line([ Token(:present_quote => ""), Token(:whitespace, " ") ],
      LineContent[delim".... ", l"day",nl]),
     Line([ Token(:present_quote => ""), Token(:whitespace, "")  ],
      LineContent[l"only", nl])
     ]

@test tokenize(org().named, """#+name: present
#+begin_quote
 .... day
only
#+end_quote
""") == [ Line([ Token(:present_quote => ""), Token(:whitespace, " ") ],
      LineContent[delim".... ", l"day", nl]),
     Line([ Token(:present_quote => ""), Token(:whitespace, "")  ],
      LineContent[l"only", nl])
     ]


@test tokenize(body, """
#+begin_quote
 .... day
only
#+end_quote
""") == [[ Line([ Token(:quote => ""), Token(:whitespace, " ") ],
                             LineContent[delim".... ", l"day", nl]),
           Line([ Token(:quote => ""), Token(:whitespace, "")  ],
                             LineContent[l"only", nl])
                   ]]

# match(Regex(plain_regex(rep(alt(named, inline * newline)))),


@test (tokenize(org().body,
"""
mit etwas Text
 - und
 - listen

#+NAME: present
#+begin_quote
.... Max, die Klasse 7b ...
#+end_quote


""")) isa Vector{<:Vector{<:Line}}



bod = """
mit etwas Text
 - und
 - listen


#+NAME: present
#+begin_quote
.... Max, die Klasse 7b ...
#+end_quote
"""

## empty par handling?
@test (tokenize(org().body, bod) |> length)  == 3

@test (x=tokenize(org().body,
"""
Polysem is a pseudonym.
We are perfectionists,
fiddlers,    
a community of writers, programmers, debaters and doubters.


We live together, condemned to share space and time, a common fate.
We are confused by this connected world so ostentatively displaying how huge and fast it is moving.
mit etwas Text

""")) isa Vector{<:Paragraph}

# Run the ORG parser.
records = ("""
:LOGBOOK:
CLOCK: [2019-04-18 Do 16:01]--[2019-04-23 Di 18:53] => 122:52
state changes too
CLOCK: [2019-04-11 Do 17:09]--[2019-04-15 Mo 13:12] => 92:03
:END:
""")
x=tokenize(logbook, records) 


@test tokenize(rep_delim_par(r"[^ ]+", r" +"), "TODO ") == ["TODO"]
@test tokenize(rep_delim_par(r"[^ ]+", r" +"), "TODO DONE") == ["TODO","DONE"]



tokenize(org(KORFS_BRILLE=identity).entry,
"""
* KORFS_BRILLE Fallvergleich
:PROPERTIES:
:CLOCK_LAST: [2019-04-23 Di 19:06]
:TREE: QUESTION/CASE/SENTENCE
:END:
:LOGBOOK:
CLOCK: [2019-04-23 Di 19:02]--[2019-04-23 Di 19:06] =>  0:04
CLOCK: [2019-04-11 Do 17:04]--[2019-04-11 Do 17:09] =>  0:05
:END:
bla
""")


twoheads="""
** TODO Einleitung

sss ss
ss ss
ss

** TODO Login oder Pseudonym
"""
x = tokenize(rep(org().entry), twoheads)




@test (tokenize(rep(org().entry), twoheads) |> length) == 2


twoheads="""
** TODO Einleitung
** TODO Login oder Pseudonym
"""
@test ( tokenize(rep(org().entry), twoheads) |> length ) == 2

twoheads="""
** TODO Einleitung
:PROPERTIES:
:ID: 1
:END:
** TODO Login oder Pseudonym
"""
@test tokenize(rep(org().entry), twoheads) |> length == 2

twoheads="""
*** About us
:PROPERTIES:
:CREATED: [2011-07-17 So 11:51]
:CREATIONCONTEXT: [[file:~/.notmuch/notmuch-new.sh::notmuch%20search%20--output%3Dfiles%20"tag:mlist%20and%20(tag:new%20or%20tag:movemail)"%20|%20grep%20INBOX%20|%20while%20read%20f][file:~/.notmuch/notmuch-new.sh::notmuch search --output=files "tag:mlist and (tag:new or tag:movemail)" | grep INBOX | while read f]]
:WHENDOING: [[~/Documents/GregorDokumente/orgmode/diary.org][mal sehen ob ich was tun kann ausser mit großem mund dazusitzen am {2011-07-16 Sa}]]
:ID: c721aaa1-9b5e-4ee5-8e41-1b8d63db92f7
:END:
:LOGBOOK:
CLOCK: [2015-01-25 So 23:48]--[2015-01-25 So 23:55] =>  0:07
:END:
:NOTES:
[2011-07-17 So 11:51]
:END:
Polysem is a pseudonym.
We are perfectionists,
fiddlers,
a community of writers, programmers, debaters and doubters.


We live together, condemned to share space and time, a common fate.
We are confused by this connected world so ostentatively displaying how huge and fast it is moving.
We debate on our direction and doubt whether we want to impact anything at all.
We discuss all these perspectives we find. 
# [[id:fc4cdc68-3e26-4eae-a68a-7a2c988c948d][This page is a therapeutic intervention against shame]].
"""

x=tokenize(org().entry, twoheads)


import FilingForest.OrgParser: list_line
tokenize(content_line,"a community of writers, programmers, debaters and doubters.\n")

name_only_par="""
** QUESTION 2) Ziel
:PROPERTIES:
:ID:       6a37932e-e258-4606-9bd1-dc7f075a1c13
:END:

- Was wollen sie erreichen?
- Woran erkennen Sie, wenn das Problem nicht mehr besteht?


#+NAME: answer
#+begin_quote
  ...  Er soll in Zukunft pünktlich kommen! ...
#+end_quote
"""
x=tokenize(rep_delim_par(org().entry,rep(r"[ \t\r]*\n")), name_only_par)

@btime tokenize(rep_delim_par(org().entry,rep(r"[ \t\r]*\n")), name_only_par)

plain_regex(O.entry)


tokenize(rep_delim_par(org().entry,rep(r"[ \t\r]*\n")), 
"""
* CASE Eingabe eines Falles
:PROPERTIES:
:CLOCK_LAST: [2019-04-23 Di 19:14]
:END:
:LOGBOOK:
CLOCK: [2019-04-29 Mo 22:54]
CLOCK: [2019-04-23 Di 19:10]--[2019-04-23 Di 19:14] =>  0:04
CLOCK: [2019-04-23 Di 18:53]--[2019-04-23 Di 18:57] =>  0:04
CLOCK: [2019-04-11 Do 16:45]--[2019-04-11 Do 17:04] =>  0:19
CLOCK: [2019-04-11 Do 16:40]--[2019-04-11 Do 16:42] =>  0:02
CLOCK: [2019-04-11 Do 16:35]--[2019-04-11 Do 16:37] =>  0:02
:END:

Bitte beschreiben Sie den Fall, der sie gerade beschäftigt:

** QUESTION 1) Ausgangspunkt
""") 

test_head= """
* a

Text
"""
x=tokenize(rep_delim_par(org().entry,rep(r"[ \t\r]*\n")), test_head)

x = parse_org(test_head)


plan="""
*** Thing-org-Strukturen Gerlitzen: OBJECTIVE und RESULT und COMMUN
CLOSED: [2014-09-10 Wed 18:11]
:PROPERTIES:
:CUSTOM_ID: /Users/gregor/Documents/Diktate/Aufnahme_0087.mp3
:ID:       3F4A2F8D-2882-4DD5-9044-5D000E192A63
:END:
:LOGBOOK:
CLOCK: [2014-08-09 Sat 17:34]--[2014-08-09 Sat 17:47] =>  0:13
CLOCK: [2014-09-10 Wed 17:28]--[2014-09-10 Wed 18:00] =>  0:32
- State "FERTIG"     from "FRONT"      [2014-09-10 Wed 18:11]
CLOCK: [2014-09-14 Sun 01:38]--[2014-09-14 Sun 01:41] =>  0:03
CLOCK: [2014-09-14 Sun 04:32]--[2014-09-14 Sun 04:34] =>  0:02
:END:
# FERTIG  > review > publish: Audio recording [[file:/Users/gregor/Documents/Diktate/Aufnahme_0087.mp3][Aufnahme_0087.mp3]]
Lieber Gerlitzen Berg, 
"""

import FilingForest.OrgParser: org_planning_line_re, org_ts_regexp_both
match(org_planning_line_re,"CLOSED:")
match(org_ts_regexp_both, "[2019-04-18 Do 16:01]")
#    match(emacs_re("org-ts-regexp"), "<2019-04-18 Do 16:01>")
# match(emacs_re("org-ts-regexp-inactive"), "[2019-04-18 Do 16:01]")

tokenize(org().planning,"CLOSED: [2014-09-10 Wed 18:11]")


plan="""
*** Thing-org-Strukturen Gerlitzen: OBJECTIVE und RESULT und COMMUN
CLOSED: [2014-09-10 Wed 18:11]
:PROPERTIES:
:CUSTOM_ID: /Users/gregor/Documents/Diktate/Aufnahme_0087.mp3
:ID:       3F4A2F8D-2882-4DD5-9044-5D000E192A63
:END:
:LOGBOOK:
CLOCK: [2014-08-09 Sat 17:34]--[2014-08-09 Sat 17:47] =>  0:13
CLOCK: [2014-09-10 Wed 17:28]--[2014-09-10 Wed 18:00] =>  0:32
- State "FERTIG"     from "FRONT"      [2014-09-10 Wed 18:11]
CLOCK: [2014-09-14 Sun 01:38]--[2014-09-14 Sun 01:41] =>  0:03
CLOCK: [2014-09-14 Sun 04:32]--[2014-09-14 Sun 04:34] =>  0:02
:END:
# FERTIG  > review > publish: Audio recording [[file:/Users/gregor/Documents/Diktate/Aufnahme_0087.mp3][Aufnahme_0087.mp3]]
Lieber Gerlitzen Berg, 
"""
tokenize(org().entry,plan)



quot="""
*** Thing-org-Strukturen Gerlitzen: OBJECTIVE und RESULT und COMMUN
das soll ich ausrichten:
#+begin_quote
Liebe Kärntner Berge, ich bestelle euch Grüße, 
#+end_quote
"""
tokenize(org().entry,quot)


@test (tokenize(org().entry,"** [#A] A :b:\n")).prio == "A"

using FilingForest.OrgParser: block
quot="""
#+begin_quote
text
#+end_quote
"""
@test (tokenize(block,quot)) ==
    [ Line([Token(:quote, ""), Token(:whitespace, "") ],
                        LineContent[ l"text", nl ]) ]

using FilingForest.OrgParser: body
@test (tokenize(body,quot)) ==
    [ [ Line([Token(:quote, ""), Token(:whitespace, "") ],
                          LineContent[ l"text", nl ]) ] ]




tokenize(org().entry,
         read("/mnt/hgfs/Desktop/anne-gregor/Prototype Baum_Schule/responses/COMMENT/2019-06-10 Mo 16:08.org",String),
         )

tokenize(org().entry,
         read("/mnt/hgfs/Desktop/anne-gregor/Prototype Baum_Schule/COMMENT/2019-06-10 Mo 00:57.org",String))


tokenize(org().entry,
         """
*** 3) Zielstellung:
:PROPERTIES:
:ID:       6bd136fb-aae1-4ea9-881b-750e38b0fdff
:END:
a
#+begin_quote
b
#+end_quote
""") 

using FilingForest.OrgParser: drawer

note="""
* Drawers
SCHEDULED: [2019-04-23 Di 19:10] DEADLINE: [2019-04-24 Di 19:10] CLOSED: [2019-04-24 Di 19:10]
:PROPERTIES:
:ID: org-id
:CUSTOM-ID: used in my org setting, possibly mail-id
:SHA: todo git sha
:END:
:NOTES:
Auf der Gerlitze
:END:
#+name: i
#+begin_quote
Liebe Kärntner Berge, ich bestelle euch Grüße, 
#+end_quote
"""
x=convert(OrgEntry, tokenize(org().entry,note))

tokenize(org().entry, "* Drawer\n")

mail_reply = """
#+begin_quote
a

b
#+end_quote
"""
tokenize(org().body, mail_reply) |> dump





entry=convert(OrgEntry,(level = 3, prio="B", todo=:DONE, title=[l"write to file"],
                        tags=[:milestone], icon="", properties= [], order=0,
                        body=[ [ Line([ Token(:whitespace, "") ], [l"body"]) ] ]))



open("tmp.org","w") do io
    print_flat(io, entry)
end
print(entry)


OrgEntry


T="""
** QUESTION 1) Ausgangspunkt :a:
:PROPERTIES:
:ID: 1
:END:
Wo und mit wem fand die Situation statt?
#+NAME: present
#+begin_quote
 .... Max, die Klasse 7b ...
#+end_quote

- Was ist das Problem bzw. die aktuelle Herausforderung?
- Verhalten der beteiligten Personen
- Wie hat die Situation geendet?

#+NAME: answer
#+begin_quote
  ...  In der ersten Stunde kam Max zu spät ...
  ... und der Lotta ist dann der Schuh verloren gegangen
#+end_quote

"""

x = tokenize(org().entry, T)
x.body[1] |> dump



T="""
** QUESTION 1) Ausgangspunkt
:NOTES:
a
:END:

#+begin_quote
b
#+end_quote

#+NAME: answer
#+begin_quote
c
#+end_quote
"""


T="""
** QUESTION 1) Ausgangspunkt
:NOTES:
a
:END:

b
"""

x = tokenize(org().entry, T)
x.body |> dump

mapWithIndent(x.body) |> print

using TableAlchemy
TypeDB([ convert(OrgEntry,x) ])
