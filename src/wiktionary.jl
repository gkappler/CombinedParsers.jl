module WikiParser

import ..Parser: inline, tokenstring, splitter, newline, whitespace, rep_delim_par, word, delimiter, bracket_number, bracket_reference, default_tokens
import ..OrgParser: indentation, paragraphs
using ..Tokens
using ..Parser
using InternedStrings


import TextParse
import TextParse: Numeric

int_range = seq(
    Vector{Int},
    # 1           # 2       # 3
    Numeric(Int), r" *- *", Numeric(Int);
    # use julia `:` syntax to collect UnitRange 
    transform=(v,i)-> collect(v[1]:v[3]))

expand_numbers = seq(
    Vector{String},"[",
    alternate(
        alt(instance(Vector{String},(v,i)->["$x" for x in v], int_range),
            instance(Vector{String},(v,i)->[v], r"[0-9]+[[:alpha:]]*")),
        ## regex: allow whitespace
        r" *, *";
    ), "]";
    transform=(v,i) -> vcat(v[2]...))


Headline{T} = NamedTuple{(:level, :title), Tuple{Int, T}}
Base.show(io::IO, v::Type{Headline{T}}) where T = print(io, "Headline{", T, "}")

function Base.show(io::IO, v::Headline{T}) where T
    print(io,repeat("=", v.level), " ")
    print(io, v.title)
    print(io," ", repeat("=", v.level))
end

Title{S,T} = NamedTuple{(:title, :text), Tuple{S, T}}
Base.show(io::IO, v::Type{Title{S,T}}) where {S,T} = print(io, "Title{", T, "}")
function Base.show(io::IO, v::Title{S,T}) where {S,T}
    println(io,v.title)
    print(io, v.text)
end


wdelim = r"^[ \t\r\n]*"
wikitext=alt(
    LineContent,
    bracket_number, ## todo: make a line type? see ordo [5]a-c
    bracket_reference,
    seq(Token,
        "<", wdelim,
        # 3
        word, wdelim, ## todo: lookback!!
        # 5
        alternate(
            seq(String,
                word, r"^[ \r\n]*=[ \r\n]*\"[ \r\n]*",
                r"^[^\"\n]*",r"[ \r\n]*\"";
                transform = (v,i) -> v[1] *"="* v[3],
                ## log=true,
                ), wdelim),
        r"^[ \r\n]*>[ \r\n]*", 
        # 7
        r"^[^<]*"s,
        r"^[ \r\n]*<[ \r\n]*/[ \r\n]*",
        # 14
        word, ## todo: lookback!!
        r"^[ \r\n]*>";
        transform = (v,i) -> Token(v[3]*" "*join(v[5],", "), v[7])
        ),
    seq(Token,
        "<", wdelim,
        # 3
        word, wdelim, ## todo: lookback!!
        # 5
        alternate(
            seq(String,
                word, r"^[ \r\n]*=[ \r\n]*\"[ \r\n]*",
                r"^[^\"\n]*",r"^[ \r\n]*\"";
                transform = (v,i) -> v[1] *"="* v[3],
                ## log=true,
                ), wdelim),
        r"^[ \r\n]*/[ \r\n]*>", 
        transform = (v,i) -> Token(v[3]*" "*join(v[5],", "), "")
        )
); 


list_item = instance(
    Vector{Token},
    (v,i) -> [Token(:list,v)],
    r"^(?:[*]+|[#]+)")

wiki_list = seq(
    Line{Token,LineContent},
    # :indent =>
    alt(list_item),
    # :tokens =>
    rep(wikitext);
    transform = (v,i) -> Line(v[1], v[2]))

wiki_content = seq(
    Line{Token,LineContent},
    # :indent =>
    instance(Vector{Token},
             (v,i)-> [Token(:whitespace,v)],
             r"^:*"),
    # :tokens =>
    rep(wikitext);
    transform = (v,i) -> Line(v[1], v[2])
);

wiki_lines = [ wiki_list, wiki_content ];
template_inner = alternate(
    alt(wiki_lines...), newline;
    appendf=(l,nl,i) -> [ Line(
        l.indent,
        vcat(l.tokens, Token(:whitespace, nl))) ]);

inner_newline = instance(Token, (v,i) -> Token(:whitespace, v), parser(newline))

function wiki_template(x=r"[^}{\|]+", key_parser=r"^[-[:alnum:]. *]*")
    seq(
        Template{Token,LineContent},
        "{{",
        ( x === nothing ? template_inner :
          x ),
        rep(
            seq(Pair{String, Paragraph{Token,LineContent}},
                opt(newline),
                "|",
                opt(key_parser,"="; default="", transform_seq=1),
                template_inner;
                ## todo: in parser have default option to intern string during building instance
                transform = (v,i) -> intern(v[3]) => v[4])),
        opt(newline),
        "}}"; 
        transform=(v,i) -> Template((v[2]),v[3]))
end



wiki_freelink = seq(
    "[[",
    alt(
        seq(Token,
            r"^([^][]*)\|",
            template_inner;
            transform =  (v,i) -> Token(Symbol(v[1][1]),
                                        string(v[2]))), # ??? a link title is a tokenstring..., a tokenpair?
        instance(Token, (v,i) -> Token("wiktionary.de", v),
                 r"^[^][]*")
    ),
    "]]"; transform=2);
push!(wikitext.els,wiki_freelink);

wiki_external_link =
    seq(Token,
        "[",
        :target => r"[^][|#]*",
        :section => opt("#", r"[^][|#]*"; default="", transform_seq = 2),
        :title => opt("|", r"[^][|#]*"; default="", transform_seq = 2),
        "]",
        transform= (v,i) -> Token(v[2].second*v[3].second, v[4].second)
        );
push!(wikitext.els,wiki_external_link);


push!(wikitext.els,wiki_template());

import ..Tokens: parentheses
function parenthesisP(name::Symbol)
    open,close = parentheses[name]
    seq(TokenPair{Symbol, Vector{LineContent}},
        open, rep(wikitext), close;
        transform=(v,i) -> TokenPair(name,v[2]))
end
function parenthesisP(open::String, close=open)
    seq(TokenPair{String, Vector{LineContent}},
        open, rep(wikitext), close;
        transform=(v,i) -> TokenPair(name,v[2]))
end

push!(wikitext.els, parenthesisP(:paren))
push!(wikitext.els, parenthesisP(:bracket))
push!(wikitext.els, parenthesisP(:curly))
push!(wikitext.els, parenthesisP(:angle))
# push!(wikitext.els, parenthesisP("\""))
# push!(wikitext.els, parenthesisP("'"))
# push!(wikitext.els, parenthesisP("„","“"))
# push!(wikitext.els, parenthesisP("''"))
# push!(wikitext.els, parenthesisP("'''"))
for p in default_tokens
    push!(wikitext.els, p)
end
push!(wikitext.els, instance(Token, parser(r"[-+*/%&!=]"), :operator))
push!(wikitext.els, instance(Token, parser(r"[^][(){}\n \t\|]"), :unknown))

heading(n) = seq(
    Line{Token, LineContent},
    Regex("^={$n,$n} *"),
    tok(r"[^\n]*",rep(wikitext)),
    Regex("^ *={$n,$n} *");
    combine=true, 
    transform = (v,i) -> Line{Token, LineContent}([ Token(:headline,string(n)) ] , v[2]))

textblock = alternate(
    vcat([ heading(level) for level in reverse(1:6) ], wiki_lines),
    emptyline; 
    appendf=(v,nl,i) -> begin
    push!(v.tokens, Token(:delimiter, nl)); [ v ];
    end)

"""
parse flat sections
"""
wikitextParser = textblock



# inner_template_names = [
#     "erweitern",
#     "Wortart", r"Pl.[0-9]?", "Gen.", "n",
#     "Prät.", "Part.",
#     "Sprache","de", "en", "cs",
#     r"[a-z]", ## todo: only valid word type abbrevs!
#     r"[a-z][a-z]", ## todo: only valid language codes!
#     r"Übersetzungen[ [:alnum:]]*", "Ü",
#     "Beispiele fehlen",
#     r"Ref-[[:alpha:]]+",
#     "IPA",
#     "Hörbeispiele", "Audio",
#     "Reime", "Reim",
#     "Komp.", "Sup.", ## Komparativ, Superlativ
#     "Wikipedia",
#     "Beispiele fehlen"
# ]
## too feeble: push!(wiki_template.els[2].els.els,wiki_template)
## inner_templates = alt(inner_template_names...) ## todo: only valid language codes!)
using TranscodingStreams
using CodecBzip2
using LibExpat

export parse_bz2
function parse_bz2(f::Function,
                   of = "/home/gregor/dev/julia/FilingForest/dewiktionary-latest-pages-articles.xml.bz2",
                   wait_onwarn = false)
    open(of) do io
        unpack = Bzip2DecompressorStream(io)
        cbs = XPCallbacks()
        local start_element_name
        ##local results = Any[]
        let count = 0,
            r=Pair{String,Vector{Pair{String,Any}}}[],
            text = [],
            ignore = String[],
            value = String[ "mediawiki",
                            "base", "case", "title", "generator", "dbname", "minor",
                            "id", "sitename", "username", "comment", "parentid", "timestamp", "ns",
                            "sha1", "model", "format", "text", "restrictions", "ip" ],
            types = String[ "redirect", "siteinfo", "contributor", "revision", "namespaces", "namespace" ],
            record = String[ "page" ],
            ignored = false
            ##
            cbs.start_element = function (h, name, attrs)
                ## lname, lattrs = name, attrs todo, push only when required in end_element
                push!(r, intern(name) => [ intern(key) => intern(attrs[key]) for key in keys(attrs) ] )
                ignored = name in ignore
                ## name in record && @info "$name...."
            end
            cbs.character_data = function(h, txt)
                if !ignored
                    txt != "\n" && txt !="" && push!(text,txt)
                    ## text = 
                end
            end            
            function trim(s::AbstractString, chars=['\n','\r',' '])
                start=firstindex(s)
                last=lastindex(s)
                while start <= last && s[start] in chars 
                    start = nextind(s, start)
                end
                while last > start && s[last] in chars
                    last = prevind(s, last)
                end
                start>last ? "" : s[start:last]
            end

            cbs.end_element = function (h, name)
                # while isempty(r[end])
                #     pop!(r)
                # end
                ## @show ignore
                ##name in record && print(text)
                ## text = replace(text, r"^[ \t\n]*|[ \t\n]*$" => "")
                name = intern(name)
                if !( name in value || name in record || name in ignore || name in types )
                    println("\n\n--\nWhat to do with $name? (r)record, (i)gnore, (v)alue\n$text\n$(r[end])")
                    strat = readline(stdin)
                    ##@show strat
                    if strat == "r"
                        push!(record, name)
                    elseif strat == "i"
                        push!(ignore, name)
                    elseif strat == "v"
                        push!(value, name)
                    end
                end
                if name in value || isempty(r[end])
                    ## @show name r[end]
                    pop!(r)
                    text =  join(filter(x-> x!="",trim.(text)), "\n") #  * txt, r"^[ \t\n\r]*|[ \t\n\r]*$" => "")
                    (!isempty(r) && text !="") && push!(r[end].second, name => text)
                    text = []
                elseif name in types
                    val = pop!(r)
                    push!(r[end].second, name => (; val.second...))
                elseif name in record
                    val = (; pop!(r).second...)
                    if !(:title in propertynames(val) &&
                         match(r"Wiktionary:|MediaWiki:",val.title)!==nothing)
                        ## show(results[end])
                        if :revision in propertynames(val) && :text in propertynames(val.revision) && !startswith(val.revision.text, "{{NOINDEX}}")
                            count = count + 1
                            f(val, count)
                        end
                    end
                elseif name in ignore
                    ig = pop!(r)
                    ## @info "ignore $(ig.first)" ig.second
                else
                    error()
                end
            end
        end
        parsestream(unpack, cbs)
    end
end

using Tables
using JuliaDB
using TextParse
using Nullables

ignore = (v, i) -> missing
ignore_heading = (v, i) -> begin
    @show typeof(v)
    length(@show v) == 1 ? [] : v[2:end]
end

wiktionary_de_content=[
                is_template_line("Übersetzungen", ignore, Missing) => (:translations, is_line()),
                is_template_line(t -> match(r"Übersicht",t.template)!==nothing) => (:overview, missing),
                is_template_line("Nebenformen", ignore, Missing) => (:variants, is_line()),
                is_template_line("Entlehnungen", ignore, Missing) => (:variants, is_line()),
                is_template_line("Nicht mehr gültige Schreibweisen") => (:deprecated, is_line()),
                is_template_line("Alternative Schreibweisen") => (:versions, is_line()),
                is_template_line("Anmerkungen", ignore, Missing) => (:annotations, is_line()),
                is_template_line("Anmerkung", ignore, Missing) => (:annotations, is_line()),
                is_template_line("Worttrennung", ignore, Missing) => (:hyphenation, is_line()),
                is_template_line("Aussprache", ignore, Missing) => (:phonetic, is_line()),
                is_template_line("Oberbegriffe", ignore, Missing) => (:superterms, is_line()),
                is_template_line("Unterbegriffe", ignore, Missing) => (:subterms, is_line()),
                is_template_line("Bedeutungen", ignore, Missing) => (:meaning, is_line()),
                is_template_line("Synonyme", ignore, Missing) => (:synonyms, is_line()),
                is_template_line("Sinnverwandte Wörter", ignore, Missing) => (:synonyms, is_line()),
                is_template_line("Wortbildungen", ignore, Missing) => (:formations, is_line()),
                is_template_line("Gegenwörter", ignore, Missing) => (:antonyms, is_line()),
                is_template_line("Abkürzungen", ignore, Missing) => (:abbreviations, is_line()),
                is_template_line("Wortfamilie", ignore, Missing) => (:family, is_line()),
                is_template_line("Grammatische Merkmale", ignore, Missing) => (:grammar, is_line()),
                is_template_line("Redewendungen", ignore, Missing) => (:phrases, is_line()),
                #:similar => is_template_line(r"^Ähnlichkeiten[ 0-9]*"),
                is_template_line("Siehe auch", ignore, Missing) => (:etymology, is_line()),
                is_template_line("erweitern", ignore, Missing) => (:etymology, is_line()),
                is_template_line("Herkunft", ignore, Missing) => (:etymology, is_line()),
                is_template_line("Beispiele", ignore, Missing) => (:examples, is_line()),
                is_template_line("Beispiele fehlen") => (:examples, is_line()),
                is_template_line("erweitern") => (:examples, is_line()),
                is_template_line("Charakteristische Wortkombinationen", ignore, Missing) => (:combinations, is_line()),
                is_template_line("Referenzen", ignore, Missing) => (:references, is_line()),
                is_template_line("Quellen", ignore, Missing) => (:sources, is_line())
            ]

export wiktionary_defs
wiktionary_defs =
    rep(seq(
        NamedTuple,
        :prefix => rep(is_line()),
        :word => seq(is_heading(x -> isequal(x.value,"2")),        
                     rep(is_line())),
        :defs => rep(seq(is_heading(x -> isequal(x.value,"3")),
                         greedy(wiktionary_de_content...; alt=[:lines => is_line() ]))
                    );
        log = false
        ##,transform = (v,i) -> v
    ));

number_line = seq(Pair{String,Vector{LineContent}},
                  IteratorParser{String}("index", x->x isa Token && variable(x)==:number, (t,i) -> value(t)),
                  rep(IteratorParser{Token}("whitespace", x->x isa Token && !isinformative(x), (t,i) -> t)),
                  rep(IteratorParser{LineContent}("rest", x->x isa LineContent, (t,i) -> t));
                  transform=(v,i) -> v[1] => v[3]);


export wiki_meaning
function wiki_meaning(v)
    L = Line{Token,LineContent}
    fields = [ x.second[1] for x in wiktionary_de_content ]
    base=(
        word = Token("wiktionary.de", trimstring(filter(t-> t isa Token,v.word[1].tokens))),
        language = v.word[1].tokens[3],
        ## todo: parse
    )
    function inner(wt)
        meaning_data = Dict{String,Dict{Symbol,Vector{L}}}()
        function pushit(num,k,val)
            meandata = get!(meaning_data, num) do
                Dict{Symbol,Vector{Line{Token,LineContent}}}()
            end
            data = get!(() -> Line{Token,LineContent}[], meandata, k)
            ## typeof(data)
            push!(data, Line([Token(:whitespace,":"), Token(:meaning, num), Token(:whitespace," ")],val))
        end
        for (k,v) in wt[2]
            ## @show k
            x = tokenize(
                rep(is_line(Pair{String,Vector{LineContent}},
                            (l, i) -> let (r,i_) = tryparsenext(number_line, l.tokens)
                            isnull(r) ? "?" => l.tokens : get(r)
                            end
                            )),
                v)
            lastnum = "?"
            for e in x
                num = e.first=="?" ? lastnum : e.first

                let is = tokenize(expand_numbers,InternedStrings.intern(num))
                    if is === nothing
                        pushit(num,k,e.second)
                    else
                        for i in is
                            pushit(i,k,e.second)
                        end
                    end
                end
                lastnum=num
            end
        end
        ## @info "?" keys(meaning_data) first(values(meaning_data)) ## (;a=1,get(meaning_data,"?",[])...) get(meaning_data,"?",[])
        getval(val,p) =  p => ( ( haskey(val,p) && val[p]!==missing ) ? val[p] : L[] )
        x = [ ( word=base.word, order=m,
                ( getval(val,p)
                  for p in fields)... )
              for (m,val) in filter(k -> k.first!="?",pairs(meaning_data))
              ]
        val = get(meaning_data,"?",Dict())
        ( (word_type=string(filter(isinformative,wt[1].tokens)), base...,           
           ( getval(val,p)
             for p in fields)...
           ),
          x
          )
    end
    [ inner(wt) for  wt = v.defs ]
end


export promote_missing
function promote_missing(x)
    nt=Vector{Pair{Symbol, Type}}()
    for i in x
        for n in propertynames(i)
            t_ = fieldtype(typeof(i),n)
            t = get!(nt, n, t_)
            t != t_
            nt[n] = promote_type(t, t_)
        end
    end
    T=NamedTuple{tuple(keys(nt)...), Tuple{values(nt)...}}
    [ convert(T, i) for i in x ]
end

export wikichunks
## import ProgressMeter
function wikichunks(inbox, output; prog=nothing, wait_onwarn = false, log = false, errorfile=nothing)
    val = take!(inbox);
    ## prog !== nothing && ProgressMeter.next!(prog; showvalues=[(:parsing, val.title)])

    ntext = try
        r=tokenize(wikitextParser, val.revision.text; errorfile=errorfile)
        r === nothing ? nothing : tokenize(wiktionary_defs,r, delta=3)
    catch e
        if wait_onwarn ##&& i < lastindex(val.revision.text)
            print("inspect and press <ENTER>")
            readline()
        end
        rethrow(e)
    end        
    if ntext !== nothing
        for v in ntext
            for (w, ms) = wiki_meaning(v)
                put!(output,("word", w))
                for m in ms
                    put!(output, ("meaning", m))
                end
            end
        end
    end
end

end
