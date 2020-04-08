
## TODO:
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
whitespace = " \t\U0085\U200E\U200F\U2028\U2029"*"\U2029\U000C\U000B"
meta_chars = raw"\^$.[|()?*+{"

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


bracket_range(start) =
    with_name(:char_range,seq(start,
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE,rep),
        '-',
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE,rep),
        bracket_char) do v
            if v[1] isa CombinedParsers.WithOptions && ( v[1].flags & Base.PCRE.CASELESS > 0 )
                cs = convert(Char,v[1]):convert(Char,v[5])
                CharIn(unique([ ( lowercase(x) for x in cs )...,
                                ( uppercase(x) for x in cs )... ]))
            else
                cs = convert(Char,v[1]):convert(Char,v[5])
                CharIn(cs)
            end
        end)

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
    rep(Int,dig,(mind,maxd)) do v
        isempty(v) ? 0 : parse(Int,join(v),base=base)
    end
end

skip_whitespace_on(flags, wrap=identity) =
    with_name(:skip_ws,on_options(
        flags,
        wrap(CharIn(whitespace...,'\n'))=>Always()))


bsr = atomic(alt("\r\n",CharIn('\n','\x0b','\f','\r','\U0085', '\U2028','\U2029'))); # backslash R (BSR)

skip_whitespace_and_comments =
    rep(alt(
        skip_whitespace_on(
            Base.PCRE.EXTENDED),
        ## comment
        on_options(
            Base.PCRE.EXTENDED,
            with_name(
                :comment,
                seq('#',rep(CharIn(whitespace)),
                    rep_until(
                        AnyChar(),
                        seq(rep(CharIn(whitespace)),alt(bsr,AtEnd())),
                        wrap = JoinSubstring
                    )) do v
                with_log(v[3],Always())
                end
            )),
        with_name(
            :comment,
            seq(
                "(?#",rep(CharIn(whitespace)),
                rep_until(
                    AnyChar(),
                    seq(rep(CharIn(whitespace)),')'),
                    wrap = JoinSubstring
                )) do v
        with_log(v[3],Always())
        end))) do v
            [a for a in v if !isa(a,Always)]
        end;

make_control(c) =
    let ui=UInt32(uppercase(c))
        ui > 127 && error("no control-$c character")
        Char(xor(ui, 0x40))
    end


seq_log(f::Function,a...) =
    seq(f, ( with_log("$i",e) for (i,e) in enumerate(a) )...)

struct UnsupportedError <: Exception
    message::String
end
Base.showerror(io::IO, e::UnsupportedError) = print(io,"unsupported PCRE syntax ",e.message)

 
hex_digit = CharIn('A':'F','a':'f','0':'9')
integer = seq(opt('-'),character_base(10,1)) do v
    if v[1]===missing
        v[2]
    else
        -v[2]
    end
end

at_linestart = alt(AtStart(),PositiveLookbehind(bsr))
lineend   = alt(AtEnd(),bsr)
at_lineend   = alt(AtEnd(),PositiveLookahead(bsr))


# pattern alternatives
# circumflex and dollar https://www.pcre.org/original/doc/html/pcrepattern.html#SEC6
pattern = alt(on_options(Base.PCRE.MULTILINE, '^' => at_linestart),
              '^' => AtStart(),
              on_options(Base.PCRE.MULTILINE, '$' => at_lineend),
              on_options(Base.PCRE.DOLLAR_ENDONLY, '$' => AtEnd()),
              '$' => alt(AtEnd(),
                         PositiveLookahead(seq(2,'\n',AtEnd())))
              );
# alt allows adding alternatives qith push!, that themselves use the alt object for recursive parsers.

# https://www.regular-expressions.info/refcharacters.html
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC4
char =  alt(
    CharNotIn([ c for c in meta_chars]),
    seq(2,'\\', CharIn(Set(vcat([m for m in meta_chars],whitespace))))) do v
        convert(AbstractParser,v)
    end
push!(pattern,char);

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5
escape_sequence(stop=AtEnd()) =
    seq(2,"\\Q",
        rep_until(AnyChar(),
                  alt("\\E",PositiveLookahead(stop)),
                  wrap=JoinSubstring));
push!(pattern,
      map(parser, with_name(:escape_sequence, escape_sequence())));


word=CharIn(UnicodeClass("L","N"),'_')

non_word=CharNotIn(UnicodeClass("L","N"),'_')
non_word_ = alt(non_word,AtStart(),AtEnd())

word_boundary = alt(
    seq(PositiveLookbehind(word),PositiveLookahead(non_word_)),
    seq(PositiveLookbehind(non_word_),PositiveLookahead(word))
)
    
@with_names simple_assertion =
    seq(2,
        '\\',
        alt(
            'A' => AtStart(),
            map_at('G') do v,i
            @warn "limited \\G support: ignoring pcre2 startoffset"
            AtStart()
            end,
            'z' => AtEnd(),
            'Z' => PositiveLookahead(seq(opt(bsr),AtEnd())),
            'b' => word_boundary,
            'B' => NegativeLookahead(word_boundary)
        ))
push!(pattern,simple_assertion);

push!(pattern,parser( "\\R" => bsr ));

name = JoinSubstring(
    seq(CharIn('a':'z','A':'Z','_'),
        rep(CharIn('0':'9','a':'z','A':'Z','_'))))

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
@with_names backreference = map(
    alt(
        seq(2,'\\',alt(
            integer, ## todo: maybe octal char
            seq(2,'g',integer),
            seq(2,"g{",integer,'}'),
            seq(2,"g{",name,'}'),
            seq(2,"k<",name,'>'),  # perl
            seq(2,"k'",name,'\''), # 
        )),
        seq(2,"(?P=",name,')'))) do v
            Backreference(v) do
                ## todo: backreference, if a capture with number (in decimal) is defined,
                ## escaped_character otherwise (if name/index not found)
                #   \ddd      character with octal code ddd, or back reference
                ## todo: error on \g<ddd>
                v isa Integer || error("capture group $v not found!")
                parse(seq(character_base(8,1,3),
                          rep(AnyChar())) do v
                      seq(parser(Char(v[1])),
                          v[2]...)
                      end,
                      "$v")
            end
        end;
push!(pattern,backreference);


@with_names escaped_character = 
    seq(2, '\\',
        alt(
            'a' => ('\a'), # alarm, that is, the BEL character (hex 07)
            seq('c',AnyChar()) do v  # \cx "control-x", where x is any ASCII character
            make_control(v[2])
            end,
            'e' => '\e',   #  escape (hex 1B)
            'f' => '\f',   #  form feed (hex 0C)
            'n' => '\n',   #  linefeed (hex 0A)
            'r' => '\r',   #  carriage return (hex 0D)
            't' => '\t',   #  tab (hex 09)
            '"' => '"',
            #   \0dd      character with octal code 0dd
            seq(Char,'0',character_base(8,0,2), transform=v->(Char(v[2]))),
            #   \ddd      character with octal code ddd, or back reference
            ## seq(Char,character_base(8,3,3), transform=v->(Char(v[1]))),
            ## see backreference, if a capture with number (in decimal) is defined
            #   \o{ddd..} character with octal code ddd..
            seq(Char,'o','{',character_base(8),'}', transform=v->(Char(v[3]))),
            #   \xhh      character with hex code hh
            seq(Char,'x','{',character_base(16),'}', transform=v->(Char(v[3]))),
            #   \x{hhh..} character with hex code hhh.. (non-JavaScript mode)
            seq(Char,'x',character_base(16,0,2), transform=v->(Char(v[2]))),
            #   \uhhhh    character with hex code hhhh (JavaScript mode only)
            seq(Char,'h',character_base(16,4,4), transform=v->(Char(v[2]))),
            CharNotIn('Q','E')
        ))


@with_names generic_character_type =
    seq(
        '\\',
        alt(
            # "any decimal digit"),
            'd' => CharIn('0':'9'),
            # "any character that is not a decimal digit"),
            'D' => CharNotIn('0':'9'),
            # "any horizontal white space character"),
            'h' => CharIn(horizontal_space...),
            # "any character that is not a horizontal white space character"),
            'H' => CharNotIn(horizontal_space...),
            # "any white space character"),
            's' => CharIn(horizontal_space...,vertical_space...),
            # "any character that is not a white space character"),
            'S' => CharNotIn(horizontal_space...,vertical_space...),
            # "any vertical white space character"),
            'v' => CharIn(vertical_space...),
            # "any character that is not a vertical white space character"),
            'V' => CharNotIn(vertical_space...),
            # "any "word" character"),
            'w' => word,
            # "any "non-word" character"),
            'W' => non_word,
        ),
        transform=2);
push!(pattern,generic_character_type);



# https://www.regular-expressions.info/posixbrackets.html#class
bracket_char = let bracket_meta_chars = raw"]\^-"
    alt(
        CharNotIn([ c for c in bracket_meta_chars]),
        escaped_character
    )
end;


@with_names bracket=seq(
    CombinedParsers.AbstractParser,
    '[',opt('^')
    , rep(alt(
        bracket_range(']'),
        ']'=>']'),(0,1))
    , rep(alt(
        seq("[:",
            alt(
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
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE,rep) => Never(),
        "\\E" => Never(),
        with_name(:escape_sequence,map(v->CharIn(v),escape_sequence())),
        generic_character_type,
        bracket_range(bracket_char),
        map(v->convert(AbstractParser,v),bracket_char),
        '^'=>'^',
        '-'=>'-'))
    , ']') do v
        r = (filter(!(x->isa(x,Never)),v[3])...,
             filter(!(x->isa(x,Never)),v[4])...)
        if v[2]===missing
            CharIn(r...)
        else
            CharNotIn(r...)
        end
    end;
push!(pattern,bracket);


# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC17
@with_names repetition = alt(
    "+"=>(1,typemax(Int)),
    "*"=>(0,typemax(Int)),
    "?"=>(0,1),
    seq(Tuple{Int,Int},
        "{",integer,
        opt(seq(",",opt(integer, default=typemax(Int)),
                transform=2)),"}") do v
    if v[3] isa Missing
    (v[2],v[2])
    else
    (v[2],v[3])
    end
    end
)

@with_names quantified=seq(
    skip_whitespace_and_comments,
    pattern,
    skip_whitespace_and_comments, ## for test 1130, preserve in map?
    opt(repetition, default=(1,1)),
    skip_whitespace_and_comments,
    opt(map(v->convert(Char,v),CharIn('+','?'))), # possessive quantifier, strip option
    skip_whitespace_and_comments
) do v
    pat = sseq(v[2],v[3]...)
    result = if v[4]==(1,1)
        parser(pat)
    elseif v[4]==(0,1)
        opt(pat)
    else
        rep(pat,v[4])
    end
    if v[6] === missing
        result
    elseif v[6]=='+'
        atomic(result)
    elseif v[6]=='?'
        lazy(result)
    else
        result
    end
end;

# Sequences and Alternation
@with_names sequence = rep(quantified) do v
    length(v) ==1 ? v[1] : seq(v...)
end;



alternations = seq(
    sequence, rep(seq('|',sequence, transform=2))) do v
        CombinedParsers.AbstractParser[v[1],v[2]...]
    end;

import CombinedParsers: pcre_options
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
@with_names pcre_option_start = seq(
    2,
    "(?",
    alt(seq(opt('^'),
            alt(seq(pcre_options,
                    opt(seq('-',pcre_options,transform=2), default=UInt32(0))),
                seq(Tuple{UInt32,UInt32},'-',pcre_options,transform=v -> (UInt32(0),v[2])))    
            ) do v
        if v[1]===missing
        # The two "extended" options are not independent; unsetting either one cancels the effects of both of them.
        affects_extended = !iszero((v[2][1] | v[2][2]) & ( Base.PCRE.EXTENDED | Base.PCRE.EXTENDED_MORE ))
        v[2][1], affects_extended ? (v[2][1] | ( Base.PCRE.EXTENDED | Base.PCRE.EXTENDED_MORE )) : v[2][2]
        else
        (v[2][1],Base.PCRE.CASELESS | Base.PCRE.MULTILINE | Base.PCRE.NO_AUTO_CAPTURE | Base.PCRE.DOTALL| Base.PCRE.EXTENDED | Base.PCRE.EXTENDED_MORE | v[2][2])
        end
        end,
        '^' => (UInt32(0),Base.PCRE.CASELESS | Base.PCRE.MULTILINE | Base.PCRE.NO_AUTO_CAPTURE | Base.PCRE.DOTALL | Base.PCRE.EXTENDED  | Base.PCRE.EXTENDED_MORE )
        ));


options_alternations = after(
    seq(pcre_option_start,opt(')')),
    Vector{AbstractParser}) do l
        set_options(l[1]..., l[2] === missing ?  seq(alternations,')',transform=1) : alternations)
    end;

option_sequences = map(
    AbstractParser,
    seq(
        alternations,
        rep(options_alternations))) do v
            r = Any[ AbstractParser[e] for e in v[1] ]
            ro = v[2]
            for i in 1:length(ro)
                length(ro[i])>0 && push!(r[end],popfirst!(ro[i]))
                for x in ro[i]
                    ## if length(ro[i])>0
                    ## @show r[end],x
                    push!(r,AbstractParser[ x ])
                end
            end
            salt( ( sseq(x...) for x in r)... )
        end;
alternation = option_sequences;


# Atomic groups
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC18
@with_names atomic_group=seq("(",alt("?>","*atomic:"),alternation,")") do v
    atomic(v[3])
end;
push!(pattern,atomic_group);

@with_names captured=seq("(",
             alt(seq(2,"?<",name,'>'),
                 seq(2,"?P<",name,'>'),
                 seq(2,"?'",name,"'"),
                 ""),
             alternation,
             ")") do v
                 with_name(v[2],CombinedParsers.capture(Symbol(v[2]),v[3]))
             end;
push!(pattern,captured);



@with_names subpattern=seq(
    2,
    "(?:",alternation,")");
push!(pattern,subpattern);


@with_names lookahead=seq(
    2,
    "(",
    alt(seq(v -> look_ahead(true,v[2]),
            alt("?=","*positive_lookahead:","*pla:"),alternation),
        seq(v -> look_ahead(false,v[2]),
            alt("?!","*negative_lookahead:","*nla:"),alternation)),
    ")");
push!(pattern,lookahead);



@with_names lookbehind=seq(
    2,
    "(",
    alt(seq(v -> look_behind(true,v[2]),
            alt("?<=","*positive_lookbehind:","*plb:"),alternation),
        seq(v -> look_behind(false,v[2]),
            alt("?<!","*negative_lookbehind:","*nlb:"),alternation)),
    ")");
push!(pattern,lookbehind);



# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
@with_names subroutine = seq(
    2,"(?",
    alt(seq(alt('+','-',""),
            integer) do v
        Subroutine(nothing,Symbol(v[1]),v[2])
        end,
        seq(alt('&',"P>"), name) do v 
        Subroutine(Symbol(v[2]),Symbol(""),-1)
        end),
    ')');
push!(pattern,subroutine);

@with_names resetting_capture_numbers = seq(
    "(?|",
    alternation,
    ")") do v
        DupSubpatternNumbers(v[2])
    end;
push!(pattern,resetting_capture_numbers);


@with_names condition = alt(
    seq(2,'(',alt(integer,
                  "DEFINE",
                  seq('R',alt(integer,seq(2,'&',name))),
                  seq(2,'\'',name,'\''),
                  seq(2,'<',name,'>'),
                  name),
        ')'),
    lookbehind,
    lookahead)

@with_names conditional = map(
    seq("(?",condition,
        sequence,
        opt(seq(2,"|",sequence), default=Always()),
        ")")) do v
            c = v[2]
            if c=="DEFINE"
                atomic(alt(Always(),v[3])) ## ignore in match
            elseif c isa Union{Integer,AbstractString}
                Conditional(Backreference(c) do
                            c == "R" && return Subroutine()
                            c isa Integer ? Backreference(()->error("?"),nothing, c) : error("no capture group $c")
                            end,
                            v[3],v[4])
                                      elseif c isa AbstractParser
                Conditional(c,v[3],v[4])
            else
                Conditional(Subroutine(c[2]),v[3],v[4])
            end
        end
push!(pattern, conditional);


# https://www.regular-expressions.info/refbasic.html
dot = alt(
    on_options(Base.PCRE.DOTALL,'.') => AnyChar(), ## todo: allow \n matching context 
    '.' => CharNotIn('\n'), ## todo: allow \n matching context 
    "\\N" => CharNotIn('\n')
)
push!(pattern,dot);

push!(pattern,map(parser,escaped_character));


# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC13
@with_names sequence_with_options = after(
    seq(1,pcre_option_start,':'),AbstractParser) do v
        seq(1,set_options(v..., alternation),')')
    end
push!(pattern,sequence_with_options);

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC27
@with_names backtrack_control = seq(
    2,"(*",
    alt(seq("ACCEPT",opt(seq(2,":",JoinSubstring(rep_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("ACCEPT")); end,
        seq(alt("FAIL","F"),opt(seq(2,":",JoinSubstring(rep_stop(AnyChar(),parser(')')))))) do v; Never(); end,
        seq("PRUNE",opt(seq(2,":",JoinSubstring(rep_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("PRUNE")); end,
        seq("SKIP",opt(seq(2,":",JoinSubstring(rep_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("SKIP")); end,
        seq(opt(parser("MARK")),':',
            JoinSubstring(rep_stop(AnyChar(),parser(')')))) do v; with_log(v[3],Always()); end,
        seq("COMMIT",opt(seq(2,":",JoinSubstring(rep_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("COMMIT")); end,
        seq("THEN",opt(seq(2,":",JoinSubstring(rep_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("THEN")); end,
        ),
    ")");
push!(pattern,backtrack_control);

push!(pattern,map(alt("\\K")) do v throw(UnsupportedError(v)); end);

pcre_parser = seq(AtStart(),alternation,AtEnd()) do v
    CombinedParsers.indexed_captures(v[2])
end

function Regcomb(x)
    try 
        r=parse(pcre_parser,x)
        r === nothing && error("invalid regex")
        r
    catch e
        if e isa UnsupportedError
            println(x,": ",e)
            throw(UnsupportedError(e.message))
        else
            rethrow(e)
        end
    end
end

function Regcomb(x::CombinedParsers.CatStrings)
    try 
        r=parse(pcre_parser,x)
        r === nothing && error("invalid regex")
        r
    catch e
        if e isa UnsupportedError
            println(x,": ",e)
            throw(UnsupportedError(e.message))
        else
            rethrow(e)
        end
    end
end

import CombinedParsers: pcre_options_parser

function Regcomb(x::AbstractString,flags::AbstractString)
    o = parse(pcre_options_parser,flags)
    Regcomb(with_options(o...,x))
end

macro re_str(x)
    esc(quote
        Regcomb($x)
        end)
end

macro re_str(x,flags)
    esc(quote
        Regcomb($x,$flags)
        end)
end


macro test_pcre(pattern,seq,log=false,flags="")
    quote
        let name = string($seq)
            @testset "$name" begin
                pcre=Regex($pattern,$flags)
                pc  =Regcomb($pattern,$flags)
                pcre_m = match(pcre,$seq)
                pc_m = match(pc,$seq)
                if $log
                    @info "testing r\"$($pattern)\" on \"$($seq)\"" pc_m pcre_m
                end
                @test pcre_m == pc_m
            end
        end
    end |> esc
end

