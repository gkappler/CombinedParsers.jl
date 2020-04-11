
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
whitespace_string = " \t\U0085\U200E\U200F\U2028\U2029"*"\U2029\U000C\U000B"
whitespace = CharIn(whitespace_string)
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
    with_name(:char_range,Sequence(start,
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE,Repeat),
        '-',
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE,Repeat),
        bracket_char) do v
            if v[1] isa WithOptions && ( v[1].flags & Base.PCRE.CASELESS > 0 )
                cs = convert(Char,v[1]):convert(Char,v[5])
                CharIn(unique([ ( lowercase(x) for x in cs )...,
                                ( uppercase(x) for x in cs )... ]))
            else
                cs = convert(Char,v[1]):convert(Char,v[5])
                CharIn(cs)
            end
        end)

function character_base(base,mind=0,maxd=typemax(Int))
    dig = if base == 16
        hex_digit
    elseif base == 8
        CharIn('0':'7')
    elseif base ==10
        CharIn('0':'9')
    else
        error()
    end
    Repeat(Int,dig,(mind,maxd)) do v
        isempty(v) ? 0 : parse(Int,join(v),base=base)
    end
end

skip_whitespace_on(flags, wrap=identity) =
    with_name(:skip_ws,on_options(
        flags,
        wrap(CharIn(whitespace_string...,'\n'))=>Always()))


bsr = Atomic(Either("\r\n",CharIn('\n','\x0b','\f','\r','\U0085', '\U2028','\U2029'))); # backslash R (BSR)

skip_whitespace_and_comments =
    Repeat(Either(
        skip_whitespace_on(
            Base.PCRE.EXTENDED),
        ## comment
        on_options(
            Base.PCRE.EXTENDED,
            with_name(
                :comment,
                Sequence('#',Repeat(whitespace),
                    Repeat_until(
                        AnyChar(),
                        Sequence(Repeat(whitespace),Either(bsr,AtEnd())),
                        wrap = JoinSubstring
                    )) do v
                with_log(v[3],Always())
                end
            )),
        with_name(
            :comment,
            Sequence(
                "(?#",Repeat(whitespace),
                Repeat_until(
                    AnyChar(),
                    Sequence(Repeat(whitespace),')'),
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
    Sequence(f, ( with_log("$i",e) for (i,e) in enumerate(a) )...)

export UnsupportedError
struct UnsupportedError <: Exception
    message::String
end
Base.showerror(io::IO, e::UnsupportedError) = print(io,"unsupported PCRE syntax ",e.message)

 
hex_digit = CharIn('A':'F','a':'f','0':'9')
integer = Sequence(Optional('-'),character_base(10,1)) do v
    if v[1]===missing
        v[2]
    else
        -v[2]
    end
end

at_linestart = Either(AtStart(),PositiveLookbehind(bsr))
lineend   = Either(AtEnd(),bsr)
at_lineend   = Either(AtEnd(),PositiveLookahead(bsr))


# pattern alternatives
# circumflex and dollar https://www.pcre.org/original/doc/html/pcrepattern.html#SEC6
pattern = Either(
    on_options(Base.PCRE.MULTILINE, '^' => at_linestart),
    '^' => AtStart(),
    on_options(Base.PCRE.MULTILINE, '$' => at_lineend),
    on_options(Base.PCRE.DOLLAR_ENDONLY, '$' => AtEnd()),
    '$' => Either(AtEnd(),
                  PositiveLookahead(Sequence(2,'\n',AtEnd())))
);

# Either allows adding alternatives qith push!, that themselves use the Either object for recursive parsers.

# https://www.regular-expressions.info/refcharacters.html
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC4
char =  Either(
    CharNotIn([ c for c in meta_chars]),
    Sequence(2,'\\', CharIn(meta_chars))) do v
        convert(AbstractParser,v)
    end
@with_names repeatable = Either{AbstractParser}(Any[char])

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5
escape_sequence(stop=AtEnd()) =
    Sequence(2,"\\Q",
        Repeat_until(AnyChar(),
                  Either("\\E",PositiveLookahead(stop)),
                  wrap=JoinSubstring));
push!(pattern,
      map(parser, with_name(:escape_sequence, escape_sequence())));

word=CharIn(UnicodeClass("L","N"),'_')

non_word=CharNotIn(UnicodeClass("L","N"),'_')
non_word_ = Either(non_word,AtStart(),AtEnd())

word_boundary = Either(
    Sequence(PositiveLookbehind(word),PositiveLookahead(non_word_)),
    Sequence(PositiveLookbehind(non_word_),PositiveLookahead(word))
)
    
@with_names simple_assertion =
    Sequence(2,
        '\\',
        Either(
            'A' => AtStart(),
            map_at('G') do v,i
            @warn "limited \\G support: ignoring pcre2 startoffset"
            AtStart()
            end,
            'z' => AtEnd(),
            'Z' => PositiveLookahead(Sequence(Optional(bsr),AtEnd())),
            'b' => word_boundary,
            'B' => NegativeLookahead(word_boundary)
        ))
push!(pattern,simple_assertion);

push!(pattern,parser( "\\R" => bsr ));

name = JoinSubstring(
    Sequence(CharIn('a':'z','A':'Z','_'),
        Repeat(CharIn('0':'9','a':'z','A':'Z','_'))))

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
@with_names backreference = map(
    Either(
        Sequence(2,'\\',Either(
            integer, ## todo: maybe octal char
            Sequence(2,'g',integer),
            Sequence(2,"g{",integer,'}'),
            Sequence(2,"g{",name,'}'),
            Sequence(2,"k<",name,'>'),  # perl
            Sequence(2,"k'",name,'\''), # 
        )),
        Sequence(2,"(?P=",name,')'))) do v
            Backreference(v) do
                ## todo: backreference, if a capture with number (in decimal) is defined,
                ## escaped_character otherwise (if name/index not found)
                #   \ddd      character with octal code ddd, or back reference
                ## todo: error on \g<ddd>
                v isa Integer || error("capture group $v not found!")
                parse(Sequence(character_base(8,1,3),
                          Repeat(AnyChar())) do v
                      Sequence(parser(Char(v[1])),
                          v[2]...)
                      end,
                      "$v")
            end
        end;
push!(repeatable,backreference);


@with_names escaped_character = 
    Sequence(2, '\\',
             Either(
                 'a' => ('\a'), # alarm, that is, the BEL character (hex 07)
                 Sequence('c',AnyChar()) do v  # \cx "control-x", where x is any ASCII character
                 make_control(v[2])
                 end,
                 'e' => '\e',   #  escape (hex 1B)
                 'f' => '\f',   #  form feed (hex 0C)
                 'n' => '\n',   #  linefeed (hex 0A)
                 'r' => '\r',   #  carriage return (hex 0D)
                 't' => '\t',   #  tab (hex 09)
                 '"' => '"',
                 #   \0dd      character with octal code 0dd
                 Sequence('0',character_base(8,0,2)) do v; Char(v[2]); end,
                 #   \ddd      character with octal code ddd, or back reference
                 ## Sequence(Char,character_base(8,3,3), transform=v->(Char(v[1]))),
                 ## see backreference, if a capture with number (in decimal) is defined
                 #   \o{ddd..} character with octal code ddd..
                 Sequence('o','{',character_base(8),'}') do v; Char(v[3]); end,
                 #   \xhh      character with hex code hh
                 Sequence('x','{',character_base(16),'}') do v; Char(v[3]); end,
                 #   \x{hhh..} character with hex code hhh.. (non-JavaScript mode)
                 Sequence('x',character_base(16,0,2)) do v; Char(v[2]); end,
                 #   \uhhhh    character with hex code hhhh (JavaScript mode only)
                 Sequence('h',character_base(16,4,4)) do v; Char(v[2]); end,
                 CharNotIn('Q','E')
             ))


@with_names generic_character_type =
    Sequence(2,
        '\\',
        Either(
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
        ));
push!(repeatable,generic_character_type);



# https://www.regular-expressions.info/posixbrackets.html#class
bracket_char = let bracket_meta_chars = raw"]\^-"
    Either(
        CharNotIn([ c for c in bracket_meta_chars]),
        escaped_character
    )
end;


@with_names bracket=Sequence(
    AbstractParser,
    '[',Optional('^')
    , Repeat(Either(
        bracket_range(']'),
        ']'=>']'),(0,1))
    , Repeat(Either(
        Sequence(
            2,
            "[:",
            Either(
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
            ":]"),
        skip_whitespace_on(Base.PCRE.EXTENDED_MORE,Repeat) => Never(),
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
push!(repeatable,bracket);


# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC17
@with_names repetition = Either(
    "+"=>(1,typemax(Int)),
    "*"=>(0,typemax(Int)),
    "?"=>(0,1),
    Sequence(Tuple{Int,Int},
        "{",integer,
        Optional(Sequence(2,",",Optional(integer, default=typemax(Int)))),"}") do v
    if v[3] isa Missing
    (v[2],v[2])
    else
    (v[2],v[3])
    end::Tuple{Int,Int}
    end
)

@with_names quantified=Sequence(
    AbstractParser,
    skip_whitespace_and_comments,
    repeatable,
    skip_whitespace_and_comments, ## for test 1130, preserve in map?
    Optional(repetition, default=(1,1)),
    skip_whitespace_and_comments,
    Optional(map(v->convert(Char,v),CharIn('+','?'))), # possessive quantifier, strip option
    skip_whitespace_and_comments
) do v
    pat = sSequence(v[2],v[3]...)
    result = if v[4]==(1,1)
        parser(pat)
    elseif v[4]==(0,1)
        Optional(pat)
    else
        Repeat(pat,v[4])
    end
    if v[6] === missing
        result
    elseif v[6]=='+'
        Atomic(result)
    elseif v[6]=='?'
        Lazy(result)
    else
        result
    end
end;
push!(pattern, quantified)


# Sequences and Alternation
@with_names sequence = Repeat(pattern) do v
    length(v) ==1 ? v[1] : Sequence(v...)
end;



alternations = Sequence(
    sequence, Repeat(Sequence(2, '|',sequence))) do v
        AbstractParser[v[1],v[2]...]
    end;

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
@with_names pcre_option_start = Sequence(
    2,
    "(?",
    Either(Sequence(Optional('^'),
                    Either(Sequence(pcre_options,
                                    Optional(Sequence(2, '-',pcre_options), default=UInt32(0))),
                           Sequence(Tuple{UInt32,UInt32},'-',pcre_options) do v
                           (UInt32(0),v[2])
                           end)
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
    Sequence(pcre_option_start,Optional(')')),
    Vector{AbstractParser}) do l
        set_options(l[1]..., l[2] === missing ?  Sequence(1, alternations,')') : alternations)
    end;

option_sequences = map(
    AbstractParser,
    Sequence(
        alternations,
        Repeat(options_alternations))) do v
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
            sEither( ( sSequence(x...) for x in r)... )
        end;
alternation = option_sequences;


# Atomic groups
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC18
@with_names atomic_group=Sequence("(",Either("?>","*atomic:"),alternation,")") do v
    Atomic(v[3])
end;
push!(repeatable,atomic_group);

@with_names captured=Sequence("(",
             Either(Sequence(2,"?<",name,'>'),
                 Sequence(2,"?P<",name,'>'),
                 Sequence(2,"?'",name,"'"),
                 ""),
             alternation,
             ")") do v
                 with_name(v[2],Capture(Symbol(v[2]),v[3]))
             end;
push!(repeatable,captured);



@with_names subpattern=Sequence(
    2,
    "(?:",alternation,")");
push!(repeatable,subpattern);


@with_names lookahead=Sequence(
    2,
    "(",
    Either(Sequence(v -> look_ahead(true,v[2]),
            Either("?=","*positive_lookahead:","*pla:"),alternation),
        Sequence(v -> look_ahead(false,v[2]),
            Either("?!","*negative_lookahead:","*nla:"),alternation)),
    ")");
push!(pattern,lookahead);



@with_names lookbehind=Sequence(
    2,
    "(",
    Either(Sequence(v -> look_behind(true,v[2]),
            Either("?<=","*positive_lookbehind:","*plb:"),alternation),
        Sequence(v -> look_behind(false,v[2]),
            Either("?<!","*negative_lookbehind:","*nlb:"),alternation)),
    ")");
push!(pattern,lookbehind);



# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
@with_names subroutine = Sequence(
    2,"(?",
    Either(Sequence(Either('+','-',""),
            integer) do v
        Subroutine(nothing,Symbol(v[1]),v[2])
        end,
        Sequence(Either('&',"P>"), name) do v 
        Subroutine(Symbol(v[2]),Symbol(""),-1)
        end),
    ')');
push!(repeatable,subroutine);

@with_names resetting_capture_numbers = Sequence(
    "(?|",
    alternation,
    ")") do v
        DupSubpatternNumbers(v[2])
    end;
push!(repeatable,resetting_capture_numbers);


@with_names condition = Either(
    Sequence(2,'(',Either(integer,
                  "DEFINE",
                  Sequence('R',Either(integer,Sequence(2,'&',name))),
                  Sequence(2,'\'',name,'\''),
                  Sequence(2,'<',name,'>'),
                  name),
        ')'),
    lookbehind,
    lookahead)

@with_names conditional = map(
    Sequence("(?",condition,
        sequence,
        Optional(Sequence(2,"|",sequence), default=Always()),
        ")")) do v
            c = v[2]
            if c=="DEFINE"
                Atomic(Either(Always(),v[3])) ## ignore in match
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
push!(repeatable, conditional);


# https://www.regular-expressions.info/refbasic.html
dot = Either(
    on_options(Base.PCRE.DOTALL,'.') => AnyChar(), ## todo: allow \n matching context 
    '.' => CharNotIn('\n'), ## todo: allow \n matching context 
    "\\N" => CharNotIn('\n')
)
push!(repeatable,dot);

push!(repeatable,map(parser,escaped_character));


# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC13
@with_names sequence_with_options = after(
    Sequence(1,pcre_option_start,':'),AbstractParser) do v
        Sequence(1,set_options(v..., alternation),')')
    end
push!(repeatable,sequence_with_options);

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC27
@with_names backtrack_control = Sequence(
    2,"(*",
    Either(Sequence("ACCEPT",Optional(Sequence(2,":",JoinSubstring(Repeat_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("ACCEPT")); end,
        Sequence(Either("FAIL","F"),Optional(Sequence(2,":",JoinSubstring(Repeat_stop(AnyChar(),parser(')')))))) do v; Never(); end,
        Sequence("PRUNE",Optional(Sequence(2,":",JoinSubstring(Repeat_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("PRUNE")); end,
        Sequence("SKIP",Optional(Sequence(2,":",JoinSubstring(Repeat_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("SKIP")); end,
        Sequence(Optional(parser("MARK")),':',
            JoinSubstring(Repeat_stop(AnyChar(),parser(')')))) do v; with_log(v[3],Always()); end,
        Sequence("COMMIT",Optional(Sequence(2,":",JoinSubstring(Repeat_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("COMMIT")); end,
        Sequence("THEN",Optional(Sequence(2,":",JoinSubstring(Repeat_stop(AnyChar(),parser(')')))))) do v; throw(UnsupportedError("THEN")); end,
        ),
    ")");
push!(pattern,backtrack_control);

push!(pattern,map(Either("\\K")) do v throw(UnsupportedError(v)); end);

export pcre_parser, @re_str, Regcomb
pcre_parser = Sequence(AtStart(),alternation,AtEnd()) do v
    indexed_captures(v[2])
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

