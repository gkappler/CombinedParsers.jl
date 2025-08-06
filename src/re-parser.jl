import ..CombinedParsers: whitespace_char, at_linestart, at_lineend, horizontal_space_char, vertical_space_char, hex_digit
import ..CombinedParsers: bsr, word, word_char, non_word_char, non_word, word_boundary
import ..CombinedParsers: Repeat_max, _integer, integer
## TODO:
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC2
# affect . ^ $
# alt("(*CR)" => with_doc(CharNotIn('\r'), "carriage return"),
#     "(*LF)" => with_doc(CharNotIn('\r'), "linefeed"),
#     "(*CRLF)" => with_doc("carriage return, followed by linefeed"),
#     "(*ANYCRLF)" => with_doc("any of the three above"),
#     "(*ANY)" => with_doc("all Unicode newline sequences"))




skip_whitespace_on(flags, wrap=identity) =
    on_options(
        flags,
        wrap(CharIn(whitespace_char,'\n'))=>Always())

make_control(c) =
    let ui=UInt32(uppercase(c))
        ui > 127 && error("no control-$c character")
        Char(xor(ui, 0x40))
    end


seq_log(f::Function,a...) =
    map(f, Sequence(( with_log("$i",e) for (i,e) in enumerate(a) )...))

export UnsupportedError
struct UnsupportedError <: Exception
    message::String
end
Base.showerror(io::IO, e::UnsupportedError) = print(io,"unsupported PCRE syntax ",e.message)

const pcre_boundaries =
    with_name(
        :pcre_boundaries,
        map(IndexAt(2),Sequence(
                  '\\',
                  Either(
                      'A' => AtStart(),
                      map(parser('G')) do v
                          @warn "limited \\G support: ignoring pcre2 startoffset"
                          AtStart()
                      end,
                      'z' => AtEnd(),
                      'Z' => PositiveLookahead(Sequence(Optional(bsr, default=missing),AtEnd())),
                      'b' => word_boundary,
                      'B' => NegativeLookahead(word_boundary)
                  ))))

const escaped_character = 
    with_name(
        :escaped_character,
        mSequence(
            2, '\\',
            Either(
                'a' => ('\a'), # alarm, that is, the BEL character (hex 07)
                mSequence('c',AnyChar()) do v  # \cx "control-x", where x is any ASCII character
                    make_control(v[2])
                end,
                'e' => '\e',   #  escape (hex 1B)
                'f' => '\f',   #  form feed (hex 0C)
                'n' => '\n',   #  linefeed (hex 0A)
                'r' => '\r',   #  carriage return (hex 0D)
                't' => '\t',   #  tab (hex 09)
                '"' => '"',
                #   \0dd      character with octal code 0dd
                map(Sequence('0',integer_base(8,0,2))) do v; Char(v[2]); end,
                #   \ddd      character with octal code ddd, or back reference
                ## Sequence(integer_base(8,3,3), transform=v->(Char(v[1]))),
                ## see backreference, if a capture with number (in decimal) is defined
                #   \o{ddd..} character with octal code ddd..
                map(Sequence('o','{',integer_base(8),'}')) do v; Char(v[3]); end,
                #   \x{hhh..} character with hex code hhh.. (non-JavaScript mode)
                map(Sequence('x','{',integer_base(16),'}')) do v; Char(v[3]); end,
                #   \xhh      character with hex code hh
                map(Sequence('x',integer_base(16,0,2))) do v; Char(v[2]); end,
                #   \uhhhh    character with hex code hhhh (JavaScript mode only)
                map(Sequence('u',integer_base(16,4,4))) do v; Char(v[2]); end,
                CharNotIn('Q','E')
            )))



const skip_whitespace_and_comments =
    with_name(
        :skip_whitespace_and_comments,
        map(Repeat(Either(
            skip_whitespace_on(
                Base.PCRE.EXTENDED),
            ## comment
            on_options(
                Base.PCRE.EXTENDED,
                with_name(
                    :comment_extended,
                    mSequence('#',Repeat(whitespace_char),
                              Repeat_until(
                                  AnyChar(),
                                  Sequence(Repeat(whitespace_char),
                                           Either(bsr,AtEnd())),
                                  wrap = MatchedSubSequence
                              )) do v
                                  with_log(v[3],Always())
                              end
                )),
            with_name(
                :comment,
                mSequence(
                    "(?#",Repeat(whitespace_char),
                    Repeat_until(
                        AnyChar(),
                        Sequence(Repeat(whitespace_char),')'),
                        wrap = MatchedSubSequence
                    )) do v
                        with_log(v[3],Always())
                    end)))) do v
                        [a for a in v if !isa(a,Always)]
                    end);

escape_sequence(stop=AtEnd()) =
    with_name(:escape_sequence, mSequence(2,"\\Q",
                                          Repeat_until(AnyChar(),
                                                       Either("\\E",PositiveLookahead(stop)),
                                                       wrap=MatchedSubSequence));)

const name = with_name(
    :name,
    MatchedSubSequence(
        Sequence(CharIn('a':'z','A':'Z','_'),
                 Repeat(CharIn('0':'9','a':'z','A':'Z','_')))))

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
const backreference = with_name(:backreference,map(
    Either(
        mSequence(2,'\\',Either(
            _integer(3), ## todo: maybe octal char
            mSequence(2,'g',_integer(3)),
            mSequence(2,"g{",_integer(3),'}'),
            mSequence(2,"g{",name,'}'),
            mSequence(2,"k<",name,'>'),  # perl
            mSequence(2,"k'",name,'\''), # 
        )),
        mSequence(2,"(?P=",name,')'))) do v
                                Backreference(v) do
                                    ## todo: backreference, if a capture with number (in decimal) is defined,
                                    ## escaped_character otherwise (if name/index not found)
                                    #   \ddd      character with octal code ddd, or back reference
                                    ## todo: error on \g<ddd>
                                    v isa Integer || error("capture group $v not found!")
                                    parse(mSequence(integer_base(8,1,3),
                                                    Repeat(AnyChar())) do v
                                                        sSequence(parser(Char(v[1])),
                                                                  v[2]...)
                                                    end,
                                          "$v")
                                end
                                end);

const char = let meta_chars = raw"\^$.[|()?*+{"
    mEither(
        CharNotIn(meta_chars),
        mSequence(2,'\\', CharIn(meta_chars))) do v
            convert(CombinedParser,v)
        end
end

const generic_character_type =
    with_name(
        :generic_character_type,
        mSequence(
            2,
            '\\', Either(
                Either(
                    # "any decimal digit"),
                    'd' => CharIn("\\d",'0':'9'),
                    # "any character that is not a decimal digit"),
                    'D' => CharNotIn("\\D",'0':'9'),
                    # "any horizontal white space character"),
                    'h' => CharIn("\\h",horizontal_space_char),
                    # "any character that is not a horizontal white space character"),
                    'H' => CharNotIn("\\H",horizontal_space_char),
                    # "any white space character"),
                    's' => CharIn("\\s",horizontal_space_char,vertical_space_char),
                    # "any character that is not a white space character"),
                    'S' => CharNotIn("\\S",horizontal_space_char,vertical_space_char),
                    # "any vertical white space character"),
                    'v' => CharIn("\\v",vertical_space_char),
                    # "any character that is not a vertical white space character"),
                    'V' => CharNotIn("\\V",vertical_space_char),
                    # "any "word" character"),
                    'w' => word_char,
                    # "any "non-word" character"),
                    'W' => non_word_char,
                ),
                mSequence(2,"p{",
                          Either(Dict(string(k)=>CharIn("\\p{$k}",UnicodeClass(v[3]))
                                      for (k,v) in CombinedParsers.unicode_classes)) ,
                          '}')
            )));

const character_class = 
    Either([
        "alpha" => CharIn(UnicodeClass("L")),
        "lower" => CharIn(UnicodeClass("Ll")),
        "upper" => CharIn(UnicodeClass("Lu")),
        "word"  => CharIn(UnicodeClass("L","Nl","Nd","Pc")),
        "digit" => CharIn(UnicodeClass("Nd")),
        "xdigit" => hex_digit,
        "alnum" => CharIn(UnicodeClass("L","N")), # Xan
        "blank" => CharIn(UnicodeClass("Zs"),'\t'),
        "cntrl" => CharIn(UnicodeClass("Cc")),
        "graph" => CharNotIn(UnicodeClass("Z","C")),
        "print" => CharIn(UnicodeClass("C")),
        "punct" => CharIn(UnicodeClass("P")),
        "space" => CharIn(UnicodeClass("Z"),'\t','\r','\n','\v','\f'),
    ])



const bracket_char = let bracket_meta_chars = raw"]\^-"
    with_name(:bracket_char,
              Either(
                  CharNotIn(bracket_meta_chars),
                  "\\b" => '\x08',
                  mSequence('\\',integer_base(8,1,3)) do v
                      Char(v[2])
                  end,
                  escaped_character
              ))
end;

# https://www.regular-expressions.info/posixbrackets.html#class
# todo: set pcre string of CharIn/CharNotIn when multi-transform is implemented
const pcre_bracket = begin 
    bracket_range(start) =
        with_name(:char_range,
                  mSequence(start,
                            skip_whitespace_on(Base.PCRE.EXTENDED_MORE,Repeat),
                            '-',
                            skip_whitespace_on(Base.PCRE.EXTENDED_MORE,Repeat),
                            bracket_char) do v
                                if v[1] isa CharWithOptions && ( v[1].flags & Base.PCRE.CASELESS > 0 )
                                    cs = convert(Char,v[1]):convert(Char,v[5])
                                    CharIn("$(v[1])-$(v[5])",unique([ ( lowercase(x) for x in cs )...,
                                                                      ( uppercase(x) for x in cs )... ]))
                                else
                                    cs = convert(Char,v[1]):convert(Char,v[5])
                                    CharIn("$(v[1])-$(v[5])",cs)
                                end
                            end)
    with_name(:pcre_bracket,
              mSequence(
                  CombinedParser,
                  '[',Optional('^', default = missing)
                  , Repeat(0,1,Either(
                      bracket_range(']'),
                      ']'=>']'))
                  , Repeat(Either(
                      mSequence(2,   "[:",  character_class,  ":]"),
                      skip_whitespace_on(Base.PCRE.EXTENDED_MORE,Repeat) => Never(),
                      "\\E" => Never(),
                      map(v->CharIn(v),escape_sequence()),
                      generic_character_type,
                      bracket_range(bracket_char),
                      map(v->convert(CombinedParser,v),bracket_char),
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
              )
end

const repetitions =
    with_name(:repetition, Either(
        '+' => 1:Repeat_max,
        '*' => 0:Repeat_max,
        '?' => 0:1,
        mSequence(
            '{',
            integer(),
            Optional(mSequence(
                2,',',
                Optional(integer(), default=Repeat_max)),
                     default=missing),
            '}') do v
                if v[3] isa Missing
                    v[2]:v[2]
                else
                    v[2]:v[3]
                end::UnitRange{Int}
            end
    ))

throw_unsupported(p) =
    map(String, map(v -> throw(UnsupportedError(v)), p))
throw_unsupported(p,s) =
    map(String, map(v -> throw(UnsupportedError(s)), p));

# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC17
quantified(repeatable) =
    with_name(:quantified,
              map(
                  Sequence(
                      repeatable,
                      skip_whitespace_and_comments, ## for test 1130, preserve in map?
                      Optional(repetitions, default=1:1),
                      skip_whitespace_and_comments,
                      Optional(CharIn('+','?')), # possessive quantifier, strip option
                  )) do v
                      pat = sSequence(v[1],v[2]...)
                      result = if v[3] == 1:1
                          parser(pat)
                      elseif v[3]==0:1
                          Optional(pat, default=missing)
                      else
                          Repeat(v[3],pat)
                      end
                      if v[5] === missing
                          result
                      elseif v[5]=='+'
                          Atomic(result)
                      elseif v[5]=='?'
                          Lazy(result)
                      else
                          result
                      end::CombinedParser
                  end)


# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC27
const backtrack_control =
    with_name(:backtrack_control,
              mSequence(
                  2,"(*",
                  Either(
                      throw_unsupported(
                          Sequence("ACCEPT",Optional(mSequence(2,":",MatchedSubSequence(Repeat_stop(AnyChar(),parser(')')))))),
                          "ACCEPT"),
                      mSequence(Either("FAIL","F"),Optional(mSequence(2,":",MatchedSubSequence(Repeat_stop(AnyChar(),parser(')')))))) do v; Never(); end,
                      throw_unsupported(
                          Sequence("PRUNE",Optional(mSequence(2,":",MatchedSubSequence(Repeat_stop(AnyChar(),parser(')')))))),
                          "PRUNE"),
                      throw_unsupported(
                          Sequence("SKIP",Optional(mSequence(2,":",MatchedSubSequence(Repeat_stop(AnyChar(),parser(')')))))),
                          "SKIP"),
                      mSequence(Optional(parser("MARK")),':',
                                MatchedSubSequence(Repeat_stop(AnyChar(),parser(')')))) do v;
                                    with_log(v[3],Always());
                                end,
                      throw_unsupported(
                          Sequence("COMMIT",Optional(mSequence(2,":",MatchedSubSequence(Repeat_stop(AnyChar(),parser(')')))))),
                          "COMMIT"),
                      throw_unsupported(
                          Sequence("THEN",Optional(mSequence(2,":",MatchedSubSequence(Repeat_stop(AnyChar(),parser(')')))))),
                          "THEN")),
                  ")"))



function alternation(sequence)

    alternations = with_name(:alternations,mSequence(
        sequence, Repeat(mSequence(2, '|',sequence))) do v
                             CombinedParser[v[1],v[2]...]
                             end);

    @with_names options_alternations = after(
        Sequence("(?",pcre_options,NegativeLookahead(':'),Optional(')')),
        Vector{CombinedParser}) do l
            #@show l
            set_options(l[2]..., l[3] === missing ?  mSequence(1, alternations,')') : alternations)
        end;
    with_name(:alternation, map(
        CombinedParser,
        Sequence(
            alternations,
            Repeat(options_alternations))) do v
                r = Any[ CombinedParser[e] for e in v[1] ]
                ro = v[2]
                for i in 1:length(ro)
                    length(ro[i])>0 && push!(r[end],popfirst!(ro[i]))
                    for x in ro[i]
                        ## if length(ro[i])>0
                        ## @show r[end],x
                        push!(r,CombinedParser[ x ])
                    end
                end
                Either( ( sSequence(x...) for x in r)... ; simplify=true)
              end);
end



# Atomic groups
# https://www.pcre.org/original/doc/html/pcrepattern.html#SEC18
function in_parentheses(sequence)

    lookahead =
        with_name(:lookahead,
                  Either(mSequence(v -> Lookahead(true,Atomic(v[2]))::CombinedParser,
                                   Either("?=","*positive_lookahead:","*pla:"),alternation(sequence)),
                         mSequence(v -> Lookahead(false,v[2])::CombinedParser,
                                   Either("?!","*negative_lookahead:","*nla:"),alternation(sequence))))





    lookbehind=
        with_name(
            :lookbehind,Either(mSequence(v -> Lookbehind(true,Atomic(v[2]))::CombinedParser,
                                         Either("?<=","*positive_lookbehind:","*plb:"),alternation(sequence)),
                               mSequence(v -> Lookbehind(false,v[2])::CombinedParser,
                                         Either("?<!","*negative_lookbehind:","*nlb:"),alternation(sequence))));
    
    mSequence(
        2,"(",
        Either(
            with_name(:atomic_group,
                      mSequence(Either("?>","*atomic:"),alternation(sequence)) do v
                          Atomic(v[2])
                      end),
            with_name(:captured,
                      mSequence(
                          Either(mSequence(2,"?<",name,'>'),
                                 mSequence(2,"?P<",name,'>'),
                                 mSequence(2,"?'",name,"'"),
                                 ""),
                          alternation(sequence)) do v
                              with_name(v[1],Capture(Symbol(v[1]),v[2]))::CombinedParser
                          end),
            with_name(
                :subpattern,
                mSequence(2,"?:",alternation(sequence))),
            lookahead,
            lookbehind,

            # https://www.pcre.org/original/doc/html/pcrepattern.html#SEC19
            with_name(
                :subroutine,
                mSequence(
                    2,"?",
                    Either(mSequence(Either('+','-',""),
                                     integer()) do v
                                         Subroutine(nothing,Symbol(v[1]),v[2])
                                     end,
                           mSequence(Either('&',"P>"), name) do v 
                               Subroutine(Symbol(v[2]),Symbol(""),-1)
                           end))),
            with_name(
                :resetting_capture_numbers,
                mSequence(
                    "?|",
                    alternation(sequence)) do v
                        DupSubpatternNumbers(v[2])
                    end),
            with_name(
                :conditional,
                map(Sequence(
                    "?",
                    with_name(
                        :condition,
                        Either(
                            mSequence(
                                2,
                                '(',
                                Either(
                                    integer(),
                                    "DEFINE",
                                    throw_unsupported(
                                        Sequence(
                                            'R', ## TODO
                                            Either(
                                                integer(),
                                                mSequence(2,'&',name),
                                                Always())), 
                                        "checking for pattern recursion"),
                                    mSequence(2,'\'',name,'\''),
                                    mSequence(2,'<',name,'>'),
                                    name),
                                ')'),
                            mSequence(2,"(",lookbehind,")"),
                            mSequence(2,"(",lookahead,")"))
                    ),
                    sequence,
                    Optional(mSequence(2,"|",sequence), default=Always()))) do v
                        c = v[2]
                        if c=="DEFINE"
                            Atomic(Either(Always(),v[3])) ## ignore in match
                        elseif c isa Union{Integer,AbstractString}
                            Conditional(Backreference(c) do
                                            c == "R" && return Subroutine()
                                            c isa Integer ? Backreference(()->error("?"),nothing, c) : error("no capture group $c")
                                        end,
                                        v[3],v[4])
                        elseif c isa CombinedParser
                            Conditional(c,v[3],v[4])
                        else
                            Conditional(Subroutine(c[2]),v[3],v[4])
                        end::CombinedParser
                    end),
            # https://www.pcre.org/original/doc/html/pcrepattern.html#SEC13
            with_name(
                :sequence_with_options,
                after(
                    mSequence(2,'?',pcre_options,':'),CombinedParser) do v
                        set_options(v..., alternation(sequence))
                    end)),
        ")")
end

splat_or(v) = (isempty(v) ? 0x00000000 : (|(v...)))::UInt32
const  pcre_option_char = begin
    @with_names pcre_option = 
        Either(
            # with_name(:MARK, "mark" => UInt32(0)),
            # with_name(:aftertext, "aftertext" => UInt32(0)),
            with_name(:DUPNAMES, "dupnames" => Base.PCRE.DUPNAMES),
            # with_name(:no_start_optimize, "no_start_optimize" => UInt32(0)),
            # with_name(:subject_literal, "subject_literal" => UInt32(0)),
            # "jitstack=256" => UInt32(0),
            with_name(:EXTENDED_MORE, "xx" => Base.PCRE.EXTENDED_MORE),
            with_name(:CASELESS, 'i' => Base.PCRE.CASELESS),
            with_name(:MULTILINE, 'm' => Base.PCRE.MULTILINE),
            with_name(:NO_AUTO_CAPTURE, 'n' => Base.PCRE.NO_AUTO_CAPTURE),
            with_name(:UNGREEDY, 'U' => Base.PCRE.UNGREEDY),
            with_name(:DUPNAMES, 'J' => Base.PCRE.DUPNAMES),
            with_name(:DOTALL, 's' => Base.PCRE.DOTALL),
            with_name(:EXTENDED, 'x' => Base.PCRE.EXTENDED),
            # 'g' => UInt32(0),
            with_name(:BINCODE, 'B' => UInt32(0)), # bincode
            with_name(:INFO, 'I' => UInt32(0)) # info
        );
    map(splat_or,Repeat(map(IndexAt(1),Sequence(pcre_option,Optional(',')))))
end

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
const pcre_options = with_name(:pcre_options, 
                               Atomic(Either(mSequence(Optional('^'),
                                                       Either(Sequence(pcre_option_char,
                                                                       Optional(mSequence(2, '-',pcre_option_char), default=UInt32(0))),
                                                              mSequence(Tuple{UInt32,UInt32},'-',pcre_option_char) do v
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
                                             )));

"""

```jldoc
julia> CombinedParsers.Regexp.character_class
🗄 Sequence |> map(#57)
├─ \\[\\: 
├─ |🗄 Either
│  ├─ alpha  => [\\p{L}] ValueIn
│  ├─ lower  => [\\p{Ll}] ValueIn
│  ├─ upper  => [\\p{Lu}] ValueIn
│  ├─ word  => [\\p{L}\\p{Nl}\\p{Nd}\\p{Pc}] ValueIn
│  ├─ digit  => [\\p{Nd}] ValueIn
│  ├─ xdigit  => [[:xdigit:]] ValueIn
│  ├─ alnum  => [\\p{L}\\p{N}] ValueIn
│  ├─ blank  => [\\t\\p{Zs}] ValueIn
│  ├─ cntrl  => [\\p{Cc}] ValueIn
│  ├─ graph  => [^\\p{Z}\\p{C}] ValueNotIn
│  ├─ print  => [\\p{C}] ValueIn
│  ├─ punct  => [\\p{P}] ValueIn
│  └─ space  => [\\r\\v\\n\\f\\t\\p{Z}] ValueIn
└─ \\:\\] 
::CombinedParsers.ValueMatcher
```

TODO:
> By default, characters with values greater than 128 do not match any of the POSIX character classes. However, if the PCRE_UCP option is passed to pcre_compile(), some of the classes are changed so that Unicode character properties are used. This is achieved by replacing certain POSIX classes by other sequences, as follows:
- [:alnum:]  becomes  \\p{Xan}
- [:alpha:]  becomes  \\p{L}
- [:blank:]  becomes  \\h
- [:digit:]  becomes  \\p{Nd}
- [:lower:]  becomes  \\p{Ll}
- [:space:]  becomes  \\p{Xps}
- [:upper:]  becomes  \\p{Lu}
- [:word:]   becomes  \\p{Xwd}

"""
const pcre_parser = begin
    #(*NO_AUTO_POSSESS)
    #(*LIMIT_MATCH=d)
    #(*LIMIT_RECURSION=d)

    # https://www.pcre.org/original/doc/html/pcrepattern.html#SEC5

    # https://www.regular-expressions.info/refcharacters.html
    # https://www.pcre.org/original/doc/html/pcrepattern.html#SEC4
    @with_names repeatable = 
        map(CombinedParser,
            Either(Any[
                char,
                on_options(
                    Base.PCRE.CASELESS,
                    map(p->set_options(Base.PCRE.CASELESS,p),
                        backreference)
                ),
                backreference,
                generic_character_type,
                pcre_bracket,
                # https://www.regular-expressions.info/refbasic.html
                with_name(:dot,Either(
                    on_options(Base.PCRE.DOTALL,'.') => AnyChar(), ## todo: allow \n matching context 
                    '.' => CharNotIn('\n'), ## todo: allow \n matching context 
                    "\\N" => CharNotIn('\n')
                )),
                map(parser,escaped_character)
            ]))




    # Sequences and Alternation
    @with_names sequence = map(Repeat(mSequence(
        2,
        skip_whitespace_and_comments,
        Either(
            # circumflex and dollar https://www.pcre.org/original/doc/html/pcrepattern.html#SEC6
            Any[ on_options(Base.PCRE.DOLLAR_ENDONLY, '$' => AtEnd()),
                 on_options(Base.PCRE.MULTILINE,
                            Either('^' => at_linestart,
                                   '$' => at_lineend)),
                 parser('^' => AtStart()),
                 parser('$' => Either(AtEnd(),
                                      PositiveLookahead(mSequence(2,'\n',AtEnd())))),
                 map(parser, escape_sequence()),
                 pcre_boundaries,
                 parser( "\\R" => bsr ),
                 throw_unsupported(parser("\\K")),
                 quantified(repeatable),
                 backtrack_control
                 ]
        ),
        skip_whitespace_and_comments))) do v
            length(v) ==1 ? v[1] : Sequence(v...)
        end;



    push!(repeatable, in_parentheses(sequence))



    mSequence(AtStart(),alternation(sequence),AtEnd()) do v
        ParserWithCaptures(v[2])
    end
end


export pcre_parser, Regcomb, parse_options
export @pcre
function Regcomb(x, _flags=""; kw...)
    try
        s = _flags == "" ? x : with_options(parse_options(_flags)...,x)
        r=parse(pcre_parser,s; kw...)
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

"""
    parse_options(options::AbstractString)

Return PCRE option mask parsed from `options`.

Parser for `flags` in [`@re_str`](@ref).

```jldoctest
julia> CombinedParsers.Regexp.pcre_options_parser
🗄 Sequence[2]
├─ ^ AtStart
├─ 🗄* Sequence[1] |> Repeat |> map(splat_or)
│  ├─ |🗄 Either
│  │  ├─ dupnames  => 0x00000040 |> with_name(:DUPNAMES)
│  │  ├─ xx  => 0x01000000 |> with_name(:EXTENDED_MORE)
│  │  ├─ i  => 0x00000008 |> with_name(:CASELESS)
│  │  ├─ m  => 0x00000400 |> with_name(:MULTILINE)
│  │  ├─ n  => 0x00002000 |> with_name(:NO_AUTO_CAPTURE)
│  │  ├─ U  => 0x00040000 |> with_name(:UNGREEDY)
│  │  ├─ J  => 0x00000040 |> with_name(:DUPNAMES)
│  │  ├─ s  => 0x00000020 |> with_name(:DOTALL)
│  │  ├─ x  => 0x00000080 |> with_name(:EXTENDED)
│  │  ├─ B  => 0x00000000 |> with_name(:BINCODE)
│  │  └─ I  => 0x00000000 |> with_name(:INFO)
│  └─ ,? |missing
└─ \$ AtEnd
::UInt32

```
"""
function parse_options(options::AbstractString)
    flags = tryparse(padded(pcre_options),options)
    if flags === nothing
        throw(UnsupportedError("options $options"))
    else
        flags
    end
end


macro pcre()
    quote
        if true || !@isdefined(__pcre)
            __pcre = CombinedParsers.Regexp.pcre_parser
        end
        if true || !@isdefined(__pcre_options_parser)
            __pcre_options_parser = CombinedParsers.padded(CombinedParsers.Regexp.pcre_options)
        end
        function Regcomb(x)
            try 
                r=parse(__pcre,x)
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

        """
            parse_options(options::AbstractString)

        Return PCRE option mask parsed from `options`.

        Parser for `flags` in [`@re_str`](@ref).

        ```jldoctest
        julia> CombinedParsers.Regexp.pcre_options_parser
        🗄 Sequence[2]
        ├─ ^ AtStart
        ├─ 🗄* Sequence[1] |> Repeat |> map(splat_or)
        │  ├─ |🗄 Either
        │  │  ├─ dupnames  => 0x00000040 |> with_name(:DUPNAMES)
        │  │  ├─ xx  => 0x01000000 |> with_name(:EXTENDED_MORE)
        │  │  ├─ i  => 0x00000008 |> with_name(:CASELESS)
        │  │  ├─ m  => 0x00000400 |> with_name(:MULTILINE)
        │  │  ├─ n  => 0x00002000 |> with_name(:NO_AUTO_CAPTURE)
        │  │  ├─ U  => 0x00040000 |> with_name(:UNGREEDY)
        │  │  ├─ J  => 0x00000040 |> with_name(:DUPNAMES)
        │  │  ├─ s  => 0x00000020 |> with_name(:DOTALL)
        │  │  ├─ x  => 0x00000080 |> with_name(:EXTENDED)
        │  │  ├─ B  => 0x00000000 |> with_name(:BINCODE)
        │  │  └─ I  => 0x00000000 |> with_name(:INFO)
        │  └─ ,? |missing
        └─ \$ AtEnd
        ::UInt32

        ```
        """
        function parse_options(options::AbstractString)
            flags = tryparse(__pcre_options_parser,options)
            if flags === nothing
                throw(UnsupportedError("options $options"))
            else
                flags
            end
        end

        function Regcomb(x::AbstractString,flags::AbstractString)
            o = parse_options(flags)
            Regcomb(with_options(o...,x))
        end
        macro $(Symbol("re_str"))(x)
            Regcomb(x)
        end
        macro $(Symbol("re_str"))(x,flags)
            Regcomb(x,flags)
        end
    end |> esc
end


"""
    @re_str(x,flags)

Construct a `ParserWithCaptures` from PCRE regex syntax, such as `re"^[a-z]*\$"`, without interpolation and unescaping (except for
quotation mark `"` which still has to be escaped). 
Plug-in replacement for PCRE string macro @r_str.

The regex also accepts one or more flags, listed after the ending quote, to change its behaviour:

- `i` enables case-insensitive matching
- `m` treats the `^` and `\$` tokens as matching the start and end of individual lines, as
  opposed to the whole string.
- `s` allows the `.` modifier to match newlines.
- `x` enables "comment mode": whitespace is ignored except when escaped with `\\`, and `#`
  is treated as starting a comment.
- `a` disables `UCP` mode (enables ASCII mode). By default `\\B`, `\\b`, `\\D`, `\\d`, `\\S`,
  `\\s`, `\\W`, `\\w`, etc. match based on Unicode character properties. With this option,
  these sequences only match ASCII characters.
- `xx` enables "extended comment mode": whitespace in bracket character matchers are ignored.


```jldoctest
julia> re"a|c"i
|🗄 Either
├─ [aA] ValueIn
└─ [cC] ValueIn
::Char

julia> re"a+c"
🗄 Sequence
├─ a+  |> Repeat
└─ c
::Tuple{Vector{Char}, Char}
```

See also [`Regcomb`](@ref), [`parse_options`](@ref).
"""


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


# using PrecompileTools: @setup_workload, @compile_workload    # this is a small dependency

# @setup_workload begin
#     # Putting some things in `@setup_workload` instead of `@compile_workload` can reduce the size of the
#     # precompile file and potentially make loading faster.
#     @compile_workload begin
#         __pcre = CombinedParsers.Regexp.pcre_parser
#         parse(__pcre,"a+b?c{1,2}(efg[a-z]\\d)")
#         tryparse(__pcre,"a)"; trace=true)
#     end
# end
