import CombinedParsers.Regexp: at_linestart, whitespace, integer, escaped_character, pcre_options
unescaped=map(Repeat_until(AnyChar(), Sequence(Repeat(' '),'\n');
                        wrap=JoinSubstring)) do v
                            join(parse(Repeat(Either(escaped_character,AnyChar())),v))
                        end;
comment_or_empty = Repeat(
    JoinSubstring(Either(Sequence(at_linestart,'#',Repeat_until(AnyChar(),'\n')),
                      Sequence(at_linestart,Repeat_until(whitespace,'\n')))));

@with_names begin
    match_test = Sequence(Repeat1(' '),
                     :sequence => unescaped,
                     :expect => Repeat(Sequence(
                         Repeat(' '),
                         :i => Either(integer,"MK"),':',
                         Repeat(' '),
                         :result => unescaped))
                     );

    testspec = Sequence(
        :pattern => map(
            v -> (with_options(reverse(v)...)),
            after(CharIn("/'\""),Any) do s
            Repeat_until(AnyChar(), Sequence(
                2, s, Optional(pcre_options,default=UInt32(0)),
                Repeat(whitespace), '\n'),
                      true; wrap=JoinSubstring)
            end),
        :test => Repeat(match_test),
        :tests_nomatch => Optional(
            Sequence(2, Optional("\\= Expect no match",Repeat_until(AnyChar(), '\n'; wrap=JoinSubstring)),
                Repeat(Sequence(2,
                        Repeat1(' '),
                        unescaped,
                        Optional(Sequence("No match",
                                Repeat_until(AnyChar(), '\n'; wrap=JoinSubstring)))
                        ))))
    );
end;

tests_parser = Sequence(Repeat(Sequence(comment_or_empty,
                           testspec)),
                   comment_or_empty,
                   AtEnd());



is_expected(pc_match::Nothing,expect) = isempty(expect)
function is_expected(pc_match,expect)
    ##@show expect
    for e in expect
        r = e.result == "<unset>" ? nothing : e.result
        trimstring(r) != trimstring(if e.i==0
                                    pc_match.match
                                    elseif e.i isa Integer
                                    pc_match.captures[e.i]
                                    else ## MK unsupported
                                    r ##""
                                    end) && return false
    end
    return true
end

macro pcre_testset(tt,log=false)
    quote
        let ts = $(tt), name = string(ts.pattern)
            ## println(ts)
            p = (ts.pattern)
            pcre=try
                Regex(p)
            catch
                nothing
            end
            ## pc  =CombinedParsers.indexed_captures(set_options(ts.pattern.flags,parse(alternation,ts.pattern)))
            pc  =Regcomb(p)
            @test pc !== nothing
            ## println(pc)
            ##test_seq = ts.test[1]
            for (i,test_seq) in enumerate(ts.test)
                s = test_seq.sequence
                re_match = pcre === nothing ? nothing : match(pcre,s)
                pc_match = match(pc,s)
                expectations = tuple( ( e.i=>e.result for e in test_seq.expect )... )
                if !is_expected(pc_match,test_seq.expect) || $log
                    @info "testing r\"$name\" on $i, \"$(test_seq.sequence)\"" expectations pc_match re_match
                end
                @test is_expected(pc_match, test_seq.expect)
                if re_match !== nothing
                    !is_expected(re_match,test_seq.expect) && @warn "pcre failed test r\"$name\" on $i, \"$(test_seq.sequence)\"" expectations pc_match re_match
                end
            end
            for (i,s) in enumerate(ts.tests_nomatch)
                if $log
                    @info "testing nomatch r\"$name\" on $i, \"$s\""
                end
                re_match = pcre === nothing ? nothing : match(pcre,s)
                pc_match = match(pc,s)
                @test pc_match === nothing
                if re_match !== nothing
                    @warn "pcre '$(name)' matches $i $s" re_match pc_match
                end
            end
        end
    end |> esc
end

macro test_pcre_str(x)
    quote
        for tt in parse(tests_parser,$x)[1]
            @testset "$(tt[2].pattern)" begin
                @pcre_testset tt[2] true
            end
        end
    end |> esc
end
