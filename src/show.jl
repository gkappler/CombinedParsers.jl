export regex_escape
@nospecialize



function escape_string_styled(io::IO, s::AbstractString; esc=(), keep = ())
    a = Iterators.Stateful(s)
    for c::AbstractChar in a
        if c in esc
            printstyled(io, '\\'; color=treecolor.pcre_escape)
            printstyled(io, c; color=treecolor.pcre_escaped)
        elseif c in keep
            printstyled(io, c; color=treecolor.pcre_unescaped)
        elseif isascii(c)
            if c == '\0'
                printstyled(io, Base.escape_nul(peek(a)::Union{AbstractChar,Nothing}); color=treecolor.pcre_escaped)
            elseif c == '\e'
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "e"; color=treecolor.pcre_escaped)
            elseif c == '\\'
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "\\"; color=treecolor.pcre_escaped)
            elseif '\a' <= c <= '\r'
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "abtnvfr"[Int(c)-6]; color=treecolor.pcre_escaped)
            elseif isprint(c)
                printstyled(io, c; color=treecolor.pcre_unescaped)
            else
                printstyled(io, "\\x", string(UInt32(c), base = 16, pad = 2); color=treecolor.pcre_escaped)
            end
        elseif !Base.isoverlong(c) && !Base.ismalformed(c)
            if isprint(c)
                printstyled(io, c; color=treecolor.pcre_unescaped)
            else
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                c <= '\x7f'        ? printstyled(io, "x", string(UInt32(c), base = 16, pad = 2); color=treecolor.pcre_escaped) :
                    c <= '\uffff'      ? printstyled(io, "u", string(UInt32(c), base = 16, pad = Base.need_full_hex(peek(a)::Union{AbstractChar,Nothing}) ? 4 : 2); color=treecolor.pcre_escaped) :
                    printstyled(io, "U", string(UInt32(c), base = 16, pad = Base.need_full_hex(peek(a)::Union{AbstractChar,Nothing}) ? 8 : 4); color=treecolor.pcre_escaped)
            end
        else # malformed or overlong
            u = bswap(reinterpret(UInt32, c)::UInt32)
            while true
                printstyled(io, '\\'; color=treecolor.pcre_escape)
                printstyled(io, "x", string(u % UInt8, base = 16, pad = 2); color=treecolor.pcre_escaped)
                (u >>= 8) == 0 && break
            end
        end
    end
end

hasregex(x::CombinedParser) = false
hasregex(x::WrappedAssertion) = hasregex(x.parser)
hasregex(x::WrappedParser) = false
hasregex(x::Union{AtStart,AtEnd}) = true
function Base.show(io::IO, x::CombinedParser)
    if hasregex(x)
        printstyled(io, "re\"", color=treecolor.julia) ##!!
        printstyled(io, regex_string(x), color=treecolor.pcre) ##!!
        printstyled(io, "\"", color=treecolor.julia) ##!!
    else
        mc = ChainableTree(MemoTree(x))
        if get(io,:compact, false)
            printnode(io, mc)
        else
            print_tree(IOContext(io, :compact=>true), mc, indicate_truncation=true, maxdepth=20)
        end
    end
end

## pcre
export regex_string
regex_string(x; kw...) =
    iostring(print_regex,x; kw...)

print_regex(io::IO, x::AbstractTokenParser; kw...) =
    printstyled(io, result_type(x.parser); color = treecolor.textparse)


_format(df::DateFormat{S}) where {S} = S
function print_regex(io::IO, x::AbstractTokenParser{<:TextParse.DateTimeToken}; kw...)
    printstyled(io, _format(x.parser.format); color = treecolor.textparse)
end

print_regex_compact(io,x;
                    parens=("","", treecolor.pcre_structure),
                    compact = get(io,:compact,false),
                    kw...) = 
    if !compact  || isliteralsequence(x)
        printstyled(io, parens[1]; color= parens[3])
        print_regex(io,x;kw...)
        printstyled(io, parens[2]; color= parens[3])
    else
        printstyled(io, children_char; color = parens[3])
    end


print_regex(io::IO, x::AbstractChar; kw...) = escape_string_styled(io, string(x); kw...)
print_regex(io::IO, x::AbstractString; kw...) = escape_string_styled(io, x; kw...)
print_regex(io::IO, x::ConstantParser; kw...) = print_regex(io, x.parser; kw...)
function print_regex(io::IO, x::Either; kw...)
    if !get(io,:compact,false)
        frst=true
        for o in x.options
            frst && printstyled(io, "|"; color = treecolor.pcre_Either)
            print_regex(io,o;kw...)
            frst = false
        end
    else
        printstyled(io, "|"; color = treecolor.pcre_Either)
        printstyled(io, children_char; color = treecolor.children_char)
    end
end


function print_with_constructor(io,x...; color=tree_color(x[1]), kw...)
    length(x)>1 && printstyled(io,x[1:end-1]...; color = color, kw...)
    #get(io,:compact,false) && printstyled(io, " ", constructor_name(x[end]), color = tree_color(x[end]))
end


function tree_color(x)
    cn = string(constructor_name(x))
    if hasproperty(treecolor, Symbol(cn)) 
        getproperty(treecolor, Symbol(cn))
    elseif hasproperty(treecolor, Symbol("pcre_$cn"))
        getproperty(treecolor, Symbol("pcre_$cn"))
    elseif x isa ValueMatcher
        treecolor.pcre_Matcher
    elseif x isa Assertion
        treecolor.pcre_Assertion
    else
        treecolor.julia
    end
end

function print_regex(io::IO, ::TextParse.Numeric{<:Integer}; kw...)
    printstyled(io,"-"; color=color = treecolor.pcre_unescaped,kw...)
    printstyled(io,"?["; color=color = treecolor.pcre_structure,kw...)
    printstyled(io,"[:digit:]"; color=color = treecolor.pcre_unescaped,kw...)
    printstyled(io,"]"; color=color = treecolor.pcre_structure,kw...)
    printstyled(io,"+"; color=color = treecolor.pcre_structure,kw...)
    print_with_constructor(io, x; kw...)
end
function print_regex(io::IO, x::SideeffectParser; kw...)
    print_regex(io,x.parser; kw...)
    print_with_constructor(io, x; kw...)
end


print_regex(io::IO, x::AtStart; kw...) =
    printstyled(io, "^"; color=tree_color(x), kw...)
print_regex(io::IO, x::AtEnd; kw...) =
    printstyled(io, "\$"; color=tree_color(x), kw...)
print_regex(io::IO, x::Never; kw...) =
    printstyled(io, "(*","FAIL",")"; color=tree_color(x))
print_regex(io::IO, x::Always; kw...) = nothing
print_regex(io::IO, x::AnyValue; kw...) =
    printstyled(io, "."; color=tree_color(x), kw...)


needs_parens(x::WrappedParser,c) = ("", "",tree_color(x))
needs_parens(parent::WrappedParser) = needs_parens(parent,parent.parser)
needs_parens(parent::WrappedAssertion) = needs_parens(parent,parent.parser)
#needs_parens(parent::Optional, x::) = ("(",")")
needs_parens(parent::WrappedParser, x::LeafParser) = ("","",tree_color(parent))
needs_parens(x::Atomic, c; kw...) = ("(?>",")",tree_color(x))
needs_parens(x::Transformation, c::ConstantParser; kw...) =
    ("","",treecolor.map)
needs_parens(x::Union{Repeat,Optional}, c::ConstantParser; kw...) =
    if length(c.parser) == 1
        ("","",treecolor.pcre_Repeat)
    else
        #error()
        ("(?>",")",treecolor.pcre_Repeat)
    end
needs_parens(x::PositiveLookahead,c) = ("(?=", ")",tree_color(x))
needs_parens(x::NegativeLookahead,c) = ("(?!", ")",tree_color(x))
needs_parens(x::PositiveLookbehind,c) = ("(?<=", ")",tree_color(x))
needs_parens(x::NegativeLookbehind,c) = ("(?<!", ")",tree_color(x))

function print_regex(io::IO, x::Pair; kw...)
    printstyled(io,x)
end
function print_regex(io::IO, x::WrappedParser; kw...)
    print_regex_compact(io, x.parser; parens = needs_parens(x), compact = false, kw...)
end
function print_regex(io::IO, x::WrappedAssertion; kw...)
    print_regex_compact(io, x.parser; parens = needs_parens(x), compact = false, kw...)
end

function print_regex(io::IO,x::Sequence;kw...)
    if !get(io,:compact,false) || isliteralsequence(x)
        for p in x.parts
            print_regex(io,p;kw...)
        end
    else
        printstyled(io, children_char; color = treecolor.children_char)
    end
end

print_regex(io,x::Union{PositiveLookbehind,NegativeLookbehind}; kw...) =
    print_regex_compact(io, reversed(x.parser); parens = needs_parens(x), kw...)

function print_regex(io::IO, x::Repeat; kw...)
    print_regex_compact(io, x.parser; compact = false, parens = needs_parens(x), kw...)
    printstyled(io,
                if x.range.start == 0
                    if x.range.stop >= Repeat_max
                        "*"
                    else            
                        "{,$(x.range.stop)}"
                    end
                else
                    if x.range.stop >= Repeat_max
                        if x.range.start == 1
                            "+"
                        else
                            "{$(x.range.start),}"
                        end
                    elseif x.range.start==x.range.stop
                        "{$(x.range.start)}"
                    else
                        "{$(x.range.start),$(x.range.stop)}"
                    end
                end; color = treecolor.pcre_Repeat)
end
function print_regex(io::IO, x::Optional; kw...)
    print_regex_compact(io, x.parser; parens = needs_parens(x), kw...)
    printstyled(io,"?"; color = treecolor.pcre_Repeat)
end



print_regex(io::IO, x::FlatMap;kw...)  =  nothing # error("regex determined at runtime!")


function print_regex(io::IO, x::Union{ValueIn,ValueNotIn}; kw...)
    printstyled(io,"["; color=tree_color(x))
    x isa ValueNotIn && printstyled(io,"^"; color=tree_color(x))
    if x.pcre ==""
        _print_bracket(io, x.sets; kw...)
    else
        printstyled(io,x.pcre; color=treecolor.pcre_matcher)
    end
    printstyled(io,"]"; color=tree_color(x))
    
    
end
_print_bracket(io::IO, x; kw...) = print_regex(io,x; kw...) # printstyled(io,x; color= treecolor.pcre_matcher
_print_bracket(io::IO, x::Nothing; kw...) = nothing
_print_bracket(io::IO, x::StepRange; kw...) =
    if x.start == x.stop
        print_regex(io, x.start; kw...)
    else
        printstyled(io, x.start*"-"*x.stop; color=treecolor.pcre_charrange)
    end
_print_bracket(io::IO, x::Union{Vector,Set,Tuple}) = 
    for e in sort(x)
        _print_bracket(io, x; kw...)
    end
_print_bracket(io::IO, x::Function) =
    printstyled(io,"$x(...)"; color=treecolor.match_function)
_print_bracket(io::IO, x::UnicodeClass) =
    if haskey(unicode_abbrev, x.class)
        printstyled(io,"\\p{"; color=treecolor.pcre_structure)
        printstyled(io,"$(unicode_abbrev[x.class])"; color=treecolor.match_function)
        printstyled(io,"}"; color=treecolor.pcre_structure)
    else
        for s in x.class
            _print_bracket(io, UnicodeClass(s))
        end
    end

#regex_inner(x::CombinedParser) = ""

## pcre



print_pipe(io) =
    if !get(io,:compact, false)
        printstyled(io, " |> "; color = treecolor.pipe)
    else
        printstyled(io, " |> "; color = treecolor.pipe)
    end

if VERSION>=v"1.6"
    constructor_name(x) = typeof(x).name.name
else
    constructor_name(x) = typeof(x).name.name
end

"""
    print_constructor(io::IO,x; kw...)

Print constructor pipeline in parser tree node.
"""
print_constructor(io::IO,x; kw...) =
    if x isa CombinedParser
        printstyled(io, constructor_name(x), color = tree_color(x))
    else
    end

function print_constructor(io::IO, x::AbstractTokenParser{<:TextParse.DateTimeToken}; kw...)
    printstyled(io, "DateTimeParser"; color = treecolor.constructor)
end

function print_constructor(io::IO,x::SideeffectParser; kw...)
    c = if x.effect == log_effect
        "with_log(;nomatch=true)"
    elseif x.effect == log_effect_match
        "with_log"
    else
        "with_effect($(x.effect))"
    end
    print(io,"$c")
end
function print_constructor(io::IO, x::NamedParser; kw...)
    if !get(io,:compact, false)
        printstyled(io, "with_name :", color=treecolor.julia)
    end
    printstyled(io, x.name, color=treecolor.name)
end

print_constructor(io::IO,x::ConstantParser; kw...) = nothing

function print_constructor(io::IO,x::Bytes{N}; kw...) where N
    printstyled(io, "$(N) TypedBytes"; color=treecolor.julia)
end

function print_constructor(io::IO,x::Transformation; kw...)
    if !get(io,:compact, false)
        printstyled(io,"map("; color = treecolor.map)
        printstyled(io,x.transform; color = treecolor.julia)
        printstyled(io,")"; color = treecolor.map)
    end
end
function print_constructor(io::IO,x::Transformation{MatchedSubSequence}; kw...)
    if !get(io,:compact, false)
        printstyled(io,"!", color=treecolor.map)
    end
end
function print_constructor(io::IO,x::Transformation{<:Constant}; kw...)
    if !get(io,:compact, false)
        printstyled(io," => ", color=treecolor.map)
        printstyled(IOContext(io, :compact => true),x.transform, color=treecolor.julia)
    end
end

function print_constructor(io::IO,x::Transformation{<:IndexAt}; kw...)
    if !get(io,:compact, false)
        printstyled(io,"[", color=treecolor.map)
        printstyled(io,x.transform.i, color=treecolor.julia)
        printstyled(io,"]", color=treecolor.map)
    end
end
print_constructor(io::IO,x::Assertion; kw...) =
    printstyled(io, constructor_name(x), color = tree_color(x))

function print_constructor(io::IO, x::Substitution; kw...)
    printstyled(io, x.name, color=:red)
    print(io, " call substitute!")
end


@specialize
