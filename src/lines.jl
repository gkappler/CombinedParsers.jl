
"""
preliminary line type implementation with tokens. todo: make structs!!!!
"""
function line_enclosing(x)
    parts = split(string(variable(x)), "_")
    if lastindex(parts)==2
        if parts[2]=="drawer"
            ( typ = :drawer, env=parts[1], variable = nothing, indent = 0 )
        else
            ( typ = :block, env=parts[2], variable = parts[1], indent = 0 )
        end
    elseif lastindex(parts)==1 && ! (parts[1] in ["whitespace", "list"])
        ( typ = :block, env = parts[1], variable = nothing, indent = 0 )
    else
        ( typ = variable(x), env = nothing, variable = nothing, indent = lastindex(value(x)) )
        ## nothing
    end
end

# line_enclosing(x)
function orgLine(is::Vector{Token}, ts::Vector{String})::Vector{String}
    if isempty(is)
         ts
    else
        l=first(is)
        inner = orgLine(is[2:end], ts)
        e = line_enclosing(l)
        name = e.variable === nothing ? "" : "#+name: " * e.variable * "\n"
        if e.typ == :block
            String[ name, "#+begin_", e.env, "\n",
                    inner...,  "#+end_", e.env, ""
                    ]
        elseif e.typ == :drawer
            String[ ":", e.env, ":\n", inner..., ":END:" ]
        else
            String[ string(l), inner... ]
            # error(string(is))
        end
    end
end

## todo:

@enum IndentStrategy  NewParagraph NewIndent Append
function indentStrategies(prevli::Token, li::Token, suffix::Vector{Token})
    e = line_enclosing(li)
    pe = line_enclosing(prevli)
    if e.typ === :whitespace
        if pe.typ in [:drawer, :block]
            ( NewParagraph, suffix )
        elseif length(value(li)) == length(value(prevli))
            ( Append, suffix )
        else
            ( NewIndent, suffix )
        end
    elseif e.typ == :list
        if pe.typ == :block
            ( NewParagraph, suffix )
        else
            ( NewIndent, suffix )
        end
    elseif e.typ in [:drawer, :block]
        if li == prevli
            nothing            
        else
            ( NewParagraph, suffix )
        end
    end
end


##import Base: join
## export join
## Base.convert(x::TokenString) = Token(:tokenstring, join(x,"\n"))
## Base.join(x::Paragraph) = Token(:paragraph, join(x,"\n"))

using Transducers
using Transducers: Transducer, R_, next, inner, xform

struct Renderer{A} <: Transducers.Transducer
    f::Function
    make_line::Function
    inter_line::Vector{A}
end


using Transducers: start, complete, wrap, unwrap, wrapping, Reduction
function Transducers.start(rf::Reduction{Renderer{A}}, result) where A
    current_indent=Token[Token(:whitespace,"")]
    current_line=A[]
    wrap(rf, ((id_path=ID{String}[], par=0, line=0, token=0, char=0),
              current_indent, current_line),
         start(inner(rf), result))
end

function Transducers.complete(rf::Reduction{Renderer{A}}, result) where A
    (here, current_indent, current_line), iresult = unwrap(rf, result)
    r = xform(rf)
    complete(inner(rf), next( inner(rf), iresult, r.make_line(current_indent, current_line) ))
end

function Transducers.next(rf::Reduction{Renderer{A}}, result, input) where A
    ## @show result, input
    wrapping(rf, result) do (here, current_indent, current_line), iresult
        r = xform(rf)
        line = input
        differing_indent =
            filter( x -> x!==nothing,
                    [ indentStrategies(current_indent[i], line.indent[i], current_indent)
                      for i in 1:min(lastindex(line.indent),lastindex(current_indent)) ]
                    )


        ( strategy, indent_part ) =
            isempty(differing_indent) ? ( Append, [] )  : ( first(differing_indent) )

        ## todo: foldl with point
        
        line_elements =
            vcat(Vector{A}[ r.f( modify(here; token=i), line.tokens[i])
              for i in 1:lastindex(line.tokens)
              ]...)

        nextline = modify(here; line = here.line + 1 )
        ( rresult, indent_::Vector{Token}, tokens_::Vector{A} ) =
            if strategy==Append
                ( iresult
                  , current_indent
                  , if isempty(current_line)
                  line_elements
                  else
                  A[ current_line..., r.inter_line..., line_elements... ]
                  end
                  )
            else# if strategy==NewIndent
                ( next( inner(rf), iresult, r.make_line(indent_part, current_line) )
                  , line.indent
                  , line_elements
                  )
            end        
        (nextline, indent_, tokens_  ), rresult
    end
end





import BasePiracy
import BasePiracy: modify, ID
CharInOrgTrie{T}=NamedTuple{(:id_path, :par, :line, :token, :char), Tuple{Vector{ID{T}}, Int, Int, Int, Int}}
export mapWithIndent, mapWithIndent_

function mapWithIndent(par::T) where {T <: Body}
    join(join.(mapWithIndent(
        Renderer{String}((p,x) -> [ string(x) ], orgLine, []),
        (id_path=ID{String}[], par=i, line=0, token=0, char=0),
        par[i], String[], [Token(:whitespace,"")], String[] )
                for i in 1:length(par)
                ), "\n") * "\n"
end

function mapWithIndent_(par::T) where {T <: Body}
    join([ mapfoldl(
        Renderer{String}((p,x) -> [ string(x) ], join ∘ orgLine, []),
        *,
        par[i] )
      for i in 1:length(par)
      ], "\n")
end

function mapWithIndent(r::Renderer{A}, here::CharInOrgTrie{Ti}, lines::P, past_lines::Vector{A}, current_indent::Vector{Token}, current_line::Vector{A} ) where {Ti, A, P <: Paragraph }
    @assert past_lines !== nothing
    if (isempty(lines))
        vcat(past_lines,
             r.make_line(current_indent, A[ current_line..., r.inter_line... ])
             )
    else
        line = first(lines)
        differing_indent =
            filter( x -> x!==nothing,
                    [ indentStrategies(current_indent[i], line.indent[i], current_indent)
                      for i in 1:min(lastindex(line.indent),lastindex(current_indent)) ]
                    )


        ( strategy, indent_part ) =
            isempty(differing_indent) ? ( Append, [] )  : ( first(differing_indent) )

        ## todo: foldl with point
        
        line_elements =
            vcat(Vector{A}[ r.f( modify(here; token=i), line.tokens[i])
              for i in 1:lastindex(line.tokens)
              ]...)

        nextline = modify(here; line = here.line + 1 )
        ( lines_, indent_::Vector{Token}, tokens_::Vector{A} ) =
            if strategy==Append
                ( past_lines
                  , current_indent
                  , if isempty(current_line)
                  line_elements
                  else
                  A[ current_line..., r.inter_line..., line_elements... ]
                  end
                  )
            else# if strategy==NewIndent
                currentline = r.make_line(indent_part, current_line)
                ( A[ past_lines..., currentline... ]
                  , line.indent
                  , line_elements
                  )
            # elseif strategy==NewParagraph
            #     if isempty(current_line)
            #         (  past_lines 
            #            , line.indent
            #            , line_elements
            #            )
            #     else
            #         ( A[ past_lines...
            #              , r.inter_line...
            #              , r.make_line(indent_part,
            #                            A[ current_line...,
            #                               r.inter_line...
            #                               ] )... ]
            #            , line.indent
            #            , line_elements
            #           )
            #     end
            end
        ##@info "" current_line lines_ indent_ tokens_
        mapWithIndent(r, nextline, lines[2:end], lines_, indent_, tokens_  )
    end
end
