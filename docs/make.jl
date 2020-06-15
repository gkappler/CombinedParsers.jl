push!(LOAD_PATH,"../src")
using Documenter: Documenter, makedocs, deploydocs, doctest, DocMeta
using CombinedParsers
using CombinedParsers.Regexp
using Test
using Literate

docdir = joinpath(dirname(pathof(CombinedParsers)),"../docs/src/")
mandir = joinpath(docdir,"man")
DocMeta.setdocmeta!(CombinedParsers, :DocTestSetup, quote
    using CombinedParsers
    using CombinedParsers.Regexp
end; recursive=true)

for f in [ "example-person.jl", "example-number-ranges.jl" ]
    Literate.markdown(joinpath(mandir,f), mandir,
                      repo_root_url="https://github.com/gkappler/CombinedParsers.jl/docs",
                      codefence = "```@repl session" => "```")
end

makedocs(;
    modules=[CombinedParsers],
    authors="Gregor Kappler",
    repo="https://github.com/gkappler/CombinedParsers.jl/blob/{commit}{path}#L{line}",
    sitename="CombinedParsers.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://gkappler.github.io/CombinedParsers.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Manual" => [
            "Overview" => "man/guide.md",
            "User Guide" => "man/user.md",
            "PCRE Compliance" => "man/pcre-compliance.md",
        ],
        "Examples" => [
            "Name and addresses" => "man/example-person.md",
            "Number sequences" => "man/example-number-ranges.md",
            "Regular Expressions" => "man/pcre.md",
            "JSON" => "man/json.md",
        ],
        "Library" => Any[
            "Public" => "lib/public.md",
            "Internals" => "lib/internals.md"
        ],
        # "Developer Guide" => "developer.md"
    ],
)

deploydocs(;
    repo="github.com/gkappler/CombinedParsers.jl",
)
