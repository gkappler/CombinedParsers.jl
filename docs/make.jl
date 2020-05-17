push!(LOAD_PATH,"../src/")
using Documenter: Documenter, makedocs, deploydocs
using CombinedParsers: CombinedParsers

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
        "User Guide" => "user.md",
        # "Developer Guide" => "developer.md"
    ],
)

deploydocs(;
    repo="github.com/gkappler/CombinedParsers.jl",
)
