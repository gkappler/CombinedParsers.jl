meta_identifier("abc")
special_sequence("? ASCII char 32 ?")

syntactic_primary("\"1b\"")

syntactic_factor("\"1b\"")

definitions_list("3 * \"aa\", \"B\"")
parse(syntax_rule,"twelve                 = \"1\", \"2\" ;")

using BenchmarkTools
integers = parse(syntax,
    """
    digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    digit                = "0" | digit excluding zero ;
    natural number = digit excluding zero, { digit } ;
    integer = "0" | [ "-" ], natural number ;
    """, log=true)

deepmap(JoinSubstring, integers, :integer)

deepmap(JoinSubstring, integers, NamedParser).options

    
syntax(
    """
    twelve a                          = "1", "2" ;
    """)

syntax(
    """
    twelve                          = "1", "2" ;
    """, log=true)

syntax(
    """
    twelve                          = "1", "2" ;    two hundredone                 = "2", "0", "1" ;
    """, log=true)

