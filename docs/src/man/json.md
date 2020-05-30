# JSON
[Adapted from the fastparse JSON example](https://www.lihaoyi.com/fastparse/#Json).



```@example session

using CombinedParsers

space    = Repeat( CharIn(" \r\n") )
hexDigit = CharIn('0':'9','a':'f','A':'F')
strChars = CharNotIn("\"\\")
@with_names begin
    decimal = Repeat1( CharIn('0':'9') )
    exponent = ( CharIn("eE") * map(v->parse(Int,v), !(('+'|'-'|missing) * decimal)) )[2]
    ## force result_type to be supertype of type inferred from transformation
    fractional    = map(v->parse(Float64,v), Number, !( "." * decimal ) )
    integral      =  "0" | CharIn('1':'9') * Optional(decimal)

    number = map(map(v->parse(Int,v), !(('+'|'-'|missing) * integral)) * (fractional | 0) * ( exponent | 0 )) do v
        (i,f,e) = v
        ((i+f)*10^e)::Union{Float64,Int64}
    end


    unicodeEscape = "u" * hexDigit * hexDigit * hexDigit * hexDigit 
    escape        = "\\" * ( CharIn("\"/\\\\bfnrt") | unicodeEscape )

    string = ( space * "\"" / !Repeat(strChars | escape) * "\"" )[3]

    data = string | parser("true"=>true) | ("false"=>false) | ("null"=>nothing) | number | NamedTuple | Vector
    jsonExpr = ( space * data * space )[2]

    array = ( "[" / join(Repeat(jsonExpr),",") * "]" )[2] 
    push!(data, array)

    pair = map(string * space / ":" / jsonExpr ) do (k,s,d,v)
        Pair{Symbol,result_type(jsonExpr)}(Symbol(k),v)
    end;

    obj = map(v->(;v...),( "{" / join(Repeat(pair),",") * space * "}")[2])
    push!(data, obj)
end
nothing # hide
```

```@repl session
parse(pair, "\"a\" : 1")
parse(obj,"{\"a_number\" : 5.0, \"an_array\" : [\"string\", 9]}")
```


```@example session
s="""
{
    "firstName": "John",
    "lastName": "Smith",
    "age": 25,
    "address": {
        "streetAddress": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": 10021
    },
    "phoneNumbers": 
    [
        {
            "type": "home",
            "number": "212 555-1234"
        },
        {
            "type": "fax",
            "number": "646 555-4567"
        }
    ]
}
"""
parse(obj,s)
```


