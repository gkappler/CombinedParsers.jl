# JSON
[Adapted from the fastparse JSON example](https://www.lihaoyi.com/fastparse/#Json).



```@example session

using CombinedParsers


@with_names begin
    space    = Repeat( CharIn(" \r\n") )
    hexDigit = CharIn('0':'9','a':'f','A':'F')
    strChars = CharNotIn("\"\\")
    decimal = Repeat1( CharIn('0':'9') )
    exponent = Sequence(
	CharIn("eE"),
	map(v->parse(Int,v), !(('+'|'-'|missing) * decimal)) 
    )[2]
    fractional = map(
        v->parse(Float64,v), ## result_type inferred and 
	Number,              ## defined explicitely as supertype 
	!( "." * decimal ) )
    integral   =  "0" | CharIn('1':'9') * Optional(decimal)

    number = Sequence(map(v->parse(Int,v), !(('+'|'-'|missing) * integral)),
	              (fractional | 0),
		      ( exponent | 0 )) do v
                          (i,f,e) = v
                          ((i+f)*10^e)::Union{Float64,Int64}
                      end


    unicodeEscape = "u" * hexDigit * hexDigit * hexDigit * hexDigit 
    escape        = "\\" * ( CharIn("\"/\\\\bfnrt") | unicodeEscape )

    lstring = ( space * "\"" * !Repeat(strChars | escape) * "\"" )[3]

    data = Either{Any}(Any[
	lstring, 
	parser("true"=>true), 
	parser("false"=>false), 
	parser("null"=>nothing), 
	number])
    ## data = (lstring | parser("true"=>true) | ("false"=>false) | ("null"=>nothing) | number) | NamedTuple | Vector
    @syntax jsonExpr = ( space * data * space )[2]

    array = ( "[" * join(Repeat(jsonExpr),",") * "]" )[2] 
    push!(data, array)

    pair = map(lstring * space * ":" * jsonExpr ) do (k,s,d,v)
        Pair{Symbol,result_type(jsonExpr)}(Symbol(k),v)
    end;

    obj = ( "{" * map(v->(;v...),join(Repeat(pair),",")) * space * "}")[2]
    push!(data, obj)
end;
nothing # hide
```

```@repl session

parse(pair, "\"a\" : 1")

jsonExpr"""
      {"a_number" : 5.0, "an_array" : ["string", 9]}
"""
```

```@example session
jsonExpr"""
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
```


