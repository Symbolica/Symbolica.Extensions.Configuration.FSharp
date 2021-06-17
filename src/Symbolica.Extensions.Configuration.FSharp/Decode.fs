namespace Symbolica.Extensions.Configuration.FSharp

type Decoder<'a> = string -> Binder<'a>

module Decode =
    let map f (m: Decoder<'a>) : Decoder<'b> = m >> Binder.map f

    let ofParser (parser: string -> bool * 'parsed) value =
        Binder
            (fun section ->
                match parser value with
                | true, x -> Success x
                | false, _ ->
                    Failure [ $"Could not decode '{value}' at path '{section |> path}' as type '{typeof<'parsed>}'." ])

    let bool = ofParser System.Boolean.TryParse
    let char = ofParser System.Char.TryParse
    let dateTime = ofParser System.DateTime.TryParse
    let float = ofParser System.Double.TryParse
    let int16 = ofParser System.Int16.TryParse
    let int = ofParser System.Int32.TryParse
    let int64 = ofParser System.Int64.TryParse
    let uint16 = ofParser System.UInt16.TryParse
    let uint = ofParser System.UInt32.TryParse
    let uint64 = ofParser System.UInt64.TryParse
