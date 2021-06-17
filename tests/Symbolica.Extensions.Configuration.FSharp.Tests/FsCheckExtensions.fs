module Symbolica.Extensions.Configuration.FSharp.FsCheck

open FsCheck

module Arb =
    type NotNullString =
        static member strings() =
            Arb.Default.String() |> Arb.filter ((<>) null)
