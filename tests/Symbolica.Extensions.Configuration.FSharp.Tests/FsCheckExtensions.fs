module Symbolica.Extensions.Configuration.FSharp.FsCheck

open FsCheck
open Microsoft.Extensions.Configuration

module Arb =
    type NotNullString =
        static member strings() =
            Arb.Default.String() |> Arb.filter ((<>) null)

type ConfigPathSegment =
    | ConfigPathSegment of string
    static member Gen: Gen<ConfigPathSegment> =
        Arb.NotNullString.strings ()
        |> Arb.toGen
        |> Gen.where (fun s -> not (s.Contains(ConfigurationPath.KeyDelimiter)))
        |> Gen.map ConfigPathSegment

module ConfigPathSegment =
    let empty = ConfigPathSegment System.String.Empty
    let value (ConfigPathSegment x) = x

type ConfigurationArb =
    static member ConfigPathSegment: Arbitrary<ConfigPathSegment> =
        ConfigPathSegment.Gen |> Arb.fromGen
