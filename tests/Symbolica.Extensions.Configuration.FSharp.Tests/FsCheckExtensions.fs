module Symbolica.Extensions.Configuration.FSharp.FsCheck

open FsCheck
open Microsoft.Extensions.Configuration

module Arb =
    type NotNullString =
        static member strings() =
            Arb.Default.String() |> Arb.filter ((<>) null)

type ConfigurationKey =
    | ConfigurationKey of string
    static member Gen : Gen<ConfigurationKey> =
        Arb.NotNullString.strings ()
        |> Arb.toGen
        |> Gen.where (fun s -> not (s.Contains(ConfigurationPath.KeyDelimiter)))
        |> Gen.map ConfigurationKey

    static member Value(ConfigurationKey k) = k

type ConfigurationPath =
    | ConfigurationPath of string
    static member Gen : Gen<ConfigurationPath> =
        ConfigurationKey.Gen
        |> Gen.listOf
        |> Gen.map
            (fun keys -> System.String.Join(ConfigurationPath.KeyDelimiter, keys |> Seq.map ConfigurationKey.Value))
        |> Gen.map ConfigurationPath

type ConfigurationArb =
    static member ConfigurationKey : Arbitrary<ConfigurationKey> = ConfigurationKey.Gen |> Arb.fromGen
    static member ConfigurationPath : Arbitrary<ConfigurationPath> = ConfigurationPath.Gen |> Arb.fromGen
