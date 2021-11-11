module Symbolica.Extensions.Configuration.FSharp.Bind

open FsCheck
open FsCheck.Xunit
open Microsoft.Extensions.Configuration
open Swensen.Unquote
open global.Xunit

module Value =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop exists and is not null should be Success value`` (ConfigurationPath path) (ConfigurationKey key) x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x } ]
              Path = path
              Value = null }

        test <@ Bind.value key |> Binder.eval section = Success(x) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop does not exist should be Failure`` (ConfigurationPath path) (ConfigurationKey key) x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x } ]
              Path = path
              Value = null }

        let missingKey = $"{key}-1"

        test
            <@ missingKey |> Bind.value |> Binder.eval section = Failure(
                [ $"The key '{missingKey}' does not exist at '{path}'." ]
            ) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when prop exists and is null should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = null } ]
              Path = path
              Value = null }

        test <@ key |> Bind.value |> Binder.eval section = Failure([ $"The key '{key}' does not exist at '{path}'." ]) @>

module ValueOf =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop exists and can be decoded should be Success value``
        (ConfigurationPath path)
        (ConfigurationKey key)
        (x: int)
        =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x |> string } ]
              Path = path
              Value = null }

        test <@ Bind.valueOf Decode.int key |> Binder.eval section = Success(x) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop does not exist should be Failure`` (ConfigurationPath path) (ConfigurationKey key) (x: int) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x |> string } ]
              Path = path
              Value = null }

        let missingKey = $"{key}-1"

        test
            <@ missingKey
               |> Bind.valueOf Decode.int
               |> Binder.eval section = Failure([ $"The key '{missingKey}' does not exist at '{path}'." ]) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when prop cannot be decoded should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ key |> Bind.valueOf Decode.int |> Binder.eval section = Failure(
                [ $"Could not decode 'string' at path '{path}' as type 'System.Int32'." ]
            ) @>

module OptValue =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop exists and is not null should be Success Some value``
        (ConfigurationPath path)
        (ConfigurationKey key)
        x
        =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x } ]
              Path = path
              Value = null }

        test <@ Bind.optValue key |> Binder.eval section = Success(Some(x)) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop does not exist should be Success None`` (ConfigurationPath path) (ConfigurationKey key) x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x } ]
              Path = path
              Value = null }

        let missingKey = $"{key}-1"

        test <@ missingKey |> Bind.optValue |> Binder.eval section = Success(None) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when prop exists and is null should be Success None`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = null } ]
              Path = path
              Value = null }

        test <@ key |> Bind.optValue |> Binder.eval section = Success(None) @>

module OptValueOf =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop exists and can be decoded should be Success value``
        (ConfigurationPath path)
        (ConfigurationKey key)
        (x: int)
        =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x |> string } ]
              Path = path
              Value = null }

        test <@ Bind.optValueOf Decode.int key |> Binder.eval section = Success(Some(x)) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when prop does not exist should be Success None`` (ConfigurationPath path) (ConfigurationKey key) (x: int) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x |> string } ]
              Path = path
              Value = null }

        let missingKey = $"{key}-1"

        test
            <@ missingKey
               |> Bind.optValueOf Decode.int
               |> Binder.eval section = Success(None) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when prop cannot be decoded should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ key
               |> Bind.optValueOf Decode.int
               |> Binder.eval section = Failure(
                [ $"Could not decode 'string' at path '{path}' as type 'System.Int32'." ]
            ) @>
