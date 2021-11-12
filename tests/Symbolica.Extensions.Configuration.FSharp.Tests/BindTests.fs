module Symbolica.Extensions.Configuration.FSharp.Bind

open FsCheck
open FsCheck.Xunit
open Microsoft.Extensions.Configuration
open Swensen.Unquote
open global.Xunit

module Section =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder succeeds should be Success section``
        (ConfigurationPath path)
        (ConfigurationKey key)
        =
        let subSection =
            { Children = Seq.empty
              Path = ConfigurationPath.Combine(path, key)
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test <@ Bind.section key Binder.ask |> Binder.eval section = Success(subSection :> IConfigurationSection) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder fails should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "Value" } ]
              Path = path
              Value = null }

        let error = [ "Key not found" ]

        test
            <@ Bind.section key (error |> Binder.fail)
               |> Binder.eval section = Failure(error) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        test
            <@ Bind.section key Binder.ask
               |> Binder.eval (path |> SectionStub.Empty) = Failure[$"The key '{key}' does not exist at '{path}'."] @>

module OptSection =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder succeeds should be Success Some section``
        (ConfigurationPath path)
        (ConfigurationKey key)
        =
        let subSection =
            { Children = Seq.empty
              Path = ConfigurationPath.Combine(path, key)
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test
            <@ Bind.optSection key Binder.ask
               |> Binder.eval section = Success(subSection :> IConfigurationSection |> Some) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder fails should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "Value" } ]
              Path = path
              Value = null }

        let error = [ "Key not found" ]

        test
            <@ Bind.optSection key (error |> Binder.fail)
               |> Binder.eval section = Failure(error) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Success None`` (ConfigurationPath path) (ConfigurationKey key) =
        test
            <@ Bind.optSection key Binder.ask
               |> Binder.eval (path |> SectionStub.Empty) = Success(None) @>

module Value =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value exists and is not null should be Success value``
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

        test <@ Bind.value key Bind.string |> Binder.eval section = Success(x) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value does not exist should be Failure`` (ConfigurationPath path) (ConfigurationKey key) x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x } ]
              Path = path
              Value = null }

        let missingKey = $"notthe{key}"

        test
            <@ Bind.value missingKey Bind.string
               |> Binder.eval section = Failure([ $"The key '{missingKey}' does not exist at '{path}'." ]) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value exists and is null should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = null } ]
              Path = path
              Value = null }

        test
            <@ Bind.value key Bind.string |> Binder.eval section = Failure(
                [ $"The key '{key}' does not exist at '{path}'." ]
            ) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value cannot be decoded should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ Bind.value key Bind.int |> Binder.eval section = Failure(
                [ "Could not decode 'string' as type 'System.Int32'." ]
            ) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section has children should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children =
                      [ { Children = Seq.empty
                          Path = ConfigurationPath.Combine(path, key)
                          Value = "Value" } ]
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "Value" } ]
              Path = path
              Value = null }

        test
            <@ Bind.value key Bind.string |> Binder.eval section = Failure(
                [ $"Expected a simple value at '{ConfigurationPath.Combine(path, key)}' but found an object." ]
            ) @>

module OptValue =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value exists and is not null should be Success Some value``
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

        test
            <@ Bind.optValue key Bind.string
               |> Binder.eval section = Success(Some(x)) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value does not exist should be Success None`` (ConfigurationPath path) (ConfigurationKey key) x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = x } ]
              Path = path
              Value = null }

        let missingKey = $"notthe{key}"

        test
            <@ Bind.optValue missingKey Bind.string
               |> Binder.eval section = Success(None) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value exists and is null should be Success None`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = null } ]
              Path = path
              Value = null }

        test
            <@ Bind.optValue key Bind.string
               |> Binder.eval section = Success(None) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value cannot be decoded should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ Bind.optValue key Bind.int |> Binder.eval section = Failure(
                [ "Could not decode 'string' as type 'System.Int32'." ]
            ) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section has children should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                [ { Children =
                      [ { Children = Seq.empty
                          Path = ConfigurationPath.Combine(path, key)
                          Value = "Value" } ]
                    Path = ConfigurationPath.Combine(path, key)
                    Value = "Value" } ]
              Path = path
              Value = null }

        test
            <@ Bind.optValue key Bind.string
               |> Binder.eval section = Failure(
                [ $"Expected a simple value at '{ConfigurationPath.Combine(path, key)}' but found an object." ]
            ) @>

module Bool =
    [<Property>]
    let ``should be Success value if can be converted to bool`` value =
        test <@ value |> string |> Binder.run Bind.bool = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to bool`` =
        test
            <@ "string" |> Binder.run Bind.bool = Failure([ "Could not decode 'string' as type 'System.Boolean'." ]) @>

module Char =
    [<Property>]
    let ``should be Success value if can be converted to char`` value =
        test <@ value |> string |> Binder.run Bind.char = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to char`` =
        test <@ "string" |> Binder.run Bind.char = Failure([ "Could not decode 'string' as type 'System.Char'." ]) @>

module DateTime =
    [<Property>]
    let ``should be Success value if can be converted to DateTime`` (value: System.DateTime) =
        let value = value.AddMilliseconds(-value.Millisecond |> float)

        let string = value.ToString("s")

        test <@ string |> Binder.run Bind.dateTime = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to DateTime`` =
        test
            <@ "string" |> Binder.run Bind.dateTime = Failure(
                [ "Could not decode 'string' as type 'System.DateTime'." ]
            ) @>

module Float =
    [<Property>]
    let ``should be Success value if can be converted to float`` () =
        Prop.forAll
            (Arb.from<float>
             |> Arb.filter System.Double.IsFinite)
            (fun value -> test <@ value |> string |> Binder.run Bind.float = Success(value) @>)

    [<Fact>]
    let ``should be Failure if string can not be converted to float`` =
        test
            <@ "string" |> Binder.run Bind.float = Failure([ "Could not decode 'string' as type 'System.Double'." ]) @>

module Int16 =
    [<Property>]
    let ``should be Success value if can be converted to int16`` value =
        test <@ value |> string |> Binder.run Bind.int16 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to int16`` =
        test <@ "string" |> Binder.run Bind.int16 = Failure([ "Could not decode 'string' as type 'System.Int16'." ]) @>

module Int =
    [<Property>]
    let ``should be Success value if can be converted to int`` value =
        test <@ value |> string |> Binder.run Bind.int = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to int`` =
        test <@ "string" |> Binder.run Bind.int = Failure([ "Could not decode 'string' as type 'System.Int32'." ]) @>

module Int64 =
    [<Property>]
    let ``should be Success value if can be converted to int64`` value =
        test <@ value |> string |> Binder.run Bind.int64 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to int64`` =
        test <@ "string" |> Binder.run Bind.int64 = Failure([ "Could not decode 'string' as type 'System.Int64'." ]) @>

module UInt16 =
    [<Property>]
    let ``should be Success value if can be converted to uint16`` value =
        test <@ value |> string |> Binder.run Bind.uint16 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to uint16`` =
        test
            <@ "string" |> Binder.run Bind.uint16 = Failure([ "Could not decode 'string' as type 'System.UInt16'." ]) @>

module UInt =
    [<Property>]
    let ``should be Success value if can be converted to uint`` value =
        test <@ value |> string |> Binder.run Bind.uint = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to uint`` =
        test <@ "string" |> Binder.run Bind.uint = Failure([ "Could not decode 'string' as type 'System.UInt32'." ]) @>

module UInt64 =
    [<Property>]
    let ``should be Success value if can be converted to uint64`` value =
        test <@ value |> string |> Binder.run Bind.uint64 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to uint64`` =
        test
            <@ "string" |> Binder.run Bind.uint64 = Failure([ "Could not decode 'string' as type 'System.UInt64'." ]) @>

module Uri =
    [<Property>]
    let ``should be Success value if can be converted to absolute uri`` (HostName host) =
        let value = System.Uri($"https://{host}")

        test
            <@ value
               |> string
               |> Binder.run (Bind.uri System.UriKind.Absolute) = Success(value) @>

    [<Fact>]
    let ``should be Success value if can be converted to relative uri`` =
        let value = System.Uri("/relative/uri", System.UriKind.Relative)

        test
            <@ value
               |> string
               |> Binder.run (Bind.uri System.UriKind.Relative) = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to uri`` =
        test
            <@ "string"
               |> Binder.run (Bind.uri System.UriKind.Absolute) = Failure(
                [ "Could not decode 'string' as type 'System.Uri'." ]
            ) @>
