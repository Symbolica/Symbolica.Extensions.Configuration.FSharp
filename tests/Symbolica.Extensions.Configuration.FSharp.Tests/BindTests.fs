module Symbolica.Extensions.Configuration.FSharp.Bind

open FsCheck
open FsCheck.Xunit
open Microsoft.Extensions.Configuration
open Swensen.Unquote
open global.Xunit

module Section =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder succeeds should be Success section`` path key =
        let subSection =
            { Children = Seq.empty
              Path = key
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test
            <@ Bind.section (key |> ConfigPathSegment.value) Binder.ask
               |> Binder.eval section = Success(subSection :> IConfigurationSection) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder fails should be Failure`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "Value" } ]
              Path = path
              Value = null }

        let keyValue = key |> ConfigPathSegment.value

        let error =
            Error.invalidType<int> keyValue "Value"
            |> Errors.single

        test
            <@ Bind.section keyValue (error |> Binder.fail)
               |> Binder.eval section = Failure(Error.SectionError(keyValue, error)) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Failure`` path key =
        test
            <@ Bind.section key Binder.ask
               |> Binder.eval (path |> SectionStub.Empty) = (Error.keyNotFound key |> Failure) @>

module OptSection =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder succeeds should be Success Some section`` path key =
        let subSection =
            { Children = Seq.empty
              Path = key
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test
            <@ Bind.optSection (key |> ConfigPathSegment.value) Binder.ask
               |> Binder.eval section = Success(subSection :> IConfigurationSection |> Some) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists and binder fails should be Failure`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "Value" } ]
              Path = path
              Value = null }

        let keyValue = key |> ConfigPathSegment.value

        let error =
            Error.invalidType<int> keyValue "Value"
            |> Errors.single

        test
            <@ Bind.optSection keyValue (error |> Binder.fail)
               |> Binder.eval section = Failure(Error.SectionError(keyValue, error)) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Success None`` path key =
        test
            <@ Bind.optSection (key |> ConfigPathSegment.value) Binder.ask
               |> Binder.eval (path |> SectionStub.Empty) = Success(None) @>

module Value =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value exists and is not null should be Success value`` path key x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = x } ]
              Path = path
              Value = null }

        test
            <@ Bind.value (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Success(x) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value does not exist should be Failure`` path key x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = x } ]
              Path = path
              Value = null }

        let missingKey = $"notthe{key |> ConfigPathSegment.value}"

        test
            <@ Bind.value missingKey Bind.string
               |> Binder.eval section = Failure(Error.keyNotFound missingKey) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value exists and is null should be Failure`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = null } ]
              Path = path
              Value = null }

        test
            <@ Bind.value (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Failure(Error.keyNotFound (key |> ConfigPathSegment.value)) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value cannot be decoded should be Failure`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ Bind.value (key |> ConfigPathSegment.value) Bind.int
               |> Binder.eval section = Failure(Error.invalidType<int> (key |> ConfigPathSegment.value) "string") @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section has children should be Failure`` path key =
        let section =
            { Children =
                [ { Children =
                      [ { Children = Seq.empty
                          Path = key
                          Value = "Value" } ]
                    Path = key
                    Value = "Value" } ]
              Path = path
              Value = null }

        test
            <@ Bind.value (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Failure(Error.notAValueNode (key |> ConfigPathSegment.value)) @>

module OptValue =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value exists and is not null should be Success(Some(value))`` path key x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = x } ]
              Path = path
              Value = null }

        test
            <@ Bind.optValue (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Success(Some(x)) @>

    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value does not exist should be Success(None)`` path key x =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = x } ]
              Path = path
              Value = null }

        let missingKey = $"notthe{key |> ConfigPathSegment.value}"

        test
            <@ Bind.optValue missingKey Bind.string
               |> Binder.eval section = Success(None) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value exists and is null should be Success(None)`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = null } ]
              Path = path
              Value = null }

        test
            <@ Bind.optValue (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Success(None) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value cannot be decoded should be Failure`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ Bind.optValue (key |> ConfigPathSegment.value) Bind.int
               |> Binder.eval section = Failure(Error.invalidType<int> (key |> ConfigPathSegment.value) "string") @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section has children should be Failure`` path key =
        let section =
            { Children =
                [ { Children =
                      [ { Children = Seq.empty
                          Path = key
                          Value = "Value" } ]
                    Path = key
                    Value = "Value" } ]
              Path = path
              Value = null }

        test
            <@ Bind.optValue (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Failure(Error.notAValueNode (key |> ConfigPathSegment.value)) @>

module Bool =
    [<Property>]
    let ``should be Success value if can be converted to bool`` value =
        test <@ value |> string |> Binder.run Bind.bool = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to bool`` =
        test <@ "string" |> Binder.run Bind.bool = Failure(ValueError.invalidType<bool>) @>

module Char =
    [<Property>]
    let ``should be Success value if can be converted to char`` value =
        test <@ value |> string |> Binder.run Bind.char = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to char`` =
        test <@ "string" |> Binder.run Bind.char = Failure(ValueError.invalidType<char>) @>

module DateTime =
    [<Property>]
    let ``should be Success value if can be converted to DateTime`` (value: System.DateTime) =
        let value = value.AddMilliseconds(-value.Millisecond |> float)

        let string = value.ToString("s")

        test <@ string |> Binder.run Bind.dateTime = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to DateTime`` =
        test <@ "string" |> Binder.run Bind.dateTime = Failure(ValueError.invalidType<System.DateTime>) @>

module Float =
    [<Property>]
    let ``should be Success value if can be converted to float`` () =
        Prop.forAll
            (Arb.from<float>
             |> Arb.filter System.Double.IsFinite)
            (fun value -> test <@ value |> string |> Binder.run Bind.float = Success(value) @>)

    [<Fact>]
    let ``should be Failure if string can not be converted to float`` =
        test <@ "string" |> Binder.run Bind.float = Failure(ValueError.invalidType<float>) @>

module Int16 =
    [<Property>]
    let ``should be Success value if can be converted to int16`` value =
        test <@ value |> string |> Binder.run Bind.int16 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to int16`` =
        test <@ "string" |> Binder.run Bind.int16 = Failure(ValueError.invalidType<int16>) @>

module Int =
    [<Property>]
    let ``should be Success value if can be converted to int`` value =
        test <@ value |> string |> Binder.run Bind.int = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to int`` =
        test <@ "string" |> Binder.run Bind.int = Failure(ValueError.invalidType<int>) @>

module Int64 =
    [<Property>]
    let ``should be Success value if can be converted to int64`` value =
        test <@ value |> string |> Binder.run Bind.int64 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to int64`` =
        test <@ "string" |> Binder.run Bind.int64 = Failure(ValueError.invalidType<int64>) @>

module UInt16 =
    [<Property>]
    let ``should be Success value if can be converted to uint16`` value =
        test <@ value |> string |> Binder.run Bind.uint16 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to uint16`` =
        test <@ "string" |> Binder.run Bind.uint16 = Failure(ValueError.invalidType<uint16>) @>

module UInt =
    [<Property>]
    let ``should be Success value if can be converted to uint`` value =
        test <@ value |> string |> Binder.run Bind.uint = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to uint`` =
        test <@ "string" |> Binder.run Bind.uint = Failure(ValueError.invalidType<uint>) @>

module UInt64 =
    [<Property>]
    let ``should be Success value if can be converted to uint64`` value =
        test <@ value |> string |> Binder.run Bind.uint64 = Success(value) @>

    [<Fact>]
    let ``should be Failure if string can not be converted to uint64`` =
        test <@ "string" |> Binder.run Bind.uint64 = Failure(ValueError.invalidType<uint64>) @>

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
               |> Binder.run (Bind.uri System.UriKind.Absolute) = Failure(ValueError.invalidType<System.Uri>) @>
