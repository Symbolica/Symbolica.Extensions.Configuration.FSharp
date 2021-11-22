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

        let error = Error.invalidType<int> keyValue "Value"

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

        let error = Error.invalidType<int> keyValue "Value"

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
    let ``when value exists and is not null should be Success value`` path x =
        let section =
            { Children = Seq.empty
              Path = path
              Value = x }

        test <@ Bind.value Bind.string |> Binder.eval section = Success(x) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when value cannot be decoded should be Failure`` path =
        let section =
            { Children = Seq.empty
              Path = path
              Value = "string" }

        test
            <@ Bind.value Bind.int |> Binder.eval section = Failure(
                Error.ValueError("string", ValueError.invalidType<int>)
            ) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section has children should be Failure`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "Value" } ]
              Path = path
              Value = null }

        test <@ Bind.value Bind.string |> Binder.eval section = Failure(Error.NotAValueNode) @>

module ValueAt =
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
            <@ Bind.valueAt (key |> ConfigPathSegment.value) Bind.string
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
            <@ Bind.valueAt missingKey Bind.string
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
            <@ Bind.valueAt (key |> ConfigPathSegment.value) Bind.string
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
            <@ Bind.valueAt (key |> ConfigPathSegment.value) Bind.int
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
            <@ Bind.valueAt (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Failure(Error.notAValueNode (key |> ConfigPathSegment.value)) @>

module OptValueAt =
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
            <@ Bind.optValueAt (key |> ConfigPathSegment.value) Bind.string
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
            <@ Bind.optValueAt missingKey Bind.string
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
            <@ Bind.optValueAt (key |> ConfigPathSegment.value) Bind.string
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
            <@ Bind.optValueAt (key |> ConfigPathSegment.value) Bind.int
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
            <@ Bind.optValueAt (key |> ConfigPathSegment.value) Bind.string
               |> Binder.eval section = Failure(Error.notAValueNode (key |> ConfigPathSegment.value)) @>

module AllOf =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return Success if all binders succeed`` path key1 key2 key3 =
        [ key1; key2; key3 ]
        |> List.distinct
        |> List.length = 3
        ==> lazy
            (let section =
                { Children =
                    [ { Children = Seq.empty
                        Path = key1
                        Value = "1" }
                      { Children = Seq.empty
                        Path = key2
                        Value = "2" }
                      { Children = Seq.empty
                        Path = key3
                        Value = "3" } ]
                  Path = path
                  Value = null }

             test
                 <@ [ key3; key1 ]
                    |> List.map (fun k -> Bind.valueAt (k |> ConfigPathSegment.value) Bind.int)
                    |> Bind.allOf
                    |> Binder.eval section = Success([ 3; 1 ]) @>)

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return Failure if any binders fail`` path key1 key2 key3 =
        [ key1; key2; key3 ]
        |> List.distinct
        |> List.length = 3
        ==> lazy
            (let section =
                { Children =
                    [ { Children = Seq.empty
                        Path = key1
                        Value = "1" }
                      { Children = Seq.empty
                        Path = key2
                        Value = "2" } ]
                  Path = path
                  Value = null }

             test
                 <@ [ key3; key1 ]
                    |> List.map (fun k -> Bind.valueAt (k |> ConfigPathSegment.value) Bind.int)
                    |> Bind.allOf
                    |> Binder.eval section = Failure(
                     Error.Many(Errors.single (Error.keyNotFound (key3 |> ConfigPathSegment.value)))
                 ) @>)

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return all Failures if multiple binders fail`` path key1 key2 key3 =
        [ key1; key2; key3 ]
        |> List.distinct
        |> List.length = 3
        ==> lazy
            (let section =
                { Children =
                    [ { Children = Seq.empty
                        Path = key2
                        Value = "2" } ]
                  Path = path
                  Value = null }

             test
                 <@ [ key3; key1 ]
                    |> List.map (fun k -> Bind.valueAt (k |> ConfigPathSegment.value) Bind.int)
                    |> Bind.allOf
                    |> Binder.eval section = Failure(
                     Error.Many(
                         Errors.AllOf(
                             Error.keyNotFound (key3 |> ConfigPathSegment.value)
                             +& Error.keyNotFound (key1 |> ConfigPathSegment.value)
                         )
                     )
                 ) @>)

module AnyOf =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return Success if any binders succeed`` path key1 key2 key3 =
        [ key1; key2; key3 ]
        |> List.distinct
        |> List.length = 3
        ==> lazy
            (let section =
                { Children =
                    [ { Children = Seq.empty
                        Path = key1
                        Value = "1" }
                      { Children = Seq.empty
                        Path = key3
                        Value = "3" } ]
                  Path = path
                  Value = null }

             test
                 <@ [ key2; key3; key1 ]
                    |> List.map (fun k -> Bind.valueAt (k |> ConfigPathSegment.value) Bind.int)
                    |> Bind.anyOf
                    |> Binder.eval section = Success([ 3; 1 ]) @>)

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return Success of empty list if all binders fail`` path key1 key2 key3 =
        [ key1; key2; key3 ]
        |> List.distinct
        |> List.length = 3
        ==> lazy
            (let section =
                { Children =
                    [ { Children = Seq.empty
                        Path = key2
                        Value = "2" } ]
                  Path = path
                  Value = null }

             test
                 <@ [ key3; key1 ]
                    |> List.map (fun k -> Bind.valueAt (k |> ConfigPathSegment.value) Bind.int)
                    |> Bind.anyOf
                    |> Binder.eval section = Success([]) @>)

module OneOf =
    type AChoice =
        | Number of int
        | Binary of bool

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return first success`` path key =

        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "1" } ]
              Path = path
              Value = null }

        test
            <@ (Bind.valueAt "bool" (Bind.bool |> Binder.map Binary))
               <|> (Bind.valueAt (key |> ConfigPathSegment.value) (Bind.int |> Binder.map Number))
               |> Bind.oneOf
               |> Binder.eval section = Success(Number 1) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return Failure if all alternatives fail`` path =
        let key = "number"

        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = ConfigPathSegment "notthekey"
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ (Bind.valueAt "bool" (Bind.bool |> Binder.map Binary))
               <|> (Bind.valueAt key (Bind.int |> Binder.map Number))
               |> Bind.oneOf
               |> Binder.eval section = Failure(
                Error.Many(
                    Errors.OneOf(
                        Error.keyNotFound "bool"
                        +| Error.keyNotFound "number"
                    )
                )
            ) @>

module OneValueOf =
    type AChoice =
        | Number of int
        | Binary of bool

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return first success`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "1" } ]
              Path = path
              Value = null }

        test
            <@ (Bind.bool |> Binder.map Binary)
               <|> (Bind.int |> Binder.map Number)
               |> Bind.oneValueOf
               |> Bind.valueAt (key |> ConfigPathSegment.value)
               |> Binder.eval section = Success(Number 1) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should return Failure if all alternatives fail`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "string" } ]
              Path = path
              Value = null }

        test
            <@ (Bind.bool |> Binder.map Binary)
               <|> (Bind.int |> Binder.map Number)
               |> Bind.oneValueOf
               |> Bind.valueAt (key |> ConfigPathSegment.value)
               |> Binder.eval section = Failure(
                Error.valueError
                    (key |> ConfigPathSegment.value)
                    "string"
                    (ValueError.Many(
                        Errors.OneOf(
                            ValueError.invalidType<bool>
                            +| ValueError.invalidType<int>
                        )
                    ))
            ) @>

module KeyValuePair =

    type CustomType = { Prop: int }

    [<Property>]
    let ``should be success if key and simple value can be decoded`` key value =
        let section =
            { Children = Seq.empty
              Path = key |> string |> ConfigPathSegment
              Value = value |> string }

        test
            <@ Bind.keyValuePair Bind.int (Bind.value Bind.bool)
               |> Binder.eval section
               |> BindResult.map (|KeyValue|) = Success(key, value) @>

    [<Property>]
    let ``should be success if key and complex value can be decoded`` key value =
        let bindCustomType =
            bind {
                let! prop = Bind.valueAt "prop" Bind.int
                return { Prop = prop }
            }

        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = "prop" |> ConfigPathSegment
                    Value = value |> string } ]
              Path = key |> string |> ConfigPathSegment
              Value = null }

        test
            <@ Bind.keyValuePair Bind.int bindCustomType
               |> Binder.eval section
               |> BindResult.map (|KeyValue|) = Success(key, { Prop = value }) @>

    [<Property>]
    let ``should be failure if the key cannot be decoded`` (value: bool) =
        let section =
            { Children = Seq.empty
              Path = "key" |> ConfigPathSegment
              Value = value |> string }

        test
            <@ Bind.keyValuePair Bind.int (Bind.value Bind.bool)
               |> Binder.eval section = Failure(
                Error.Many(Errors.single (Error.ValueError("key", ValueError.invalidType<int>)))
            ) @>

    [<Property>]
    let ``should be failure if the value cannot be decoded`` (key: int) =
        let section =
            { Children = Seq.empty
              Path = key |> string |> ConfigPathSegment
              Value = "value" }

        test
            <@ Bind.keyValuePair Bind.int (Bind.value Bind.bool)
               |> Binder.eval section = Failure(
                Error.Many(Errors.single (Error.ValueError("value", ValueError.invalidType<bool>)))
            ) @>

    [<Fact>]
    let ``should be failure if the key and value cannot be decoded`` () =
        let section =
            { Children = Seq.empty
              Path = "key" |> ConfigPathSegment
              Value = "value" }

        test
            <@ Bind.keyValuePair Bind.int (Bind.value Bind.bool)
               |> Binder.eval section = Failure(
                Error.Many(
                    Errors.AllOf(
                        Error.ValueError("key", ValueError.invalidType<int>)
                        +& Error.ValueError("value", ValueError.invalidType<bool>)
                    )
                )
            ) @>

module Dict =

    type CustomType = { Prop: int }

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should be success if all keys and values can be decoded`` path =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = "1" |> ConfigPathSegment
                    Value = "true" } ]
              Path = path
              Value = null }

        test
            <@ Bind.dict Bind.int (Bind.value Bind.bool)
               |> Binder.eval section
               |> BindResult.map (Seq.map (|KeyValue|) >> List.ofSeq) = Success[1, true] @>

    [<Property>]
    let ``should be success if key and complex value can be decoded`` path key value =
        let bindCustomType =
            bind {
                let! prop = Bind.valueAt "prop" Bind.int
                return { Prop = prop }
            }

        let section =
            { Children =
                [ { Children =
                      [ { Children = Seq.empty
                          Path = "prop" |> ConfigPathSegment
                          Value = value |> string } ]
                    Path = key |> string |> ConfigPathSegment
                    Value = null } ]
              Path = path
              Value = null }

        test
            <@ Bind.dict Bind.int bindCustomType
               |> Binder.eval section
               |> BindResult.map (Seq.map (|KeyValue|) >> List.ofSeq) = Success[key, { Prop = value }] @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should be failure if any key cannot be decoded`` path =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = "foo" |> ConfigPathSegment
                    Value = "true" } ]
              Path = path
              Value = null }

        test
            <@ Bind.dict Bind.int (Bind.value Bind.bool)
               |> Binder.eval section = Failure(
                Error.Many(
                    Errors.single (
                        Error.SectionError(
                            "foo",
                            Error.Many(Errors.single (Error.ValueError("foo", ValueError.invalidType<int>)))
                        )
                    )
                )
            ) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should be failure if any value cannot be decoded`` path =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = "1" |> ConfigPathSegment
                    Value = "foo" }
                  { Children = Seq.empty
                    Path = "2" |> ConfigPathSegment
                    Value = "bar" }
                  { Children = Seq.empty
                    Path = "3" |> ConfigPathSegment
                    Value = "true" } ]
              Path = path
              Value = null }

        test
            <@ Bind.dict Bind.int (Bind.value Bind.bool)
               |> Binder.eval section = Failure(
                Error.Many(
                    Errors.AllOf(
                        Error.SectionError(
                            "1",
                            Error.Many(Errors.single (Error.ValueError("foo", ValueError.invalidType<bool>)))
                        )
                        +& Error.SectionError(
                            "2",
                            Error.Many(Errors.single (Error.ValueError("bar", ValueError.invalidType<bool>)))
                        )
                    )
                )
            ) @>

module List =

    type CustomType = { Prop: int }

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should be success ordered by keys if all values can be decoded`` path =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = "foo" |> ConfigPathSegment
                    Value = "foo" }
                  { Children = Seq.empty
                    Path = "bar" |> ConfigPathSegment
                    Value = "bar" }
                  { Children = Seq.empty
                    Path = "4" |> ConfigPathSegment
                    Value = "4" }
                  { Children = Seq.empty
                    Path = "0" |> ConfigPathSegment
                    Value = "0" }
                  { Children = Seq.empty
                    Path = "1" |> ConfigPathSegment
                    Value = "1" } ]
              Path = path
              Value = null }

        test
            <@ Bind.list (Bind.value Bind.string)
               |> Binder.eval section = Success["0"
                                                "1"
                                                "4"
                                                "bar"
                                                "foo"] @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should be success if key and complex value can be decoded`` path key value =
        let bindCustomType =
            bind {
                let! prop = Bind.valueAt "prop" Bind.int
                return { Prop = prop }
            }

        let section =
            { Children =
                [ { Children =
                      [ { Children = Seq.empty
                          Path = "prop" |> ConfigPathSegment
                          Value = value |> string } ]
                    Path = key
                    Value = null } ]
              Path = path
              Value = null }

        test <@ Bind.list bindCustomType |> Binder.eval section = Success[{ Prop = value }] @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should be failure if any value cannot be decoded`` path =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = "1" |> ConfigPathSegment
                    Value = "foo" }
                  { Children = Seq.empty
                    Path = "2" |> ConfigPathSegment
                    Value = "bar" }
                  { Children = Seq.empty
                    Path = "3" |> ConfigPathSegment
                    Value = "true" } ]
              Path = path
              Value = null }

        test
            <@ Bind.list (Bind.value Bind.bool)
               |> Binder.eval section = Failure(
                Error.Many(
                    Errors.AllOf(
                        Error.SectionError(
                            "1",
                            Error.Many(Errors.single (Error.ValueError("foo", ValueError.invalidType<bool>)))
                        )
                        +& Error.SectionError(
                            "2",
                            Error.Many(Errors.single (Error.ValueError("bar", ValueError.invalidType<bool>)))
                        )
                    )
                )
            ) @>

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
