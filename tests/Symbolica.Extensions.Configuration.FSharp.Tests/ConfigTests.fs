namespace Symbolica.Extensions.Configuration.FSharp

open FsCheck
open FsCheck.Xunit
open Microsoft.Extensions.Configuration
open Swensen.Unquote
open global.Xunit

module BindResult =
    module Apply =
        let (<*>) = BindResult.apply

        [<Property>]
        let ``should obey identity law`` (w: BindResult<int>) = test <@ Success(id) <*> w = w @>

        [<Property>]
        let ``should obey composition law`` (u: BindResult<int -> string>) (v: BindResult<bool -> int>) (w: BindResult<bool>) =
            test <@ Success(<<) <*> u <*> v <*> w = (u <*> (v <*> w)) @>

        [<Property>]
        let ``should obey homomorphism law`` (f: string -> string) x =
            test <@ Success(f) <*> Success(x) = Success(x |> f) @>

        [<Property>]
        let ``should obey interchange law`` (u: BindResult<string -> string>) x =
            test <@ u <*> Success(x) = (Success(fun f -> x |> f) <*> u) @>

        [<Property>]
        let ``Failure(e1) apply Success(x) should be Failure(e1)`` e1 (x: int) =
            test <@ Failure(e1) <*> Success(x) = Failure(e1) @>

        [<Property>]
        let ``Success(f) apply Failure(e2) should be Failure(e2)`` (f: int -> int) e2 =
            test <@ Success(f) <*> Failure(e2) = Failure(e2) @>

        [<Property>]
        let ``Failure(e1) apply Failure(e2) should be Failure(e1 append e2)`` e1 e2 =
            test <@ Failure(e1) <*> Failure(e2) = Failure(e1 |> List.append <| e2) @>

    module Bind =
        let (>>=) m f = BindResult.bind f m

        [<Property>]
        let ``should obey left identity`` x (f: int -> BindResult<int>) = test <@ Success(x) >>= f = (f x) @>

        [<Property>]
        let ``should obey right identity`` (m: BindResult<int>) = test <@ m >>= Success = m @>

        [<Property>]
        let ``should obey associativity`` m (f: bool -> BindResult<int>) (g: int -> BindResult<string>) =
            test <@ (m >>= f) >>= g = (m >>= (fun x -> x |> f >>= g)) @>

        [<Property>]
        let ``Failure(e) >>= f should be Failure(e)`` e (f: int -> BindResult<string>) =
            test <@ Failure(e) >>= f = Failure(e) @>

    module DefaultWith =
        [<Property>]
        let ``should obey identity law`` e =
            test <@ Failure(e) |> BindResult.defaultWith id = e @>

        [<Property>]
        let ``Success(x) |> defaultWith f should be x`` (x: int) f =
            test <@ Success(x) |> BindResult.defaultWith f = x @>

        [<Property>]
        let ``Failure(e) |> defaultWith f should be f e`` e (f: string list -> string) =
            test <@ Failure(e) |> BindResult.defaultWith f = (f e) @>

    module Map =
        [<Property>]
        let ``should obey identity law`` (x: BindResult<int>) = test <@ x |> BindResult.map id = x @>

        [<Property>]
        let ``should obey associativity law`` x (f: bool -> int) (g: int -> string) =
            test <@ x |> BindResult.map (f >> g) = (x |> BindResult.map f |> BindResult.map g) @>

    module Zip =
        [<Property>]
        let ```zip Success(a) Success(b) should be Success(a, b)`` (a: int) (b: string) =
            test <@ BindResult.zip (Success(a)) (Success(b)) = Success(a, b) @>

        [<Property>]
        let ``zip Failure(e1) Success(b) should be Failure(e1)`` e1 (b: string) =
            test <@ BindResult.zip (Failure(e1)) (Success(b)) = Failure(e1) @>

        [<Property>]
        let ```zip Success(a) Failure(e2) should be Failure(e2)`` (a: int) e2 =
            test <@ BindResult.zip (Success(a)) (Failure(e2)) = Failure(e2) @>

        [<Property>]
        let ```zip Failure(e1) Failure(e2) should be Failure(e1 append e2)`` e1 e2 =
            test <@ BindResult.zip (Failure(e1)) (Failure(e2)) = Failure(e1 |> List.append <| e2) @>

type SectionStub =
    { Children: IConfigurationSection seq
      Path: string
      Value: string }
    static member Empty path =
        { Children = Seq.empty
          Path = path
          Value = null }

    interface IConfigurationSection with
        member x.GetChildren() = x.Children
        member _.GetReloadToken() = failwith "Not Implemented"

        member x.GetSection(key) =
            x.Children
            |> Seq.tryFind (fun s -> s.Key = key)
            |> Option.defaultValue (
                ConfigurationPath.Combine(x.Path, key)
                |> SectionStub.Empty
                :> IConfigurationSection
            )

        member _.Item
            with get _ = failwith "Not Implemented"
            and set _ __ = failwith "Not Implemented"

        member x.Key = ConfigurationPath.GetSectionKey(x.Path)
        member x.Path = x.Path
        member x.Value = x.Value

        member _.Value
            with set _ = failwith "Not Implemented"

module Binder =
    module Apply =
        let (<*>) = Binder.apply

        let config = SectionStub.Empty ""

        [<Property>]
        let ``should obey identity law`` (w: Binder<int>) =
            test <@ (Binder.result id <*> w) |> Binder.eval config = (w |> Binder.eval config) @>

        [<Property>]
        let ``should obey composition law`` (u: Binder<int -> string>) (v: Binder<bool -> int>) (w: Binder<bool>) =
            test
                <@ Binder.result (<<) <*> u <*> v <*> w
                   |> Binder.eval config = ((u <*> (v <*> w)) |> Binder.eval config) @>

        [<Property>]
        let ``should obey homomorphism law`` (f: string -> string) x =
            test
                <@ Binder.result f <*> Binder.result x
                   |> Binder.eval config = Success(x |> f) @>

        [<Property>]
        let ``should obey interchange law`` (u: Binder<string -> string>) x =
            test
                <@ u <*> Binder.result x |> Binder.eval config = ((Binder.result (fun f -> x |> f) <*> u)
                                                                  |> Binder.eval config) @>

        [<Property>]
        let ``Failure(e1) apply Success(x) should be Failure(e1)`` e1 (x: int) =
            test
                <@ Binder.ofBind (Failure(e1)) <*> Binder.result x
                   |> Binder.eval config = Failure(e1) @>

        [<Property>]
        let ``Success(f) apply Failure(e2) should be Failure(e2)`` (f: int -> int) e2 =
            test
                <@ Binder.result f <*> Binder.ofBind (Failure(e2))
                   |> Binder.eval config = Failure(e2) @>

        [<Property>]
        let ``Failure(e1) apply Failure(e2) should be Failure(e1 append e2)`` e1 e2 =
            test
                <@ Binder.ofBind (Failure(e1))
                   <*> Binder.ofBind (Failure(e2))
                   |> Binder.eval config = Failure(e1 |> List.append <| e2) @>

    module Bind =
        let (>>=) m f = Binder.bind f m
        let config = SectionStub.Empty ""

        [<Property>]
        let ``should obey left identity`` x (f: int -> Binder<int>) =
            test <@ Binder.result x >>= f |> Binder.eval config = (x |> f |> Binder.eval config) @>

        [<Property>]
        let ``should obey right identity`` (m: Binder<int>) =
            test <@ m >>= Binder.result |> Binder.eval config = (m |> Binder.eval config) @>

        [<Property>]
        let ``should obey associativity`` m (f: bool -> Binder<int>) (g: int -> Binder<string>) =
            test
                <@ (m >>= f) >>= g |> Binder.eval config = (m
                                                            >>= (fun x -> x |> f >>= g)
                                                            |> Binder.eval config) @>

        [<Property>]
        let ``Failure(e) >>= f should be Failure(e)`` e (f: int -> Binder<string>) =
            test
                <@ Binder.ofBind (Failure(e))
                   >>= f
                   |> Binder.eval config = Failure(e) @>

    module Map =
        let config = SectionStub.Empty ""

        [<Property>]
        let ``should obey identity law`` (x: Binder<int>) =
            test <@ x |> Binder.map id |> Binder.eval config = (x |> Binder.eval config) @>

        [<Property>]
        let ``should obey associativity law`` x (f: bool -> int) (g: int -> string) =
            test
                <@ x |> Binder.map (f >> g) |> Binder.eval config = (x
                                                                     |> Binder.map f
                                                                     |> Binder.map g
                                                                     |> Binder.eval config) @>

    module Zip =
        let config = SectionStub.Empty ""

        [<Property>]
        let ```zip Success(a) Success(b) should be Success(a, b)`` (a: int) (b: string) =
            test
                <@ Binder.zip (Binder.result a) (Binder.result b)
                   |> Binder.eval config = Success(a, b) @>

        [<Property>]
        let ``zip Failure(e1) Success(b) should be Failure(e1)`` e1 (b: string) =
            test
                <@ Binder.zip (Binder.ofBind (Failure(e1))) (Binder.result b)
                   |> Binder.eval config = Failure(e1) @>

        [<Property>]
        let ```zip Success(a) Failure(e2) should be Failure(e2)`` (a: int) e2 =
            test
                <@ Binder.zip (Binder.result a) (Binder.ofBind (Failure(e2)))
                   |> Binder.eval config = Failure(e2) @>

        [<Property>]
        let ```zip Failure(e1) Failure(e2) should be Failure(e1 append e2)`` e1 e2 =
            test
                <@ Binder.zip (Binder.ofBind (Failure(e1))) (Binder.ofBind (Failure(e2)))
                   |> Binder.eval config = Failure(e1 |> List.append <| e2) @>

module Decode =
    module Bool =
        [<Property>]
        let ``should be Success value if can be converted to bool`` value =
            test
                <@ value
                   |> string
                   |> Decode.bool
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to bool`` path =
            test
                <@ "string"
                   |> Decode.bool
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Boolean'." ]
                ) @>

    module Char =
        [<Property>]
        let ``should be Success value if can be converted to char`` value =
            test
                <@ value
                   |> string
                   |> Decode.char
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to char`` path =
            test
                <@ "string"
                   |> Decode.char
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Char'." ]
                ) @>

    module DateTime =
        [<Property>]
        let ``should be Success value if can be converted to DateTime`` (value: System.DateTime) =
            let value =
                value.AddMilliseconds(-value.Millisecond |> float)

            let string = value.ToString("s")

            test
                <@ string
                   |> Decode.dateTime
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to DateTime`` path =
            test
                <@ "string"
                   |> Decode.dateTime
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.DateTime'." ]
                ) @>

    module Float =
        [<Property>]
        let ``should be Success value if can be converted to float`` () =
            Prop.forAll
                (Arb.from<float>
                 |> Arb.filter System.Double.IsFinite)
                (fun value ->
                    test
                        <@ value
                           |> string
                           |> Decode.float
                           |> Binder.eval (SectionStub.Empty "") = Success(value) @>)

        [<Property>]
        let ``should be Failure if string can not be converted to float`` path =
            test
                <@ "string"
                   |> Decode.float
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Double'." ]
                ) @>

    module Int16 =
        [<Property>]
        let ``should be Success value if can be converted to int16`` value =
            test
                <@ value
                   |> string
                   |> Decode.int16
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to int16`` path =
            test
                <@ "string"
                   |> Decode.int16
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Int16'." ]
                ) @>

    module Int =
        [<Property>]
        let ``should be Success value if can be converted to int`` value =
            test
                <@ value
                   |> string
                   |> Decode.int
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to int`` path =
            test
                <@ "string"
                   |> Decode.int
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Int32'." ]
                ) @>

    module Int64 =
        [<Property>]
        let ``should be Success value if can be converted to int64`` value =
            test
                <@ value
                   |> string
                   |> Decode.int64
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to int64`` path =
            test
                <@ "string"
                   |> Decode.int64
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.Int64'." ]
                ) @>

    module UInt16 =
        [<Property>]
        let ``should be Success value if can be converted to uint16`` value =
            test
                <@ value
                   |> string
                   |> Decode.uint16
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to uint16`` path =
            test
                <@ "string"
                   |> Decode.uint16
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.UInt16'." ]
                ) @>

    module UInt =
        [<Property>]
        let ``should be Success value if can be converted to uint`` value =
            test
                <@ value
                   |> string
                   |> Decode.uint
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to uint`` path =
            test
                <@ "string"
                   |> Decode.uint
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.UInt32'." ]
                ) @>

    module UInt64 =
        [<Property>]
        let ``should be Success value if can be converted to uint64`` value =
            test
                <@ value
                   |> string
                   |> Decode.uint64
                   |> Binder.eval (SectionStub.Empty "") = Success(value) @>

        [<Property>]
        let ``should be Failure if string can not be converted to uint64`` path =
            test
                <@ "string"
                   |> Decode.uint64
                   |> Binder.eval (SectionStub.Empty path) = Failure(
                    [ $"Could not decode 'string' at path '{path}' as type 'System.UInt64'." ]
                ) @>

module Section =
    module Value =
        [<Property>]
        let ``when is not an object should be Success value`` path =
            let section =
                { Children = Seq.empty
                  Path = path
                  Value = "Value" }

            test <@ Section.value |> Binder.eval section = Success("Value") @>

        [<Property>]
        let ``when section has children should be Failure`` path childKey =
            let section =
                { Children =
                      [ { Children = Seq.empty
                          Path = ConfigurationPath.Combine(path, childKey)
                          Value = "Value" } ]
                  Path = path
                  Value = null }

            test
                <@ Section.value |> Binder.eval section = Failure(
                    [ $"Expected a simple value at '{path}' but found an object." ]
                ) @>

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

module GetSection =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists should be Success section`` (ConfigurationPath path) (ConfigurationKey key) =
        let subSection =
            { Children = Seq.empty
              Path = ConfigurationPath.Combine(path, key)
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test <@ key |> getSection |> Binder.eval section = Success(subSection :> IConfigurationSection) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
        test
            <@ key
               |> getSection
               |> Binder.eval (path |> SectionStub.Empty) = Failure([ $"The key '{key}' does not exist at '{path}'." ]) @>

module GetOptSection =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists should be Success Some section`` (ConfigurationPath path) (ConfigurationKey key) =

        let subSection =
            { Children = Seq.empty
              Path = ConfigurationPath.Combine(path, key)
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test <@ key |> getOptSection |> Binder.eval section = Success(subSection :> IConfigurationSection |> Some) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Success None`` (ConfigurationPath path) (ConfigurationKey key) =
        test
            <@ key
               |> getOptSection
               |> Binder.eval (path |> SectionStub.Empty) = Success(None) @>

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

        test <@ value key |> Binder.eval section = Success(x) @>

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
            <@ missingKey |> value |> Binder.eval section = Failure(
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

        test <@ key |> value |> Binder.eval section = Failure([ $"The key '{key}' does not exist at '{path}'." ]) @>

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

        test <@ valueOf Decode.int key |> Binder.eval section = Success(x) @>

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
               |> valueOf Decode.int
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
            <@ key |> valueOf Decode.int |> Binder.eval section = Failure(
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

        test <@ optValue key |> Binder.eval section = Success(Some(x)) @>

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

        test <@ missingKey |> optValue |> Binder.eval section = Success(None) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when prop exists and is null should be Success None`` (ConfigurationPath path) (ConfigurationKey key) =
        let section =
            { Children =
                  [ { Children = Seq.empty
                      Path = ConfigurationPath.Combine(path, key)
                      Value = null } ]
              Path = path
              Value = null }

        test <@ key |> optValue |> Binder.eval section = Success(None) @>

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

        test <@ optValueOf Decode.int key |> Binder.eval section = Success(Some(x)) @>

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
               |> optValueOf Decode.int
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
               |> optValueOf Decode.int
               |> Binder.eval section = Failure(
                [ $"Could not decode 'string' at path '{path}' as type 'System.Int32'." ]
            ) @>

module Builders =
    type SubOptions =
        { OptionalNumber: float option
          Bool: bool }

    type Options =
        { Name: string
          SubOptions: SubOptions
          OptSubOptions: SubOptions option }

    let mkOptions config =
        section "Options" {
            let! name = value "Name"

            and! subOptions =
                section "Sub" {
                    let! optionalNumber = optValueOf Decode.float "MaybeDecimal"
                    and! bool = valueOf Decode.bool "bool"

                    return
                        { OptionalNumber = optionalNumber
                          Bool = bool }
                }

            and! optSubOptions =
                optSection "OptSub" {
                    let! optionalNumber = optValueOf Decode.float "MaybeDecimal"
                    and! bool = valueOf Decode.bool "bool"

                    return
                        { OptionalNumber = optionalNumber
                          Bool = bool }
                }

            return
                { Name = name
                  SubOptions = subOptions
                  OptSubOptions = optSubOptions }
        }
        |> Binder.eval config

    [<Fact>]
    let ``should bind when configuration correct`` () =
        let config =
            { Children =
                  [ { Children =
                          [ { Children = Seq.empty
                              Path = "Name"
                              Value = "A name" }
                            { Children =
                                  [ { Children = Seq.empty
                                      Path = "MaybeDecimal"
                                      Value = "1.0" }
                                    { Children = Seq.empty
                                      Path = "bool"
                                      Value = "true" } ]
                              Path = "Sub"
                              Value = null }
                            { Children =
                                  [ { Children = Seq.empty
                                      Path = "MaybeDecimal"
                                      Value = "2.0" }
                                    { Children = Seq.empty
                                      Path = "bool"
                                      Value = "false" } ]
                              Path = "OptSub"
                              Value = null } ]
                      Path = "Options"
                      Value = null } ]
              Path = System.String.Empty
              Value = null }

        test
            <@ config |> mkOptions = Success(
                { Name = "A name"
                  SubOptions =
                      { OptionalNumber = Some(1.0)
                        Bool = true }
                  OptSubOptions =
                      { OptionalNumber = Some(2.0)
                        Bool = false }
                      |> Some }
            ) @>

    [<Fact>]
    let ``should bind successfully when optional config missing`` () =
        let config =
            { Children =
                  [ { Children =
                          [ { Children = Seq.empty
                              Path = "Name"
                              Value = "A name" }
                            { Children =
                                  [ { Children = Seq.empty
                                      Path = "MaybeDecimal"
                                      Value = null }
                                    { Children = Seq.empty
                                      Path = "bool"
                                      Value = "true" } ]
                              Path = "Sub"
                              Value = null } ]
                      Path = "Options"
                      Value = null } ]
              Path = System.String.Empty
              Value = null }

        test
            <@ config |> mkOptions = Success(
                { Name = "A name"
                  SubOptions = { OptionalNumber = None; Bool = true }
                  OptSubOptions = None }
            ) @>

    [<Fact>]
    let ``should fail when non optional config missing`` () =
        let config =
            { Children =
                  [ { Children =
                          [ { Children = Seq.empty
                              Path = "NotTheNameKey"
                              Value = "A name" }
                            { Children = Seq.empty
                              Path = "Sub"
                              Value = null } ]
                      Path = "Options"
                      Value = null } ]
              Path = System.String.Empty
              Value = null }

        test
            <@ config |> mkOptions = Failure(
                [ "The key 'Name' does not exist at 'Options'."
                  "The key 'Sub' does not exist at 'Options'." ]
            ) @>
