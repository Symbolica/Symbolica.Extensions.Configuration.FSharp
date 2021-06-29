namespace Symbolica.Extensions.Configuration.FSharp

open FsCheck
open FsCheck.Xunit
open Microsoft.Extensions.Configuration
open Swensen.Unquote

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
                <@ Binder.ofBindResult (Failure(e1)) <*> Binder.result x
                   |> Binder.eval config = Failure(e1) @>

        [<Property>]
        let ``Success(f) apply Failure(e2) should be Failure(e2)`` (f: int -> int) e2 =
            test
                <@ Binder.result f <*> Binder.ofBindResult (Failure(e2))
                   |> Binder.eval config = Failure(e2) @>

        [<Property>]
        let ``Failure(e1) apply Failure(e2) should be Failure(e1 append e2)`` e1 e2 =
            test
                <@ Binder.ofBindResult (Failure(e1))
                   <*> Binder.ofBindResult (Failure(e2))
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
                <@ Binder.ofBindResult (Failure(e))
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
                <@ Binder.zip (Binder.ofBindResult (Failure(e1))) (Binder.result b)
                   |> Binder.eval config = Failure(e1) @>

        [<Property>]
        let ```zip Success(a) Failure(e2) should be Failure(e2)`` (a: int) e2 =
            test
                <@ Binder.zip (Binder.result a) (Binder.ofBindResult (Failure(e2)))
                   |> Binder.eval config = Failure(e2) @>

        [<Property>]
        let ```zip Failure(e1) Failure(e2) should be Failure(e1 append e2)`` e1 e2 =
            test
                <@ Binder.zip (Binder.ofBindResult (Failure(e1))) (Binder.ofBindResult (Failure(e2)))
                   |> Binder.eval config = Failure(e1 |> List.append <| e2) @>

    module Section =
        module Value =
            [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
            let ``when is not an object should be Success value`` (ConfigurationPath path) =
                let section =
                    { Children = Seq.empty
                      Path = path
                      Value = "Value" }

                test <@ Binder.Section.value |> Binder.eval section = Success("Value") @>

            [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
            let ``when section has children should be Failure`` (ConfigurationPath path) (ConfigurationKey childKey) =
                let section =
                    { Children =
                          [ { Children = Seq.empty
                              Path = ConfigurationPath.Combine(path, childKey)
                              Value = "Value" } ]
                      Path = path
                      Value = null }

                test
                    <@ Binder.Section.value |> Binder.eval section = Failure(
                        [ $"Expected a simple value at '{path}' but found an object." ]
                    ) @>

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

            test <@ key |> Binder.section |> Binder.eval section = Success(subSection :> IConfigurationSection) @>

        [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
        let ``when section does not exist should be Failure`` (ConfigurationPath path) (ConfigurationKey key) =
            test
                <@ key
                   |> Binder.section
                   |> Binder.eval (path |> SectionStub.Empty) = Failure(
                    [ $"The key '{key}' does not exist at '{path}'." ]
                ) @>

    module OptSection =
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

            test
                <@ key |> Binder.optSection |> Binder.eval section = Success(
                    subSection :> IConfigurationSection |> Some
                ) @>

        [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
        let ``when section does not exist should be Success None`` (ConfigurationPath path) (ConfigurationKey key) =
            test
                <@ key
                   |> Binder.optSection
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
