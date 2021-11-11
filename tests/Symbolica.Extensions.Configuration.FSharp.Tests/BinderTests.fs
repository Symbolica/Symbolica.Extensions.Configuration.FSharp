module Symbolica.Extensions.Configuration.FSharp.Binder

open FsCheck
open FsCheck.Xunit
open Microsoft.Extensions.Configuration
open Swensen.Unquote

module OfBindResult =
    [<Property>]
    let ``should create a Binder that ignores the config`` (r: BindResult<int>) (config: string) =
        test <@ r |> Binder.ofBindResult |> Binder.eval config = r @>

module Result =
    [<Property>]
    let ``should create a Binder that ignoes the config and returns Success`` (x: int) (config: string) =
        test <@ x |> Binder.result |> Binder.eval config = Success(x) @>

module Apply =
    type Binder<'a> = Binder<string, 'a>

    let (<*>) = Binder.apply

    [<Property>]
    let ``should obey identity law`` (w: Binder<int>) config =
        test <@ (Binder.result id <*> w) |> Binder.eval config = (w |> Binder.eval config) @>

    [<Property>]
    let ``should obey composition law`` (u: Binder<int -> string>) (v: Binder<bool -> int>) (w: Binder<bool>) config =
        let (<<): Binder<_> = Binder.result (<<)
        test <@ (<<) <*> u <*> v <*> w |> Binder.eval config = ((u <*> (v <*> w)) |> Binder.eval config) @>

    [<Property>]
    let ``should obey homomorphism law`` (f: string -> string) x config =
        test
            <@ (Binder.result f: Binder<_>)
               <*> (Binder.result x: Binder<_>)
               |> Binder.eval config = Success(x |> f) @>

    [<Property>]
    let ``should obey interchange law`` (u: Binder<string -> string>) x config =
        test
            <@ u <*> Binder.result x |> Binder.eval config = ((Binder.result (fun f -> x |> f) <*> u)
                                                              |> Binder.eval config) @>

    [<Property>]
    let ``Failure(e1) apply Success(x) should be Failure(e1)`` (e1: string list) (x: int) config =
        test
            <@ Binder.ofBindResult (Failure(e1))
               <*> (Binder.result x: Binder<_>)
               |> Binder.eval config = Failure(e1) @>

    [<Property>]
    let ``Success(f) apply Failure(e2) should be Failure(e2)`` (f: int -> int) (e2: string list) config =
        test
            <@ (Binder.result f: Binder<_>)
               <*> Binder.ofBindResult (Failure(e2))
               |> Binder.eval config = Failure(e2) @>

    [<Property>]
    let ``Failure(e1) apply Failure(e2) should be Failure(e1 append e2)``
        (e1: string list)
        (e2: string list)
        (config: string)
        =
        test
            <@ Binder.ofBindResult (Failure(e1))
               <*> Binder.ofBindResult (Failure(e2))
               |> Binder.eval config = Failure(e1 |> List.append <| e2) @>

module Bind =
    type Binder<'a> = Binder<string, 'a>
    let (>>=) m f = Binder.bind f m

    [<Property>]
    let ``should obey left identity`` x (f: int -> Binder<int>) config =
        test <@ Binder.result x >>= f |> Binder.eval config = (x |> f |> Binder.eval config) @>

    [<Property>]
    let ``should obey right identity`` (m: Binder<int>) config =
        test <@ m >>= Binder.result |> Binder.eval config = (m |> Binder.eval config) @>

    [<Property>]
    let ``should obey associativity`` m (f: bool -> Binder<int>) (g: int -> Binder<string>) config =
        test
            <@ (m >>= f) >>= g |> Binder.eval config = (m
                                                        >>= (fun x -> x |> f >>= g)
                                                        |> Binder.eval config) @>

    [<Property>]
    let ``Failure(e) >>= f should be Failure(e)`` e (f: int -> Binder<string>) config =
        test
            <@ Binder.ofBindResult (Failure(e))
               >>= f
               |> Binder.eval config = Failure(e) @>

module Map =
    type Binder<'a> = Binder<string, 'a>

    [<Property>]
    let ``should obey identity law`` (m: Binder<int>) config =
        test <@ m |> Binder.map id |> Binder.eval config = (m |> Binder.eval config) @>

    [<Property>]
    let ``should obey associativity law`` (m: Binder<bool>) (f: bool -> int) (g: int -> string) config =
        test
            <@ m |> Binder.map (f >> g) |> Binder.eval config = (m
                                                                 |> Binder.map f
                                                                 |> Binder.map g
                                                                 |> Binder.eval config) @>

module Zip =
    type Binder<'a> = Binder<string, 'a>

    [<Property>]
    let ```zip Success(a) Success(b) should be Success(a, b)`` (a: int) (b: string) config =
        test
            <@ Binder.zip (Binder.result a: Binder<_>) (Binder.result b: Binder<_>)
               |> Binder.eval config = Success(a, b) @>

    [<Property>]
    let ``zip Failure(e1) Success(b) should be Failure(e1)`` (e1: string list) (b: string) config =
        test
            <@ Binder.zip (Binder.ofBindResult (Failure(e1))) (Binder.result b: Binder<_>)
               |> Binder.eval config = Failure(e1) @>

    [<Property>]
    let ```zip Success(a) Failure(e2) should be Failure(e2)`` (a: int) (e2: string list) config =
        test
            <@ Binder.zip (Binder.result a: Binder<_>) (Binder.ofBindResult (Failure(e2)))
               |> Binder.eval config = Failure(e2) @>

    [<Property>]
    let ```zip Failure(e1) Failure(e2) should be Failure(e1 append e2)``
        (e1: string list)
        (e2: string list)
        (config: string)
        =
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
               |> Binder.eval (path |> SectionStub.Empty) = Failure([ $"The key '{key}' does not exist at '{path}'." ]) @>

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
            <@ key |> Binder.optSection |> Binder.eval section = Success(subSection :> IConfigurationSection |> Some) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Success None`` (ConfigurationPath path) (ConfigurationKey key) =
        test
            <@ key
               |> Binder.optSection
               |> Binder.eval (path |> SectionStub.Empty) = Success(None) @>
