module Symbolica.Extensions.Configuration.FSharp.Binder

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

module OfBindResult =
    [<Property>]
    let ``should create a Binder that ignores the config`` (r: BindResult<int, string>) (config: string) =
        test <@ r |> Binder.ofBindResult |> Binder.eval config = r @>

module Result =
    [<Property>]
    let ``should create a Binder that ignores the config and returns Success`` (x: int) (config: string) =
        test <@ x |> Binder.result |> Binder.eval config = Success(x) @>

module Fail =
    [<Property>]
    let ``should create a Binder that ignores the config and returns Failure`` (e: string) (config: string) =
        test <@ e |> Binder.fail |> Binder.eval config = Failure(e) @>

module Ask =
    [<Property>]
    let ``should eval to Success(config)`` (config: string) =
        test <@ Binder.ask |> Binder.eval config = Success(config) @>

module Apply =
    type ApErrors = ApplicativeErrors<string>
    type Binder<'a> = Binder<string, 'a, ApErrors>

    [<Property>]
    let ``should obey identity law`` (w: Binder<int>) config =
        let id: Binder<_> = Binder.result id
        test <@ (id <*> w) |> Binder.eval config = (w |> Binder.eval config) @>

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
            <@ u <*> (Binder.result x: Binder<_>)
               |> Binder.eval config = (((Binder.result (fun f -> x |> f): Binder<_>) <*> u)
                                        |> Binder.eval config) @>

    [<Property>]
    let ``Failure(e1) apply Success(x) should be Failure(e1)`` (e1: ApErrors) (x: int) config =
        test
            <@ Binder.ofBindResult (Failure(e1))
               <*> (Binder.result x: Binder<_>)
               |> Binder.eval config = Failure(e1) @>

    [<Property>]
    let ``Success(f) apply Failure(e2) should be Failure(e2)`` (f: int -> int) (e2: ApErrors) config =
        test
            <@ (Binder.result f: Binder<_>)
               <*> Binder.ofBindResult (Failure(e2))
               |> Binder.eval config = Failure(e2) @>

    [<Property>]
    let ``Failure(e1) apply Failure(e2) should be Failure(e1 +& e2)`` (e1: ApErrors) (e2: ApErrors) (config: string) =
        test
            <@ Binder.ofBindResult (Failure(e1))
               <*> Binder.ofBindResult (Failure(e2))
               |> Binder.eval config = Failure(e1 +& e2) @>

module Alt =
    type AltErrors = AltErrors<Error>
    type Binder<'a> = Binder<string, 'a, Error>

    [<Property>]
    let ``should obey associativity law`` (u: Binder<int>) (v: Binder<_>) (w: Binder<_>) config =
        test <@ u <|> (v <|> w) |> Binder.eval config = (((u <|> v) <|> w) |> Binder.eval config) @>

    [<Property>]
    let ``Success(x) <|> u should be Success(x)`` (x: int) (u: Binder<_>) config =
        test
            <@ Binder.ofBindResult (Success(x)) <|> u
               |> Binder.eval config = Success(x) @>

    [<Property>]
    let ``Failure(x) <|> Success(y) should be Success(y)`` (x: int) y (config: string) =
        test
            <@ Binder.ofBindResult (Success(x))
               <|> Binder.ofBindResult (Success(y))
               |> Binder.eval config = Success(x) @>

    [<Property>]
    let ``Failure(e1) <|> Failure(e2) should be Failure(e1 +| e2)`` (e1: Error) (e2: Error) (config: string) =
        test
            <@ Binder.ofBindResult (Failure(e1))
               <|> Binder.ofBindResult (Failure(e2))
               |> Binder.eval config = Failure(e1 +| e2) @>

module Bind =
    type Binder<'a> = Binder<string, 'a, string>

    [<Property>]
    let ``should obey left identity`` x (f: int -> Binder<int>) config =
        test <@ Binder.result x >>= f |> Binder.eval config = (x |> f |> Binder.eval config) @>

    [<Property>]
    let ``should obey right identity`` (m: Binder<int>) config =
        test <@ m >>= Binder.result |> Binder.eval config = (m |> Binder.eval config) @>

    [<Property>]
    let ``should obey associativity`` (m: Binder<_>) (f: bool -> Binder<int>) (g: int -> Binder<string>) config =
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

module ContraMap =
    type Binder<'config, 'a> = Binder<'config, 'a, string>

    [<Property>]
    let ``should obey identity law`` (m: Binder<string, int>) config =
        test <@ m |> Binder.contramap id |> Binder.eval config = (m |> Binder.eval config) @>

    [<Property>]
    let ``should obey associativity law`` (m: Binder<bool, bool>) (f: string -> int) (g: int -> bool) config =
        test
            <@ m
               |> Binder.contramap (f >> g)
               |> Binder.eval config = (m
                                        |> Binder.contramap g
                                        |> Binder.contramap f
                                        |> Binder.eval config) @>

module Map =
    type Binder<'a> = Binder<string, 'a, string>

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

module MapFailure =
    type Binder<'a, 'e> = Binder<string, 'a, 'e>

    [<Property>]
    let ``should obey identity law`` (m: Binder<int, string>) config =
        test <@ m |> Binder.mapFailure id |> Binder.eval config = (m |> Binder.eval config) @>

    [<Property>]
    let ``should obey associativity law`` (m: Binder<string, bool>) (f: bool -> int) (g: int -> string) config =
        test
            <@ m
               |> Binder.mapFailure (f >> g)
               |> Binder.eval config = (m
                                        |> Binder.mapFailure f
                                        |> Binder.mapFailure g
                                        |> Binder.eval config) @>

module TraverseOpt =
    [<Property>]
    let ``Some(Success(x)) traverse id should be Success(Some(x))`` (x: int) (config: string) =
        test
            <@ Some(Success(x) |> Binder.ofBindResult)
               |> Binder.traverseOpt id
               |> Binder.eval config = Success(Some(x)) @>

    [<Property>]
    let ``Some(Failure(e)) traverse id should be Failure(e)`` (e: ApplicativeErrors<Error>) (config: string) =
        test
            <@ Some(Failure(e) |> Binder.ofBindResult)
               |> Binder.traverseOpt id
               |> Binder.eval config = Failure(e) @>

    [<Property>]
    let ``None traverse id should be Success(None)`` (config: string) =
        test
            <@ None
               |> Binder.traverseOpt id
               |> Binder.eval config = Success(None) @>

module SequenceOpt =
    type Binder<'a> = Binder<string, 'a, ApplicativeErrors<Error>>

    [<Property>]
    let ``should be equal to traverse id`` (opt: Binder<string> option) config =
        test
            <@ opt |> Binder.sequenceOpt |> Binder.eval config = (opt |> Binder.traverseOpt id |> Binder.eval config) @>

module TraverseList =
    type BindResult<'a> = BindResult<'a, ApplicativeErrors<Error>>
    type Binder<'a> = Binder<string, 'a, ApplicativeErrors<Error>>

    [<Property>]
    let ``[Success(x) * n] traverse id should be Success [x * n]`` (xs: int list) (config: string) =
        test
            <@ xs
               |> List.map Binder.result
               |> Binder.traverseList id
               |> Binder.eval config = (Success(xs): BindResult<int list>) @>

    [<Property>]
    let ``Binder list that contains Failure traverse id should be Failure containing all errors``
        (list: BindResult<string> list)
        (config: string)
        =
        list
        |> List.exists (function
            | Success _ -> false
            | Failure _ -> true)
        ==> lazy
            (test
                <@ list
                   |> List.map Binder.ofBindResult
                   |> Binder.traverseList id
                   |> Binder.eval config = Failure(
                    List.foldBack
                        (function
                        | Success _ -> id
                        | Failure e -> fun es -> e :: es)
                        list
                        []
                    |> List.reduce (+&)
                ) @>)

    [<Property>]
    let ``[] traverse id should be Success []`` config =
        test
            <@ ([]: Binder<int> list)
               |> Binder.traverseList id
               |> Binder.eval config = Success [] @>

module SequenceList =
    type Binder<'a> = Binder<string, 'a, ApplicativeErrors<Error>>

    [<Property>]
    let ``should be equal to traverse id`` (list: Binder<string> list) config =
        test
            <@ list |> Binder.sequenceList |> Binder.eval config = (list
                                                                    |> Binder.traverseList id
                                                                    |> Binder.eval config) @>

module Extend =
    type Binder<'a> = Binder<string, 'a, string>

    [<Property>]
    let ``ask |> extend m should be m`` (m: Binder<string>) config =
        test
            <@ Binder.ask
               |> Binder.extend m
               |> Binder.eval config = (m |> Binder.eval config) @>

    [<Property>]
    let ``fail e |> extend m should be Failure(e)`` (e: string) (m: Binder<string>) (config: string) =
        test
            <@ e
               |> Binder.fail
               |> Binder.extend m
               |> Binder.eval config = Failure(e) @>

module ExtendOpt =
    type Binder<'a> = Binder<string, 'a, string>

    [<Property>]
    let ``ask |> map Some |> extendOpt m |> eval config should be m |> eval config |> BindResult.map Some``
        (m: Binder<string>)
        config
        =
        test
            <@ Binder.ask
               |> Binder.map Some
               |> Binder.extendOpt m
               |> Binder.eval config = (m |> Binder.eval config |> BindResult.map Some) @>

    [<Property>]
    let ``result None |> extendOpt m |> eval config should be Success(None)`` (m: Binder<string>) (config: string) =
        test
            <@ Binder.result None
               |> Binder.extendOpt m
               |> Binder.eval config = Success(None) @>

    [<Property>]
    let ``fail e |> extend m |> eval config should be Failure(e)`` (e: string) (m: Binder<string>) (config: string) =
        test
            <@ e
               |> Binder.fail
               |> Binder.extendOpt m
               |> Binder.eval config = Failure(e) @>

module Zip =
    type ApErrors = ApplicativeErrors<string>
    type Binder<'a> = Binder<string, 'a, ApErrors>

    [<Property>]
    let ```zip Success(a) Success(b) should be Success(a, b)`` (a: int) (b: string) config =
        test
            <@ Binder.zip (Binder.result a: Binder<_>) (Binder.result b: Binder<_>)
               |> Binder.eval config = Success(a, b) @>

    [<Property>]
    let ``zip Failure(e1) Success(b) should be Failure(e1)`` (e1: ApErrors) (b: string) config =
        test
            <@ Binder.zip (Binder.ofBindResult (Failure(e1))) (Binder.result b: Binder<_>)
               |> Binder.eval config = Failure(e1) @>

    [<Property>]
    let ```zip Success(a) Failure(e2) should be Failure(e2)`` (a: int) (e2: ApErrors) config =
        test
            <@ Binder.zip (Binder.result a: Binder<_>) (Binder.ofBindResult (Failure(e2)))
               |> Binder.eval config = Failure(e2) @>

    [<Property>]
    let ```zip Failure(e1) Failure(e2) should be Failure(e1 Error.and' e2)``
        (e1: ApErrors)
        (e2: ApErrors)
        (config: string)
        =
        test
            <@ Binder.zip (Binder.ofBindResult (Failure(e1))) (Binder.ofBindResult (Failure(e2)))
               |> Binder.eval config = Failure(e1 +& e2) @>
