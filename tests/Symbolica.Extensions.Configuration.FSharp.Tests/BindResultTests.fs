module Symbolica.Extensions.Configuration.FSharp.BindResult

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

module Result =
    [<Property>]
    let ``should create Success`` (x: int) =
        test <@ x |> BindResult.result = Success(x) @>

module Apply =
    type ApErrors = ApplicativeErrors<string>
    type BindResult<'a> = BindResult<'a, ApErrors>

    [<Property>]
    let ``should obey identity law`` (w: BindResult<int>) = test <@ Success(id) <*> w = w @>

    [<Property>]
    let ``should obey composition law``
        (u: BindResult<int -> string>)
        (v: BindResult<bool -> int>)
        (w: BindResult<bool>)
        =
        test <@ (<<) <!> u <*> v <*> w = (u <*> (v <*> w)) @>

    [<Property>]
    let ``should obey homomorphism law`` (f: string -> string) x =
        test <@ Success(f) <*> Success(x) = (Success(x |> f): BindResult<_>) @>

    [<Property>]
    let ``should obey interchange law`` (u: BindResult<string -> string>) x =
        test <@ u <*> (Success(x): BindResult<_>) = ((Success(fun f -> x |> f): BindResult<_>) <*> u) @>

    [<Property>]
    let ``Failure(e1) apply Success(x) should be Failure(e1)`` (e1: ApErrors) (x: int) =
        test
            <@ (Failure(e1): BindResult<_>)
               <*> (Success(x): BindResult<_>) = Failure(e1) @>

    [<Property>]
    let ``Success(f) apply Failure(e2) should be Failure(e2)`` (f: int -> int) (e2: ApErrors) =
        test <@ (Success(f): BindResult<_>) <*> Failure(e2) = Failure(e2) @>

    [<Property>]
    let ``Failure(e1) apply Failure(e2) should be Failure(e1 +& e2)`` (e1: ApErrors) (e2: ApErrors) =
        test <@ Failure(e1) <*> Failure(e2) = Failure(e1 +& e2) @>

module Alt =
    type AltErrors = AltErrors<Error>
    type BindResult<'a> = BindResult<'a, Error>

    [<Property>]
    let ``should obey associativity law`` (u: BindResult<int>) (v: BindResult<_>) (w: BindResult<_>) =
        test <@ u <|> (v <|> w) = ((u <|> v) <|> w) @>

    [<Property>]
    let ``Success(x) <|> u should be Success(x)`` (x: int) (u: BindResult<_>) =
        test <@ Success(x) <|> u = Success(x) @>

    [<Property>]
    let ``Failure(x) <|> Success(y) should be Success(y)`` (x: int) y =
        test <@ Success(x) <|> Success(y) = Success(x) @>

    [<Property>]
    let ``Failure(e1) <|> Failure(e2) should be Failure(e1 +| e2)`` (e1: Error) (e2: Error) =
        test <@ Failure(e1) <|> Failure(e2) = Failure(e1 +| e2) @>

module Bind =
    type BindResult<'a> = BindResult<'a, string>

    [<Property>]
    let ``should obey left identity`` x (f: int -> BindResult<int>) = test <@ Success(x) >>= f = (f x) @>

    [<Property>]
    let ``should obey right identity`` (m: BindResult<int>) = test <@ m >>= Success = m @>

    [<Property>]
    let ``should obey associativity`` (m: BindResult<_>) (f: bool -> BindResult<int>) (g: int -> BindResult<string>) =
        test <@ (m >>= f) >>= g = (m >>= (fun x -> x |> f >>= g)) @>

    [<Property>]
    let ``Failure(e) >>= f should be Failure(e)`` e (f: int -> BindResult<string>) =
        test <@ Failure(e) >>= f = Failure(e) @>

module DefaultWith =
    [<Property>]
    let ``should obey identity law`` (e: string) =
        test <@ Failure(e) |> BindResult.defaultWith id = e @>

    [<Property>]
    let ``Success(x) |> defaultWith f should be x`` (x: int) (f: string -> int) =
        test <@ Success(x) |> BindResult.defaultWith f = x @>

    [<Property>]
    let ``Failure(e) |> defaultWith f should be f e`` e (f: string -> string) =
        test <@ Failure(e) |> BindResult.defaultWith f = (f e) @>

module OfResult =
    [<Property>]
    let ``Ok(x) should be Success(x)`` (x: int) =
        test <@ x |> Ok |> BindResult.ofResult = Success(x) @>

    [<Property>]
    let ``Error(e) should be Failure(e)`` (e: Error) =
        test <@ e |> Error |> BindResult.ofResult = Failure(e) @>

module Map =
    type BindResult<'a> = BindResult<'a, string>

    [<Property>]
    let ``should obey identity law`` (m: BindResult<int>) = test <@ m |> BindResult.map id = m @>

    [<Property>]
    let ``should obey associativity law`` (m: BindResult<bool>) (f: bool -> int) (g: int -> string) =
        test <@ m |> BindResult.map (f >> g) = (m |> BindResult.map f |> BindResult.map g) @>

module MapFailure =
    [<Property>]
    let ``should obey identity law`` (m: BindResult<int, string>) =
        test <@ m |> BindResult.mapFailure id = m @>

    [<Property>]
    let ``should obey associativity law`` (m: BindResult<string, bool>) (f: bool -> int) (g: int -> string) =
        test
            <@ m |> BindResult.mapFailure (f >> g) = (m
                                                      |> BindResult.mapFailure f
                                                      |> BindResult.mapFailure g) @>

module TraverseOpt =
    [<Property>]
    let ``Some(Success(x)) traverse id should be Success(Some(x))`` (x: int) =
        test <@ Some(Success(x)) |> BindResult.traverseOpt id = Success(Some(x)) @>

    [<Property>]
    let ``Some(Failure(e)) traverse id should be Failure(e)`` (e: ApplicativeErrors<Error>) =
        test <@ Some(Failure(e)) |> BindResult.traverseOpt id = Failure(e) @>

    [<Property>]
    let ``None traverse id should be Success(None)`` () =
        test <@ None |> BindResult.traverseOpt id = Success(None) @>

module SequenceOpt =
    type BindResult<'a> = BindResult<'a, ApplicativeErrors<Error>>

    [<Property>]
    let ``should be equal to traverse id`` (opt: BindResult<string> option) =
        test <@ opt |> BindResult.sequenceOpt = (opt |> BindResult.traverseOpt id) @>

module TraverseList =
    type BindResult<'a> = BindResult<'a, ApplicativeErrors<Error>>

    [<Property>]
    let ``[Success(x) * n] traverse id should be Success [x * n]`` (xs: int list) =
        test
            <@ xs
               |> List.map BindResult.result
               |> BindResult.traverseList id = (Success(xs): BindResult<int list>) @>

    [<Property>]
    let ``BindResult list that contains Failure traverse id should be Failure containing all errors``
        (list: BindResult<string> list)
        =
        list
        |> List.exists (function
            | Success _ -> false
            | Failure _ -> true)
        ==> lazy
            (test
                <@ list |> BindResult.traverseList id = Failure(
                    List.foldBack
                        (function
                        | Success _ -> id
                        | Failure e -> fun es -> e :: es)
                        list
                        []
                    |> List.reduce (+&)
                ) @>)

    [<Property>]
    let ``[] traverse id should be Success []`` () =
        test
            <@ ([]: BindResult<int> list)
               |> BindResult.traverseList id = Success [] @>

module SequenceList =
    type BindResult<'a> = BindResult<'a, ApplicativeErrors<Error>>

    [<Property>]
    let ``should be equal to traverse id`` (list: BindResult<string> list) =
        test <@ list |> BindResult.sequenceList = (list |> BindResult.traverseList id) @>

module Zip =
    type ApErrors = ApplicativeErrors<string>
    type BindResult<'a> = BindResult<'a, ApErrors>

    [<Property>]
    let ```zip Success(a) Success(b) should be Success(a, b)`` (a: int) (b: string) =
        test <@ BindResult.zip (Success(a): BindResult<_>) (Success(b): BindResult<_>) = Success(a, b) @>

    [<Property>]
    let ``zip Failure(e1) Success(b) should be Failure(e1)`` (e1: ApErrors) (b: string) =
        test <@ BindResult.zip (Failure(e1)) (Success(b): BindResult<_>) = Failure(e1) @>

    [<Property>]
    let ```zip Success(a) Failure(e2) should be Failure(e2)`` (a: int) (e2: ApErrors) =
        test <@ BindResult.zip (Success(a): BindResult<_>) (Failure(e2)) = Failure(e2) @>

    [<Property>]
    let ```zip Failure(e1) Failure(e2) should be Failure(e1 Error.and' e2)`` (e1: ApErrors) (e2: ApErrors) =
        test <@ BindResult.zip (Failure(e1)) (Failure(e2)) = Failure(e1 +& e2) @>
