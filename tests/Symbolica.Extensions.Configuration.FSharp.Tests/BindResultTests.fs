module Symbolica.Extensions.Configuration.FSharp.BindResult

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote

module Result =
    [<Property>]
    let ``should create Success`` (x: int) =
        test <@ x |> BindResult.result = Success(x) @>

module Apply =
    let (<*>) = BindResult.apply

    [<Property>]
    let ``should obey identity law`` (w: BindResult<int>) = test <@ Success(id) <*> w = w @>

    [<Property>]
    let ``should obey composition law``
        (u: BindResult<int -> string>)
        (v: BindResult<bool -> int>)
        (w: BindResult<bool>)
        =
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

module OfResult =
    [<Property>]
    let ``Ok(x) should be Success(x)`` (x: int) =
        test <@ x |> Ok |> BindResult.ofResult = Success(x) @>

    [<Property>]
    let ``Error(e) should be Failure(e)`` e =
        test <@ e |> Error |> BindResult.ofResult = Failure(e) @>

module Map =
    [<Property>]
    let ``should obey identity law`` (x: BindResult<int>) = test <@ x |> BindResult.map id = x @>

    [<Property>]
    let ``should obey associativity law`` x (f: bool -> int) (g: int -> string) =
        test <@ x |> BindResult.map (f >> g) = (x |> BindResult.map f |> BindResult.map g) @>

module TraverseOpt =
    [<Property>]
    let ``Some(Success(x)) traverse id should be Success(Some(x))`` (x: int) =
        test <@ Some(Success(x)) |> BindResult.traverseOpt id = Success(Some(x)) @>

    [<Property>]
    let ``Some(Failure(e)) traverse id should be Failure(e)`` e =
        test <@ Some(Failure(e)) |> BindResult.traverseOpt id = Failure(e) @>

    [<Property>]
    let ``None traverse id should be Success(None)`` () =
        test <@ None |> BindResult.traverseOpt id = Success(None) @>

module SequenceOpt =
    [<Property>]
    let ``should be equal to traverse id`` (opt: BindResult<string> option) =
        test <@ opt |> BindResult.sequenceOpt = (opt |> BindResult.traverseOpt id) @>

module TraverseList =
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
                    |> List.reduce (List.append)
                ) @>)

    [<Property>]
    let ``[] traverse id should be Success []`` () =
        test
            <@ ([]: BindResult<int> list)
               |> BindResult.traverseList id = Success [] @>

module SequenceList =
    [<Property>]
    let ``should be equal to traverse id`` (list: BindResult<string> list) =
        test <@ list |> BindResult.sequenceList = (list |> BindResult.traverseList id) @>

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
