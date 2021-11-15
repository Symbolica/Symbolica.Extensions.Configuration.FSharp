module Symbolica.Extensions.Configuration.FSharp.Errors

open FsCheck.Xunit
open Swensen.Unquote

module ApplicativeErrors =
    module op_PlusAmpersand =
        type Error =
            { Error: string }
            interface IToApSemiGroup<ApplicativeErrors<string>> with
                member x.ToSemiGroup() = x.Error |> ApplicativeErrors.single

        [<Property>]
        let ``should append e2 to e1`` (e1: ApplicativeErrors<_>) (e2: Error) =
            test
                <@ e1 +& e2 |> ApplicativeErrors.toList = [ yield! e1 |> ApplicativeErrors.toList
                                                            e2.Error ] @>

        [<Property>]
        let ``should obey associativity law``
            (a: ApplicativeErrors<string>)
            (b: ApplicativeErrors<_>)
            (c: ApplicativeErrors<_>)
            =
            test <@ (a +& b) +& c = (a +& (b +& c)) @>

    module Map =
        [<Property>]
        let ``should obey identity law`` (e: ApplicativeErrors<string>) =
            test <@ e |> ApplicativeErrors.map id = e @>

        [<Property>]
        let ``should obey associativity law`` (e: ApplicativeErrors<_>) (f: bool -> int) (g: int -> string) =
            test
                <@ e |> ApplicativeErrors.map (f >> g) = (e
                                                          |> ApplicativeErrors.map f
                                                          |> ApplicativeErrors.map g) @>

    module Single =
        [<Property>]
        let ``should create a single error`` (x: string) =
            test
                <@ x
                   |> ApplicativeErrors.single
                   |> ApplicativeErrors.toList = [ x ] @>

    module ToList =
        [<Property>]
        let ``should be first followed by rest`` (e: ApplicativeErrors<string>) =
            test <@ e |> ApplicativeErrors.toList = e.First :: e.Rest @>

module AltErrors =
    module op_PlusBar =
        type Error =
            { Error: string }
            interface IAltSemiGroup<Error, AltErrors<Error>> with
                member x.Append y =
                    match y with
                    | AltSemiGroupItem.SemiGroup errors -> errors |> AltErrors.cons x
                    | AltSemiGroupItem.Item error -> AltErrors.pair (x, error)

                member x.ToItem() = AltSemiGroupItem.Item x

        [<Property>]
        let ``should append e2 to e1`` (e1: AltErrors<_>) (e2: Error) =
            test <@ e1 +| e2 |> AltErrors.toList = [ yield! e1 |> AltErrors.toList; e2 ] @>

        [<Property>]
        let ``should obey associativity law`` (a: AltErrors<Error>) (b: AltErrors<_>) (c: AltErrors<_>) =
            test <@ (a +| b) +| c = (a +| (b +| c)) @>

    module Map =
        [<Property>]
        let ``should obey identity law`` (e: AltErrors<Error>) = test <@ e |> AltErrors.map id = e @>

        [<Property>]
        let ``should obey associativity law`` (e: AltErrors<_>) (f: bool -> int) (g: int -> string) =
            test <@ e |> AltErrors.map (f >> g) = (e |> AltErrors.map f |> AltErrors.map g) @>

    module Pair =
        [<Property>]
        let ``should create a new AltError from a pair of individual errors`` (x: string) (y: string) =
            test <@ (x, y) |> AltErrors.pair |> AltErrors.toList = [ x; y ] @>

    module Cons =
        [<Property>]
        let ``should create cons a single errors on to the front of an AltError`` (x: string) e =
            test <@ e |> AltErrors.cons x |> AltErrors.toList = [ x; yield! e |> AltErrors.toList ] @>

    module ToList =
        [<Property>]
        let ``should be First followed by Second followed by Rest`` (e: AltErrors<string>) =
            test <@ e |> AltErrors.toList = e.First :: e.Second :: e.Rest @>

module Errors =
    module Map =
        [<Property>]
        let ``should obey identity law`` (e: Errors<string>) = test <@ e |> Errors.map id = e @>

        [<Property>]
        let ``should obey associativity law`` (e: Errors<_>) (f: bool -> int) (g: int -> string) =
            test <@ e |> Errors.map (f >> g) = (e |> Errors.map f |> Errors.map g) @>

module ValueError =
    module op_PlusAmpersand =
        [<Property>]
        let ``should append e2 to e1`` (e1: ValueError) (e2: ValueError) =
            test <@ e1 +& e2 |> ApplicativeErrors.toList = [ e1; e2 ] @>

        [<Property>]
        let ``should obey associativity law`` (a: ValueError) (b: ValueError) (c: ValueError) =
            test <@ (a +& b) +& c = (a +& (b +& c)) @>

    module op_PlusBar =
        [<Property>]
        let ``should append e2 to e1`` (e1: ValueError) (e2: ValueError) =
            test <@ e1 +| e2 |> AltErrors.toList = [ e1; e2 ] @>

        [<Property>]
        let ``should obey associativity law`` (a: ValueError) (b: ValueError) (c: ValueError) =
            test <@ (a +| b) +| c = (a +| (b +| c)) @>

module Error =
    module op_PlusAmpersand =
        [<Property>]
        let ``should append e2 to e1`` (e1: Error) (e2: Error) =
            test <@ e1 +& e2 |> ApplicativeErrors.toList = [ e1; e2 ] @>

        [<Property>]
        let ``should obey associativity law`` (a: Error) (b: Error) (c: Error) =
            test <@ (a +& b) +& c = (a +& (b +& c)) @>

    module op_PlusBar =
        [<Property>]
        let ``should append e2 to e1`` (e1: Error) (e2: Error) =
            test <@ e1 +| e2 |> AltErrors.toList = [ e1; e2 ] @>

        [<Property>]
        let ``should obey associativity law`` (a: Error) (b: Error) (c: Error) =
            test <@ (a +| b) +| c = (a +| (b +| c)) @>
