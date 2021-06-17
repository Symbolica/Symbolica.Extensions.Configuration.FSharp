namespace Symbolica.Extensions.Configuration.FSharp

type BindResult<'a> =
    | Success of 'a
    | Failure of string list

module BindResult =

    let apply f a : BindResult<'b> =
        match f, a with
        | Failure e1, Failure e2 -> Failure(List.append e1 e2)
        | Failure e1, Success _ -> e1 |> Failure
        | Success _, Failure e2 -> e2 |> Failure
        | Success f, Success a -> a |> f |> Success

    let bind f : BindResult<'a> -> BindResult<'b> =
        function
        | Success x -> x |> f
        | Failure e -> e |> Failure

    let defaultWith f =
        function
        | Success x -> x
        | Failure e -> e |> f

    let map f : BindResult<'a> -> BindResult<'b> =
        function
        | Success x -> x |> f |> Success
        | Failure e -> e |> Failure

    let zip x y : BindResult<'a * 'b> =
        match x, y with
        | Failure e1, Failure e2 -> Failure(List.append e1 e2)
        | Failure e1, Success _ -> e1 |> Failure
        | Success _, Failure e2 -> e2 |> Failure
        | Success a, Success b -> Success(a, b)
