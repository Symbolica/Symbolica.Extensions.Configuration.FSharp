namespace Symbolica.Extensions.Configuration.FSharp

type BindResult<'a> =
    | Success of 'a
    | Failure of string list

module BindResult =

    /// <summary>Create a <see cref="BindResult" /> of <c>Success</c> from a plain value.</summary>
    /// <param name="x">The value to lift up in a <see cref="BindResult" />.</param>
    let result x = x |> Success

    /// <summary>
    /// Applies the <see cref="BindResult" /> <paramref name="a" /> to the function <paramref name="f" />.
    /// </summary>
    /// <remarks>
    /// This applicative instance accumulates errors.
    /// If both <paramref name="f" /> and <paramref name="a" /> are <c>Success</c> then the outcome will be <c>Success</c>;
    /// otherwise the outcome will be a <c>Failure</c> containing all errors.
    /// </remarks>
    /// <param name="f">The function to which the value will be applied.</param>
    /// <param name="a">The value to be applied to the function.</param>
    /// <returns>A <see cref="BindResult" /> containing the result of applying the value to the function.</returns>
    let apply f a : BindResult<'b> =
        match f, a with
        | Failure e1, Failure e2 -> Failure(List.append e1 e2)
        | Failure e1, Success _ -> e1 |> Failure
        | Success _, Failure e2 -> e2 |> Failure
        | Success f, Success a -> a |> f |> Success

    /// <summary>Monadic bind for a <see cref="BindResult" />.</summary>
    /// <remarks>
    /// If the input <see cref="BindResult" /> is <c>Success</c> then the function <paramref name="f" /> will be evaluated
    /// with the data contained in that <see cref="BindResult" />; otherwise the original <c>Failure</c> will be maintained.
    /// </remarks>
    /// <param name="f">The function to which the value will be bound.</param>
    /// <returns>A <see cref="BindResult" />.</returns>
    let bind f : BindResult<'a> -> BindResult<'b> =
        function
        | Success x -> x |> f
        | Failure e -> e |> Failure

    /// <summary>
    /// Extracts the value from <see cref="BindResult" /> applying the compensation function <paramref name="f" />
    /// in the <c>Failure</c> case.
    /// </summary>
    let defaultWith f =
        function
        | Success x -> x
        | Failure e -> e |> f

    /// <summary>Maps the output of the <see cref="BindResult" />.</summary>
    /// <param name="f">The function used to map the output.</param>
    /// <returns>A <see cref="BindResult" /> whose output is mapped by the function <paramref name="f" />.</returns>
    let map f : BindResult<'a> -> BindResult<'b> =
        function
        | Success x -> x |> f |> Success
        | Failure e -> e |> Failure

    /// <summary>
    /// Runs the result generating function <paramref name="f"/> on the optional value creating a result of an optional value.
    /// </summary>
    let traverseOpt f : 'b option -> BindResult<'a option> =
        function
        | Some m -> (f m) |> map Some
        | None -> None |> result

    /// <summary>
    /// Turns an <see cref="Option" /> of a <see cref="BindResult" /> into a <see cref="BindResult" /> of an <see cref="Option" />.
    /// </summary>
    let sequenceOpt (m: BindResult<'a> option) : BindResult<'a option> = m |> traverseOpt id

    /// <summary>
    /// Runs the result generating function <paramref name="f"/> on each element of the list creating a result of a list.
    /// </summary>
    let rec traverseList (f: 'a -> BindResult<'b>) (list: 'a list) : BindResult<'b list> =
        let cons head tail = head :: tail
        let (<!>) = map
        let (<*>) = apply
        List.foldBack (fun head tail -> cons <!> f head <*> tail) list (result [])

    /// <summary>
    /// Turns a <see cref="List" /> of <see cref="BindResult" /> into a <see cref="BindResult" /> of <see cref="List" />.
    /// </summary>
    let sequenceList (m: BindResult<'a> list) : BindResult<'a list> = m |> traverseList id

    /// <summary>Combines two <see cref="BindResult" /> instances.</summary>
    /// <remarks>
    /// If both instances are <c>Success</c> then the result is a <c>Success</c> containing a tuple with the value
    /// of both instances.
    /// If at least one instance is a <c>Failure</c> then the result is a <c>Failure</c> holding all of the errors.
    /// </remarks>
    /// <param name="x">The left hand side of the zip.</param>
    /// <param name="y">The right hand side of the zip.</param>
    let zip x y : BindResult<'a * 'b> =
        match x, y with
        | Failure e1, Failure e2 -> Failure(List.append e1 e2)
        | Failure e1, Success _ -> e1 |> Failure
        | Success _, Failure e2 -> e2 |> Failure
        | Success a, Success b -> Success(a, b)
