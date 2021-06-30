namespace Symbolica.Extensions.Configuration.FSharp

type BindResult<'a> =
    | Success of 'a
    | Failure of string list

module BindResult =

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
    /// <returns>A <see cref="BindResult" /> whose output is mapped by <paramref name="f" />.</returns>
    let map f : BindResult<'a> -> BindResult<'b> =
        function
        | Success x -> x |> f |> Success
        | Failure e -> e |> Failure

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
