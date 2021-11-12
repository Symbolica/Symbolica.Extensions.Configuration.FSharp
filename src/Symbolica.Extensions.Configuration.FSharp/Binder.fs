namespace Symbolica.Extensions.Configuration.FSharp

/// <summary>
/// A Reader monad that reads configuration and returns a <see cref="BindResult" />.
/// </summary>
/// <remarks>
/// By utilising the Reader monad the caller can avoid having to supply the configuration
/// until the end of the computation. Making the binding code less cluttered and more declarative.
/// </remarks>
type Binder<'config, 'a> = Binder of ('config -> BindResult<'a>)

module Binder =
    /// <summary>Create a <see cref="Binder" /> from a <see cref="BindResult" />.</summary>
    /// <remarks>
    /// Effectively this is creating a <see cref="Binder" /> that doesn't need to read from the configuration.
    /// </remarks>
    /// <param name="bindResult">The <see cref="BindResult" /> from which to create this <see cref="Binder" />.</param>
    /// <returns>A <see cref="Binder" />.</returns>
    let ofBindResult bindResult = Binder(fun _ -> bindResult)

    /// <summary>Create a <see cref="Binder" /> from a plain value.</summary>
    /// <remarks>
    /// This <see cref="Binder" /> does not read from the configuration and always returns <c>Success</c>.
    /// </remarks>
    /// <param name="x">The value to wrap up in a <see cref="Binder" />.</param>
    let result x = x |> BindResult.result |> ofBindResult

    /// <summary>Create a <see cref="Binder" /> from an error.</summary>
    /// <remarks>
    /// This <see cref="Binder" /> does not read from the configuration and always returns <c>Failure</c>.
    /// </remarks>
    /// <param name="e">The error to wrap up in a <see cref="Binder" />.</param>
    let fail e = e |> Failure |> ofBindResult

    /// <summary>Unwraps the <see cref="Binder" />.</summary>
    /// <returns>The function which was contained inside in the <see cref="Binder" />.</returns>
    let run (Binder binder) = binder

    /// <summary>Evaluates the <see cref="Binder" /> for the <paramref name="config" />.</summary>
    /// <param name="config">The configuration to bind to.</param>
    /// <param name="binder">The <see cref="Binder" /> to be evaluated.</param>
    /// <returns>A <see cref="BindResult" />.</returns>
    let eval config (binder: Binder<_, _>) = (run binder) config

    /// <summary>Retrieves the configuration environment.</summary>
    let ask = Binder Success

    /// <summary>
    /// Applies the <see cref="Binder" /> <paramref name="a" /> to the function <paramref name="f" />.
    /// </summary>
    /// <remarks>
    /// This applicative instance accumulates errors.
    /// If both <paramref name="f" /> and <paramref name="a" /> are <c>Success</c> then the outcome will be <c>Success</c>;
    /// otherwise the outcome will be a <c>Failure</c> containing all errors.
    /// </remarks>
    /// <param name="f">The function to which the value will be applied.</param>
    /// <param name="a">The value to be applied to the function.</param>
    /// <returns>A <see cref="Binder" /> containing the result of applying the value to the function.</returns>
    let apply f a : Binder<_, _> =
        Binder (fun config ->
            a
            |> eval config
            |> BindResult.apply (f |> eval config))

    /// <summary>Monadic bind for a <see cref="Binder" />.</summary>
    /// <remarks>
    /// If <paramref name="m" /> evaulates to <c>Success</c> then the function <paramref name="f" /> will be evaluated
    /// with the data contained in <paramref name="m" />; otherwise the original <c>Failure</c> will be maintained.
    /// </remarks>
    /// <param name="f">The function to which the value will be bound.</param>
    /// <param name="m">The <see cref="Binder" /> that contains the input value to the function <paramref name="f" />.</param>
    /// <returns>A <see cref="Binder" />.</returns>
    let bind (f: 'a -> Binder<_, 'b>) (m: Binder<_, 'a>) : Binder<_, 'b> =
        Binder (fun config ->
            m
            |> eval config
            |> BindResult.bind (f >> eval config))

    /// <summary>Maps the inputs to the <see cref="Binder" />.</summary>
    /// <param name="f">The function used to map the input.</param>
    /// <param name="m">The <see cref="Binder" /> to be contramapped.</param>
    /// <returns>A <see cref="Binder" /> whose input is mapped by the function <paramref name="f" />.</returns>
    let contramap f (m: Binder<_, _>) = Binder(f >> run m)

    /// <summary>Maps the output of the <see cref="Binder" />.</summary>
    /// <param name="f">The function used to map the output.</param>
    /// <param name="m">The <see cref="Binder" /> to be mapped.</param>
    /// <returns>A <see cref="Binder" /> whose output is mapped by the function <paramref name="f" />.</returns>
    let map f (m: Binder<_, 'a>) : Binder<_, 'b> = Binder(run m >> BindResult.map f)

    /// <summary>
    /// Runs the binder generating function <paramref name="f"/> on the optional value creating a binder for an optional value.
    /// </summary>
    let traverseOpt f : 'b option -> Binder<'config, 'a option> =
        function
        | Some m -> (f m) |> map Some
        | None -> None |> result

    /// <summary>
    /// Turns an <see cref="Option" /> of a <see cref="Binder" /> into a <see cref="Binder" /> of an <see cref="Option" />.
    /// </summary>
    let sequenceOpt (m: Binder<_, 'a> option) : Binder<_, 'a option> = m |> traverseOpt id

    /// <summary>
    /// Runs the binder generating function <paramref name="f"/> on each element of the list creating a binder of a list.
    /// </summary>
    let rec traverseList (f: 'a -> Binder<_, 'b>) (list: 'a list) : Binder<_, 'b list> =
        let cons head tail = head :: tail
        let (<!>) = map
        let (<*>) = apply
        List.foldBack (fun head tail -> cons <!> f head <*> tail) list (result [])

    /// <summary>
    /// Turns a <see cref="List" /> of <see cref="Binder" /> into a <see cref="Binder" /> of <see cref="List" />.
    /// </summary>
    let sequenceList (m: Binder<_, 'a> list) : Binder<_, 'a list> = m |> traverseList id

    /// <summary>
    /// Extends a <see cref="Binder" /> by feeding its output to the input of a subsequent binder.
    /// </summary>
    /// <remarks>
    /// Effectively the value produced by the binder <paramref name="wa"/> becomes the configuration environment from which the
    /// binder <paramref name="wb" /> reads from.
    /// </remarks>
    /// <param name="wb">
    /// The <see cref="Binder" /> that reads from the output of <paramref name="wa" />.
    /// </param>
    /// <param name="wa">
    /// The <see cref="Binder" /> that produces the environment for <paramref name="wb" />.
    /// </param>
    /// <returns>
    /// A <see cref="Binder" /> that is the composition of first evaluating <paramref name="wa" /> and
    /// then evaluating <paramref name="wb" /> using the output of <paramref name="wa" />.
    /// </returns>
    let extend (wb: Binder<'config, _>) (wa: Binder<_, 'config>) = wa |> bind (run wb >> ofBindResult)

    /// <summary>
    /// Extends a <see cref="Binder" /> by feeding its optional output to the input of a subsequent binder.
    /// </summary>
    /// <remarks>
    /// Effectively the value produced by the binder <paramref name="wa"/> becomes the configuration environment from which the
    /// binder <paramref name="wb" /> reads from.
    /// If <paramref name="wa" /> produces <c>None</c> then <paramref name="wb" /> isn't evaluated.
    /// </remarks>
    /// <param name="wb">
    /// The <see cref="Binder" /> that reads from the output of <paramref name="wa" />.
    /// </param>
    /// <param name="wa">
    /// The <see cref="Binder" /> that produces the environment for <paramref name="wb" />.
    /// </param>
    /// <returns>
    /// A <see cref="Binder" /> that is the composition of first evaluating <paramref name="wa" /> and
    /// then evaluating <paramref name="wb" /> using the output of <paramref name="wa" />.
    /// </returns>
    let extendOpt (wb: Binder<'config, 'a>) (wa: Binder<_, 'config option>) : Binder<_, 'a option> =
        wa
        |> bind (BindResult.traverseOpt (run wb) >> ofBindResult)

    /// <summary>Combines two <see cref="Binder" /> instances.</summary>
    /// <remarks>
    /// If both instances are <c>Success</c> then the result is a <c>Success</c> containing a tuple with the value
    /// of both instances.
    /// If at least one instance is a <c>Failure</c> then the result is a <c>Failure</c> holding all of the errors.
    /// </remarks>
    /// <param name="x">The left hand side of the zip.</param>
    /// <param name="y">The right hand side of the zip.</param>
    let zip x y : Binder<_, 'a * 'b> =
        Binder(fun config -> BindResult.zip (x |> eval config) (y |> eval config))
