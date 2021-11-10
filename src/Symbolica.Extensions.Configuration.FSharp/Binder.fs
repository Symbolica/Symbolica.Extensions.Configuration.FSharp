namespace Symbolica.Extensions.Configuration.FSharp

open Microsoft.Extensions.Configuration

/// <summary>
/// A specialised type of Reader monad binding values from an <see cref="IConfiguration" />.
/// </summary>
/// <remarks>
/// By utilising the Reader monad the caller can avoid having to supply an <see cref="IConfiguration" />
/// until the end of the computation. Making the binding code less cluttered and more declarative.
/// </remarks>
type Binder<'a> = Binder of (IConfiguration -> BindResult<'a>)

module Binder =
    /// <summary>Create a <see cref="Binder" /> from a <see cref="BindResult" />.</summary>
    /// <remarks>
    /// Effectively this is creating a <see cref="Binder" /> that doesn't need to read from the <see cref="IConfiguration" />.
    /// </remarks>
    /// <param name="bindResult">The <see cref="BindResult" /> from which to create this <see cref="Binder" />.</param>
    /// <returns>A <see cref="Binder" />.</returns>
    let ofBindResult bindResult = Binder(fun _ -> bindResult)

    /// <summary>Unwraps the <see cref="Binder" />.</summary>
    /// <returns>The function which was contained inside in the <see cref="Binder" />.</returns>
    let run (Binder binder) = binder

    /// <summary>Evaluates the <see cref="Binder" /> for the <paramref name="config" />.</summary>
    /// <param name="config">The <see cref="IConfiguration" /> to bind to.</param>
    /// <param name="binder">The <see cref="Binder" /> to be evaluated.</param>
    /// <returns>A <see cref="BindResult" />.</returns>
    let eval config (binder: Binder<'a>) = (run binder) config

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
    let apply (f: Binder<'a -> 'b>) (a: Binder<'a>) : Binder<'b> =
        Binder(fun config -> BindResult.apply (f |> eval config) (a |> eval config))

    /// <summary>Monadic bind for a <see cref="Binder" />.</summary>
    /// <remarks>
    /// If <paramref name="m" /> is <c>Success</c> then the function <paramref name="f" /> will be evaluated
    /// with the data contained in <paramref name="m" />; otherwise the original <c>Failure</c> will be maintained.
    /// </remarks>
    /// <param name="f">The function to which the value will be bound.</param>
    /// <param name="m">The <see cref="Binder" /> that contains the input value to the function <paramref name="f" />.</param>
    /// <returns>A <see cref="Binder" />.</returns>
    let bind (f: 'a -> Binder<'b>) (m: Binder<'a>) : Binder<'b> =
        Binder (fun config ->
            m
            |> eval config
            |> BindResult.bind (f >> eval config))

    /// <summary>Maps the inputs to the <see cref="Binder" />.</summary>
    /// <param name="f">The function used to map the input.</param>
    /// <param name="m">The <see cref="Binder" /> to be contramapped.</param>
    /// <returns>A <see cref="Binder" /> whose input is mapped by the function <paramref name="f" />.</returns>
    let contramap f (m: Binder<'a>) = Binder(f >> run m)

    /// <summary>Maps the output of the <see cref="Binder" />.</summary>
    /// <param name="f">The function used to map the output.</param>
    /// <param name="m">The <see cref="Binder" /> to be mapped.</param>
    /// <returns>A <see cref="Binder" /> whose output is mapped by the function <paramref name="f" />.</returns>
    let map f (m: Binder<'a>) : Binder<'b> =
        Binder(fun config -> m |> eval config |> BindResult.map f)

    /// <summary>
    /// Nests a <see cref="Binder" /> of a child <see cref="IConfiguration" /> under a <see cref="Binder" /> of a
    /// parent <see cref="IConfiguration" />.
    /// </summary>
    /// <param name="childBinder">
    /// The <see cref="Binder" /> that operates on the child <see cref="IConfiguration" />.
    /// </param>
    /// <param name="parentBinder">
    /// The <see cref="Binder" /> that extracts the child <see cref="IConfiguration" /> from the parent
    /// <see cref="IConfiguration" />.
    /// </param>
    /// <returns>
    /// A <see cref="Binder" /> that is the composition of first evaluating the <paramref name="parentBinder" /> and
    /// then evaluating the <paramref name="childBinder" />.
    /// </returns>
    let nest (childBinder: Binder<'a>) parentBinder =
        Binder (fun parent ->
            parentBinder
            |> eval parent
            |> BindResult.bind (fun subSection -> childBinder |> eval subSection))

    /// <summary>
    /// Nests a <see cref="Binder" /> of a child <see cref="IConfiguration" /> under an optional <see cref="Binder" />
    /// of a parent <see cref="IConfiguration" />.
    /// </summary>
    /// <param name="childBinder">
    /// The <see cref="Binder" /> that operates on the child <see cref="IConfiguration" />.
    /// </param>
    /// <param name="parentBinder">
    /// The <see cref="Binder" /> that extracts the optional child <see cref="IConfiguration" /> from the parent
    /// <see cref="IConfiguration" />.
    /// </param>
    /// <returns>
    /// A <see cref="Binder" /> that is the composition of first evaluating the <paramref name="parentBinder" /> and
    /// then evaluating the <paramref name="childBinder" />.
    /// </returns>
    let nestOpt (childBinder: Binder<'a>) (parentBinder: Binder<IConfigurationSection option>) =
        Binder (fun parent ->
            parentBinder
            |> eval parent
            |> BindResult.bind (function
                | Some subSection ->
                    childBinder
                    |> eval subSection
                    |> BindResult.map Some
                | None -> None |> Success))

    /// <summary>Create a <see cref="Binder" /> from a plain value.</summary>
    /// <remarks>
    /// This <see cref="Binder" /> does not read from the <see cref="IConfiguration" /> and always returns <c>Success</c>.
    /// </remarks>
    /// <param name="x">The value to lift up in a <see cref="Binder" />.</param>
    let result x = x |> Success |> ofBindResult

    /// <summary>Combines two <see cref="Binder" /> instances.</summary>
    /// <remarks>
    /// If both instances are <c>Success</c> then the result is a <c>Success</c> containing a tuple with the value
    /// of both instances.
    /// If at least one instance is a <c>Failure</c> then the result is a <c>Failure</c> holding all of the errors.
    /// </remarks>
    /// <param name="x">The left hand side of the zip.</param>
    /// <param name="y">The right hand side of the zip.</param>
    let zip x y : Binder<'a * 'b> =
        Binder(fun config -> BindResult.zip (x |> eval config) (y |> eval config))

    /// <summary>
    /// Attempts to bind the child <see cref="IConfiguration" /> located at the <paramref name="key" /> of the
    /// input <see cref="IConfiguration" />.
    /// </summary>
    /// <param name="key">The key at which to try and find a child <see cref="IConfiguration" />.</param>
    /// <returns>
    /// A <see cref="Binder" /> whose input is a parent <see cref="IConfiguration" /> and whose output when
    /// evaluated is the child <see cref="IConfiguration" />.
    /// </returns>
    let section key =
        Binder (fun parent ->
            let section = parent.GetSection(key)

            if section.Exists() then
                Success section
            else
                [ $"The key '{key}' does not exist at '{parent |> path}'." ]
                |> Failure)

    /// <summary>
    /// Binds the optional child <see cref="IConfiguration" /> located at the <paramref name="key" /> of the
    /// input <see cref="IConfiguration" />.
    /// </summary>
    /// <param name="key">The key at which to try and find a child <see cref="IConfiguration" />.</param>
    /// <returns>
    /// A <see cref="Binder" /> whose input is a parent <see cref="IConfiguration" /> and whose output when
    /// evaluated is the optional child <see cref="IConfiguration" />.
    /// </returns>
    let optSection key =
        Binder (fun parent ->
            let section = parent.GetSection(key)

            Success(
                if section.Exists() then
                    Some section
                else
                    None
            ))

    module Section =
        /// <summary>
        /// Attempts to bind the value of the current <see cref="IConfiguration" /> .
        /// </summary>
        let value =
            Binder (fun section ->
                if section.GetChildren() |> Seq.isEmpty then
                    match section with
                    | :? IConfigurationSection as s -> s.Value |> Success
                    | _ ->
                        failwith
                            $"Expected to bind from an IConfigurationSection, but was binding from an {section.GetType()}"
                else
                    [ $"Expected a simple value at '{section |> path}' but found an object." ]
                    |> Failure)

        /// <summary>
        /// Attempts to bind the value of the current <see cref="IConfiguration" /> using the <paramref name="decoder"/>.
        /// </summary>
        /// <param name="decoder">The decoder to apply to the value.</param>
        let valueOf decoder = value |> bind decoder

    type Builder() =
        member _.Bind(x: Binder<'a>, f) = x |> bind f
        member _.BindReturn(x: Binder<'a>, f) = x |> map f
        member _.MergeSources(x1, x2) = zip x1 x2
        member _.Return(x: 'a) : Binder<'a> = x |> result
        member _.ReturnFrom(x: Binder<'a>) = x
