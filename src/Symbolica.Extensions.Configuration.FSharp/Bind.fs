namespace Symbolica.Extensions.Configuration.FSharp

open System

type Bind() =
    member _.Bind(x: Binder<_, _, _>, f) = x |> Binder.bind f
    member _.BindReturn(x: Binder<_, 'a, Errors<_>>, f) : Binder<_, 'b, Errors<_>> = x |> Binder.map f

    member this.BindReturn(x: Binder<_, 'a, ApplicativeErrors<Error>>, f) : Binder<_, 'b, Errors<_>> =
        this.BindReturn(x |> Binder.mapFailure Errors.AllOf, f)

    member this.BindReturn(x: Binder<_, 'a, Error>, f) : Binder<_, 'b, Errors<_>> =
        this.BindReturn(x |> Binder.mapFailure Errors.single, f)

    member _.MergeSources(x1, x2) = Binder.zip x1 x2
    member _.Return x : Binder<_, 'a, Errors<_>> = x |> Binder.result
    member _.ReturnFrom(x: Binder<_, _, Errors<_>>) : Binder<_, _, Errors<_>> = x

    member _.ReturnFrom(x: Binder<_, _, ApplicativeErrors<Error>>) : Binder<_, _, Errors<_>> =
        x |> Binder.mapFailure Errors.AllOf

    member _.ReturnFrom(x: Binder<_, _, Error>) : Binder<_, _, Errors<_>> = x |> Binder.mapFailure Errors.single

[<AutoOpen>]
module Builder =
    /// <summary>A computation expression that is used to create a binder which binds the data of config section to a user defined type.</summary>
    /// <returns>An instance of the builder.</returns>
    let bind = Bind()

/// Contains binders for common types and combinator functions for building new binders.
module Bind =
    /// <summary>A combinator for taking an existing binder and nesting it under a parent section at the given key.</summary>
    /// <param name="key">The key of the child section to which the <paramref name="sectionBinder" /> should be bound.</param>
    /// <param name="sectionBinder">The binder that binds the child section to some type.</param>
    /// <returns>A binder for the section at the <paramref name="key" />.</returns>
    let section key (sectionBinder: Binder<_, 'a, Errors<Error>>) =
        Config.section key
        |> Binder.mapFailure Errors.single
        |> Binder.extend sectionBinder
        |> Binder.mapFailure (fun e -> Error.SectionError(key, e))

    /// <summary>A combinator for taking an existing binder and nesting it under an optional parent section at the given key.</summary>
    /// <remarks>If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.</remarks>
    /// <param name="key">The key of the child section to which the <paramref name="sectionBinder" /> should be bound.</param>
    /// <param name="sectionBinder">The binder that binds the child section to some type.</param>
    /// <returns>A binder for the section at the <paramref name="key" />.</returns>
    let optSection key sectionBinder =
        Config.optSection key
        |> Binder.extendOpt sectionBinder
        |> Binder.mapFailure (fun e -> Error.SectionError(key, e))

    let private decode decoder =
        Binder (fun value ->
            decoder
            |> Binder.eval value
            |> BindResult.mapFailure (fun error -> Error.ValueError(value, error)))

    /// <summary>Binds the value of this config section with the <paramref name="decoder" />.</summary>
    /// <param name="decoder">The binder to use when converting the string value.</param>
    /// <returns>A binder for the value at the current config section.</returns>
    let value decoder : Binder<'config, 'a, Errors<Error>> =
        Config.value
        |> Binder.extend (decode decoder)
        |> Binder.mapFailure Errors.single

    /// <summary>Binds the value at the <paramref name="key" /> with the <paramref name="decoder" />.</summary>
    /// <param name="key">The key whose value should be bound.</param>
    /// <param name="decoder">The binder to use when converting the string value.</param>
    /// <returns>A binder for the value at the <paramref name="key" />.</returns>
    let valueAt key decoder : Binder<'config, 'a, Error> = section key (value decoder)

    /// <summary>Binds the optional value at the <paramref name="key" /> with the <paramref name="decoder" />.</summary>
    /// <remarks>If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.</remarks>
    /// <param name="key">The key whose value should be bound.</param>
    /// <param name="decoder">The binder to use when converting the string value.</param>
    /// <returns>A binder for the optional value at the <paramref name="key" />.</returns>
    let optValueAt key decoder : Binder<'config, 'a option, Error> = optSection key (value decoder)

    /// <summary>
    /// Creates a new <see cref="Binder" /> that produces a list of results from evaluating all of the <paramref name="binders" />
    /// with the same configuration.
    /// </summary>
    /// <remarks>
    /// All must complete with <c>Success</c> in order for the new <see cref="Binder" /> to be a <c>Success</c>.
    /// If any fail then this new <see cref="Binder" /> will evaluate to a <c>Failure</c> containing all of the errors.
    /// </remarks>
    let allOf binders =
        binders
        |> Binder.traverseList (Binder.mapFailure ApplicativeErrors.single)
        |> Binder.mapFailure Errors.AllOf

    /// <summary>
    /// Creates a new <see cref="Binder" /> that produces a list of only the <c>Success</c> results from evaluating all of the
    /// <paramref name="binders" /> with the same configuration.
    /// </summary>
    /// <remarks>
    /// This new <see cref="Binder" /> will never evaluate to a <c>Failure</c>.
    /// If all fail then it will evaluate to a <c>Success []</c>.
    /// </remarks>
    let anyOf binders =
        Binder (fun config ->
            let folder head tail =
                match head |> Binder.eval config with
                | Success x -> x :: tail
                | Failure _ -> tail

            List.foldBack folder binders [] |> Success)

    /// <summary>
    /// Takes a <see cref="Binder" /> that can fail with <see cref="AltErrors" /> and maps it to <see cref="Errors" />.
    /// </summary>
    /// <remarks>
    /// To create the <paramref name="binders" /> use the <c>&lt;|&gt;</c> operator. E.g. <c>oneOf (binderA &lt;|&gt; binderB)</c>.
    /// </remarks>
    let oneOf binders =
        binders |> Binder.mapFailure Errors.OneOf

    /// <summary>
    /// Takes a <see cref="Binder" /> that can fail with <see cref="AltErrors" /> and maps it to <see cref="ValueError.Many" />.
    /// </summary>
    /// <remarks>
    /// To create the <paramref name="binders" /> use the <c>&lt;|&gt;</c> operator. E.g. <c>oneOf (binderA &lt;|&gt; binderB)</c>.
    /// </remarks>
    let oneValueOf binders =
        binders
        |> oneOf
        |> Binder.mapFailure ValueError.Many

    /// <summary>Creates a <see cref="Binder" /> from a System.Type.TryParse style parsing function.</summary>
    /// <remarks>
    /// Useful for creating a parser for a primitive type for which a <c>TryParse</c> function already exists.
    /// </remarks>
    /// <param name="parser">The parsing function with which to create the <see cref="Binder" />.</param>
    /// <returns>A <see cref="Binder" />.</returns>
    let tryParseable (parser: string -> bool * 'parsed) : Binder<string, 'parsed, _> =
        Binder (fun value ->
            match parser value with
            | true, x -> Success x
            | false, _ ->
                typeof<'parsed>.Name
                |> ValueError.InvalidType
                |> Failure)

    /// <summary>A <see cref="Binder" /> for <see cref="System.Boolean" /> values.</summary>
    let bool = tryParseable Boolean.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.Char" /> values.</summary>
    let char = tryParseable Char.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.DateTime" /> values.</summary>
    let dateTime = tryParseable DateTime.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.Double" /> values.</summary>
    let float = tryParseable Double.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.Int16" /> values.</summary>
    let int16 = tryParseable Int16.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.Int32" /> values.</summary>
    let int = tryParseable Int32.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.Int64" /> values.</summary>
    let int64 = tryParseable Int64.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="string" /> values.</summary>
    /// <remarks>Always succeeds.</remarks>
    let string: Binder<string, string, 'e> = Binder.ask

    /// <summary>A <see cref="Binder" /> for <see cref="System.UInt16" /> values.</summary>
    let uint16 = tryParseable UInt16.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.UInt32" /> values.</summary>
    let uint = tryParseable UInt32.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.UInt64" /> values.</summary>
    let uint64 = tryParseable UInt64.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.Uri" /> values of the specified <see cref="System.UriKind" />.</summary>
    let uri kind =
        tryParseable (fun s -> Uri.TryCreate(s, kind))
