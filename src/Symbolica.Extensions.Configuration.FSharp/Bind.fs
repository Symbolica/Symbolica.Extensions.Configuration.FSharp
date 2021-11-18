namespace Symbolica.Extensions.Configuration.FSharp

open System
open System.Collections.Generic
open Microsoft.Extensions.Configuration

type Bind() =
    member _.Bind(x: Binder<_, _, _>, f) = x |> Binder.bind f
    member _.BindReturn(x: Binder<_, 'a, Error>, f) : Binder<_, 'b, Error> = x |> Binder.map f

    member this.BindReturn(x: Binder<_, 'a, Errors<_>>, f) : Binder<_, 'b, Error> =
        this.BindReturn(x |> Binder.mapFailure Error.Many, f)

    member this.BindReturn(x: Binder<_, 'a, ApplicativeErrors<Error>>, f) : Binder<_, 'b, Error> =
        this.BindReturn(x |> Binder.mapFailure Errors.AllOf, f)

    member _.MergeSources(x1, x2) = Binder.zip x1 x2
    member _.Return x : Binder<_, 'a, Error> = x |> Binder.result
    member _.ReturnFrom(x: Binder<_, _, Error>) : Binder<_, _, Error> = x
    member _.ReturnFrom(x: Binder<_, _, Errors<_>>) : Binder<_, _, Error> = x |> Binder.mapFailure Error.Many

    member this.ReturnFrom(x: Binder<_, _, ApplicativeErrors<Error>>) : Binder<_, _, Error> =
        this.ReturnFrom(x |> Binder.mapFailure Errors.AllOf)

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
    let section key (sectionBinder: Binder<_, 'a, Error>) =
        Config.section key
        |> Binder.extend sectionBinder
        |> Binder.mapFailure (fun e -> Error.SectionError(key, e))

    /// <summary>A combinator for taking an existing binder and nesting it under an optional parent section at the given key.</summary>
    /// <remarks>If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.</remarks>
    /// <param name="key">The key of the child section to which the <paramref name="sectionBinder" /> should be bound.</param>
    /// <param name="sectionBinder">The binder that binds the child section to some type.</param>
    /// <returns>A binder for the section at the <paramref name="key" />.</returns>
    let optSection key (sectionBinder: Binder<_, 'a, Error>) =
        Config.optSection key
        |> Binder.extendOpt sectionBinder
        |> Binder.mapFailure (fun e -> Error.SectionError(key, e))

    let private decode decoder =
        Binder (fun value ->
            decoder
            |> Binder.eval value
            |> BindResult.mapFailure (fun error -> Error.ValueError(value, error)))

    /// <summary>Binds the key of this config section with the <paramref name="decoder" />.</summary>
    /// <param name="decoder">The binder to use when converting the string key.</param>
    /// <returns>A binder for the key at the current config section.</returns>
    let key decoder : Binder<'config, 'a, Error> =
        Config.key |> Binder.extend (decode decoder)

    /// <summary>Binds the value of this config section with the <paramref name="decoder" />.</summary>
    /// <param name="decoder">The binder to use when converting the string value.</param>
    /// <returns>A binder for the value at the current config section.</returns>
    let value decoder : Binder<'config, 'a, Error> =
        Config.value |> Binder.extend (decode decoder)

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
    let allOf binders : Binder<'config, 'a list, Error> =
        binders
        |> Binder.traverseList (Binder.mapFailure ApplicativeErrors.single)
        |> Binder.mapFailure (Errors.AllOf >> Error.Many)

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
    let oneOf binders : Binder<'config, 'a, Error> =
        binders
        |> Binder.mapFailure (Errors.OneOf >> Error.Many)

    /// <summary>
    /// Takes a <see cref="Binder" /> that can fail with <see cref="AltErrors" /> and maps it to <see cref="ValueError.Many" />.
    /// </summary>
    /// <remarks>
    /// To create the <paramref name="binders" /> use the <c>&lt;|&gt;</c> operator. E.g. <c>oneOf (binderA &lt;|&gt; binderB)</c>.
    /// </remarks>
    let oneValueOf binders =
        binders
        |> Binder.mapFailure (Errors.OneOf >> ValueError.Many)

    /// <summary>
    /// Binds an <see cref="IConfigurationSection" /> as a <see cref="KeyValuePair" /> by applying the
    /// <paramref name="keyBinder" /> to the key and the <paramref name="valueBinder" /> to the value.
    /// </summary>
    /// <param name="keyBinder">
    /// The <see cref="Binder" /> to apply to the key of this section in order to convert it to the key type.
    /// </param>
    /// <param name="valueBinder">
    /// The <see cref="Binder" /> to apply to this section in order to convert it to the key type.
    /// Note this can be either a simple value binder or a binder for a more complex type that spans
    /// multiple child sections.
    /// </param>
    let keyValuePair
        (keyBinder: Binder<_, 'key, ValueError>)
        (valueBinder: Binder<IConfigurationSection, 'value, Error>)
        : Binder<_, KeyValuePair<'key, 'value>, Error> =
        bind {
            let! key = key keyBinder
            and! value = Binder.ask |> Binder.extend valueBinder
            return KeyValuePair(key, value)
        }

    /// <summary>
    /// Binds an <see cref="IConfigurationSection" /> as an <see cref="IDictionary" /> by applying the
    /// <paramref name="keyBinder" /> to the key and the <paramref name="valueBinder" /> to the value
    /// of each child section.
    /// </summary>
    /// <param name="keyBinder">
    /// The <see cref="Binder" /> to apply to the key of each child section in order to convert it to the key type.
    /// </param>
    /// <param name="valueBinder">
    /// The <see cref="Binder" /> to apply to each child section in order to convert it to the key type.
    /// Note this can be either a simple value binder or a binder for a more complex type that spans
    /// multiple child sections.
    /// </param>
    let dict
        (keyBinder: Binder<string, 'key, ValueError>)
        (valueBinder: Binder<IConfigurationSection, 'value, Error>)
        : Binder<'config, IDictionary<'key, 'value>, Error> =
        Config.children
        |> Binder.map List.ofSeq
        |> Binder.bind (
            Binder.traverseList (
                Config.key
                |> Binder.bind (fun key ->
                    keyValuePair keyBinder valueBinder
                    |> Binder.mapFailure (fun e -> Error.SectionError(key, e)))
                |> Binder.mapFailure ApplicativeErrors.single
                |> Binder.run
                >> Binder.ofBindResult
            )
        )
        |> Binder.mapFailure (Errors.AllOf >> Error.Many)
        |> Binder.map (fun x -> Dictionary(x) :> IDictionary<'key, 'value>)

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
