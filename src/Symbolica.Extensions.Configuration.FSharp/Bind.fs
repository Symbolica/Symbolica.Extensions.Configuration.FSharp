namespace Symbolica.Extensions.Configuration.FSharp

open System
open Microsoft.Extensions.Configuration

type Bind() =
    member _.Bind(x: Binder<_, _>, f) = x |> Binder.bind f
    member _.BindReturn(x: Binder<_, _>, f) = x |> Binder.map f
    member _.MergeSources(x1, x2) = Binder.zip x1 x2
    member _.Return(x: 'a) : Binder<_, 'a> = x |> Binder.result
    member _.ReturnFrom(x: Binder<_, _>) = x

[<AutoOpen>]
module Builder =
    /// <summary>A computation expression that is used to create a binder which binds the data of config section to a user defined type.</summary>
    /// <returns>An instance of the builder.</returns>
    let bind = Bind()

/// Contains combinator functions for building new binders.
module Bind =
    /// <summary>A combinator for taking an existing binder and nesting it under a parent section at the given key.</summary>
    /// <param name="key">The key of the child section to which the <paramref name="sectionBinder" /> should be bound.</param>
    /// <param name="sectionBinder">The binder that binds the child section to some type.</param>
    /// <returns>A binder for the section at the <paramref name="key" />.</returns>
    let section key sectionBinder =
        Binder (fun (parent: #IConfiguration) ->
            let section = parent.GetSection(key)

            if section.Exists() then
                Success section
            else
                [ $"The key '{key}' does not exist at '{parent |> path}'." ]
                |> Failure)
        |> Binder.nest sectionBinder

    /// <summary>A combinator for taking an existing binder and nesting it under an optional parent section at the given key.</summary>
    /// <remarks>If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.</remarks>
    /// <param name="key">The key of the child section to which the <paramref name="sectionBinder" /> should be bound.</param>
    /// <param name="sectionBinder">The binder that binds the child section to some type.</param>
    /// <returns>A binder for the section at the <paramref name="key" />.</returns>
    let optSection key sectionBinder =
        Binder (fun (parent: #IConfiguration) ->
            let section = parent.GetSection(key)

            Success(
                if section.Exists() then
                    Some section
                else
                    None
            ))
        |> Binder.nestOpt sectionBinder

    let private decode decoder : Binder<_, _> =
        Binder(fun value -> decoder |> Binder.eval value)

    let private readValue =
        Binder (fun (section: #IConfigurationSection) ->
            if section.GetChildren() |> Seq.isEmpty then
                section.Value |> Success
            else
                [ $"Expected a simple value at '{section |> path}' but found an object." ]
                |> Failure)

    /// <summary>Binds the value at the <paramref name="key" /> with the <paramref name="decoder" />.</summary>
    /// <param name="key">The key whose value should be bound.</param>
    /// <param name="decoder">The binder to use when converting the string value.</param>
    /// <returns>A binder for the value at the <paramref name="key" />.</returns>
    let value key decoder : Binder<'config, 'a> =
        section key (readValue |> Binder.nest (decode decoder))

    /// <summary>Binds the optional value at the <paramref name="key" /> with the <paramref name="decoder" />.</summary>
    /// <remarks>If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.</remarks>
    /// <param name="key">The key whose value should be bound.</param>
    /// <param name="decoder">The binder to use when converting the string value.</param>
    /// <returns>A binder for the optional value at the <paramref name="key" />.</returns>
    let optValue key decoder : Binder<'config, 'a option> =
        optSection key (readValue |> Binder.nest (decode decoder))

    /// <summary>Creates a <see cref="Binder" /> from a System.Type.TryParse style parsing function.</summary>
    /// <remarks>
    /// Useful for creating a parser for a primitive type for which a <c>TryParse</c> function already exists.
    /// </remarks>
    /// <param name="parser">The parsing function with which to create the <see cref="Binder" />.</param>
    /// <returns>A <see cref="Binder" />.</returns>
    let tryParseable (parser: string -> bool * 'parsed) : Binder<string, 'parsed> =
        Binder (fun value ->
            match parser value with
            | true, x -> Success x
            | false, _ -> Failure [ $"Could not decode '{value}' as type '{typeof<'parsed>}'." ])

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
    let string: Binder<string, string> = Binder.ask

    /// <summary>A <see cref="Binder" /> for <see cref="System.UInt16" /> values.</summary>
    let uint16 = tryParseable UInt16.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.UInt32" /> values.</summary>
    let uint = tryParseable UInt32.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.UInt64" /> values.</summary>
    let uint64 = tryParseable UInt64.TryParse

    /// <summary>A <see cref="Binder" /> for <see cref="System.Uri" /> values of the specified <see cref="System.UriKind" />.</summary>
    let uri kind =
        tryParseable (fun s -> Uri.TryCreate(s, kind))
