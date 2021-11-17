namespace Symbolica.Extensions.Configuration.FSharp

module private String =
    let indent by string =
        $"""{" " |> String.replicate (2 * by)}{string}"""

/// A container for errors that are accumulated in an applicative computation.
/// There is always at least one error.
type ApplicativeErrors<'a> =
    { First: 'a
      Rest: 'a list }

    /// <summary>A binary append operation for when both errors must be fixed.</summary>
    /// <param name="e1">The <see cref="ApplicativeErrors" /> accumulated so far on the left hand side.</param>
    /// <param name="e2">The other error(s) on the right hand side. Must be convertible to <see cref="ApplicativeErrors" />.</param>
    static member (+&)(e1: ApplicativeErrors<_>, e2) = e1 |> ApSemiGroup.append e2

    /// <summary>Converts the <see cref="ApplicativeErrors" /> to a <see cref="list" />.</summary>
    member x.ToList() = x.First :: x.Rest

    interface IApSemiGroup<ApplicativeErrors<'a>> with
        member x.Append y =
            { x with Rest = y.ToList() |> List.append x.Rest }

    interface IToApSemiGroup<ApplicativeErrors<'a>> with
        member x.ToSemiGroup() = x

module ApplicativeErrors =
    /// <summary>Maps the errors contained within the <see cref="ApplicativeErrors" />.</summary>
    /// <param name="f">The function used to map the errors.</param>
    /// <param name="a">The <see cref="ApplicativeErrors" /> to be mapped.</param>
    /// <returns><see cref="ApplicativeErrors" /> whose elements are mapped by the function <paramref name="f" />.</returns>
    let map f (a: ApplicativeErrors<_>) =
        { First = a.First |> f
          Rest = a.Rest |> List.map f }

    /// <summary>Creates an <see cref="ApplicativeErrors" /> from a single item.</summary>
    /// <param name="e">The error item.</param>
    let single e = { First = e; Rest = [] }

    /// <summary>Converts the <see cref="ApplicativeErrors" /> to a <see cref="list" />.</summary>
    let toList (e: ApplicativeErrors<_>) = e.ToList()

/// A container for errors that are accumulated in an alt computation.
/// There are always at least two errors.
type AltErrors<'a> =
    { First: 'a
      Second: 'a
      Rest: 'a list }

    /// <summary>A binary append operation for when only one of the errors must be fixed.</summary>
    /// <param name="e1">The <see cref="AltErrors" /> accumulated so far on the left hand side.</param>
    /// <param name="e2">
    /// The other error(s) on the right hand side. Must be an <see cref="AltErrors" /> or a single item of <typeparamref name="'a" />.
    /// </param>
    static member (+|)(e1: AltErrors<'a>, e2) = e1 |> AltSemiGroup.append e2

    /// <summary>Converts the <see cref="AltErrors" /> to a <see cref="list" />.</summary>
    member x.ToList() = x.First :: x.Second :: x.Rest

    interface IAltSemiGroup<'a, AltErrors<'a>> with
        member x.Append y =
            match y with
            | AltSemiGroupItem.SemiGroup errors -> { x with Rest = List.append x.Rest (errors.ToList()) }
            | AltSemiGroupItem.Item a -> { x with Rest = List.append x.Rest [ a ] }

        member x.ToItem() = AltSemiGroupItem.SemiGroup x

module AltErrors =
    /// <summary>Maps the errors contained within the <see cref="AltErrors" />.</summary>
    /// <param name="f">The function used to map the errors.</param>
    /// <param name="a">The <see cref="AltErrors" /> to be mapped.</param>
    /// <returns><see cref="AltErrors" /> whose elements are mapped by the function <paramref name="f" />.</returns>
    let map f (a: AltErrors<_>) =
        { First = a.First |> f
          Second = a.Second |> f
          Rest = a.Rest |> List.map f }

    /// <summary>Creates an <see cref="AltErrors" /> from a pair of individual error items.</summary>
    /// <param name="e1">The first error.</param>
    /// <param name="e2">The second error.</param>
    let pair (e1, e2) = { First = e1; Second = e2; Rest = [] }

    /// <summary>Adds an error to the front of an existing <see cref="AltErrors" />.</summary>
    /// <param name="e">The error to be added to the front.</param>
    /// <param name="errors">The rest of the <see cref="AltErrors" />.</param>
    let cons e errors =
        { First = e
          Second = errors.First
          Rest = errors.Second :: errors.Rest }

    /// <summary>Converts the <see cref="AltErrors" /> to a <see cref="list" />.</summary>
    let toList (e: AltErrors<_>) = e.ToList()

/// <summary>
/// A type that can hold both <see cref="ApplicativeErrors" /> and <see cref="AltErrors" />.
/// The <c>AllOf</c> case indicates that all of the errors must be fixed.
/// The <c>OneOf</c> case indicates that one of the errors must be fixed.
/// </summary>
[<RequireQualifiedAccess>]
type Errors<'a> =
    | AllOf of ApplicativeErrors<'a>
    | OneOf of AltErrors<'a>

    member x.ToString(indent, printItem) =
        let key, errors =
            match x with
            | AllOf errors -> "all of these", errors |> ApplicativeErrors.toList
            | OneOf errors -> "one of these", errors |> AltErrors.toList

        let printedErrors =
            errors
            |> List.map (printItem (indent + 1))
            |> String.concat "\n"

        $"{key |> String.indent indent}:\n{printedErrors}"

module Errors =
    /// <summary>Maps the errors contained within the <see cref="Errors" />.</summary>
    /// <param name="f">The function used to map the errors.</param>
    /// <returns><see cref="Errors" /> whose elements are mapped by the function <paramref name="f" />.</returns>
    let map (f: 'a -> 'b) : Errors<'a> -> Errors<'b> =
        function
        | Errors.AllOf e -> e |> ApplicativeErrors.map f |> Errors.AllOf
        | Errors.OneOf e -> e |> AltErrors.map f |> Errors.OneOf

    /// <summary>Creates an <see cref="Errors" /> from a single item.</summary>
    /// <param name="e">The error item.</param>
    let single e =
        e |> ApplicativeErrors.single |> Errors.AllOf

/// <summary>Errors that can occur when binding to a value, i.e. converting a string to a type.</summary>
/// <remarks><c>Message</c> can be used for custom or miscellaneous errors.</remarks>
[<RequireQualifiedAccess>]
type ValueError =
    | InvalidType of targetType: string
    | Message of string
    | Many of Errors<ValueError>

    /// <summary>A binary append operation for when both errors must be fixed.</summary>
    /// <param name="e1">The first <see cref="ValueError" /> on the left hand side.</param>
    /// <param name="e2">The other error(s) on the right hand side. Must be convertible to <see cref="ApplicativeErrors" />.</param>
    static member (+&)(e1: ValueError, e2) = e1 |> ApSemiGroup.append e2

    /// <summary>A binary append operation for when only one of the errors must be fixed.</summary>
    /// <param name="e1">The first <see cref="ValueError" /> on the left hand side.</param>
    /// <param name="e2">The other error(s) on the right hand side. Must be an <see cref="AltErrors" /> or an <see cref="Error" />.</param>
    static member (+|)(e1: ValueError, e2) = e1 |> AltSemiGroup.append e2

    member x.ToString(indent) =
        match x with
        | InvalidType targetType ->
            $"Could not parse value as type '{targetType}'."
            |> String.indent indent
        | Message message -> message |> String.indent indent
        | Many errors -> errors.ToString(indent, (fun i x -> x.ToString(i)))

    override x.ToString() = x.ToString(0)

    interface IToApSemiGroup<ApplicativeErrors<ValueError>> with
        member x.ToSemiGroup() = x |> ApplicativeErrors.single

    interface IAltSemiGroup<ValueError, AltErrors<ValueError>> with
        member x.Append y =
            match y with
            | AltSemiGroupItem.SemiGroup errors -> errors |> AltErrors.cons x
            | AltSemiGroupItem.Item error -> AltErrors.pair (x, error)

        member x.ToItem() = AltSemiGroupItem.Item x

module ValueError =
    /// <summary>Shorthand for <c>ValueError.InvalidType typeof&lt;'a&gt;.Name</c>.</summary>
    /// <typeparam name="'a">The type for which the conversion was attempted, but failed.</typeparam>
    let invalidType<'a> = ValueError.InvalidType typeof<'a>.Name

/// <summary>Errors that can occur when binding to a configuration section.</summary>
/// <remarks>Anything that has a key is a section and a section may either have child sections or a string value.</remarks>
[<RequireQualifiedAccess>]
type Error =
    | SectionError of key: string * Errors<Error>
    | ValueError of value: string * ValueError
    | NotAValueNode
    | KeyNotFound

    /// <summary>A binary append operation for when both errors must be fixed.</summary>
    /// <param name="e1">The first <see cref="Error" /> on the left hand side.</param>
    /// <param name="e2">The other error(s) on the right hand side. Must be convertible to <see cref="ApplicativeErrors" />.</param>
    static member (+&)(e1: Error, e2) = e1 |> ApSemiGroup.append e2

    /// <summary>A binary append operation for when only one of the errors must be fixed.</summary>
    /// <param name="e1">The first <see cref="Error" /> on the left hand side.</param>
    /// <param name="e2">The other error(s) on the right hand side. Must be an <see cref="AltErrors" /> or an <see cref="Error" />.</param>
    static member (+|)(e1: Error, e2) = e1 |> AltSemiGroup.append e2

    member x.ToString(indent) =
        (match x with
         | SectionError (key, errors) -> [ $"@'{key}'\n{errors.ToString(indent + 1, (fun i x -> x.ToString(i)))}" ]
         | ValueError (value, error) ->
             [ $"Value: {value}"
               $"Error:\n{error.ToString(indent + 1)}" ]
         | NotAValueNode -> [ "Expected a value, but found a section with children." ]
         | KeyNotFound -> [ "The key was not found." ])
        |> Seq.map (String.indent indent)
        |> String.concat "\n"

    override x.ToString() = x.ToString(0)

    interface IToApSemiGroup<ApplicativeErrors<Error>> with
        member x.ToSemiGroup() = x |> ApplicativeErrors.single

    interface IAltSemiGroup<Error, AltErrors<Error>> with
        member x.Append y =
            match y with
            | AltSemiGroupItem.SemiGroup errors -> errors |> AltErrors.cons x
            | AltSemiGroupItem.Item error -> AltErrors.pair (x, error)

        member x.ToItem() = AltSemiGroupItem.Item x

module Error =
    /// <summary>Shorthand for creating a <c>Error.KeyNotFound</c> for a given key.</summary>
    /// <paramref name="key">The key that was not found.</paramref>
    let keyNotFound key =
        Error.SectionError(key, Errors.single Error.KeyNotFound)

    /// <summary>Shorthand for creating a <c>Error.NotAValueNode</c> for a given key.</summary>
    /// <paramref name="key">The key that was expected to contain a simple string value.</paramref>
    let notAValueNode key =
        Error.SectionError(key, Errors.single Error.NotAValueNode)

    /// <summary>Shorthand for creating a <c>Error.ValueError</c> for a given key and value.</summary>
    /// <paramref name="key">The key for which there was an error.</paramref>
    /// <paramref name="value">The value for which there was an error.</paramref>
    /// <paramref name="error">The error.</paramref>
    let valueError key value error =
        Error.SectionError(key, Error.ValueError(value, error) |> Errors.single)

    /// <summary>Shorthand for <c>valueError key value ValueError.invalidType&lt;'a&gt;</c>.</summary>
    /// <paramref name="key">The key for which the type conversion was invalid.</paramref>
    /// <paramref name="value">The value for which the type conversion was invalid.</paramref>
    /// <typeparam name="'a">The type for which the conversion was attempted, but failed.</typeparam>
    let invalidType<'a> key value =
        valueError key value ValueError.invalidType<'a>
