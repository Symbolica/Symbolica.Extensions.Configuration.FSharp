namespace Symbolica.Extensions.Configuration.FSharp

/// <summary>
/// A function that takes a value and returns a <see cref="Binder"/> which evaluates to <c>Success</c> if the
/// value can be decoded otherwise <c>Failure</c>.
/// </summary>
type Decoder<'a> = string -> Binder<'a>

/// Contains decoders for common types and combinators functions for building new decoders.
module Decode =

    /// <summary>
    /// Lifts the function <paramref name="f" /> up to operate on the contents of the <see cref="Decoder" />.
    /// </summary>
    /// <remarks>Useful for converting a decoder of one type to another.</remarks>
    /// <param name="f">The mapping function to apply to the contents of the <see cref="Decoder" />.</param>
    /// <param name="m">The <see cref="Decoder" />.</param>
    /// <returns>A <see cref="Decoder" /> containing the mapped contents.</returns>
    let map f (m: Decoder<'a>) : Decoder<'b> = m >> Binder.map f

    /// <summary>Creates a <see cref="Decoder" /> from a System.Type.TryParse style parsing function.</summary>
    /// <remarks>
    /// Useful for creating a parser for a privitive type for which a <c>TryParse</c> function already exists.
    /// </remarks>
    /// <param name="parser">The parsing function with which to create the <see cref="Decoder" />.</param>
    /// <param name="value">The value to be decoded.</param>
    /// <returns>A <see cref="Decoder" />.</returns>
    let ofParser (parser: string -> bool * 'parsed) : Decoder<'parsed> =
        fun value ->
            Binder
                (fun section ->
                    match parser value with
                    | true, x -> Success x
                    | false, _ ->
                        Failure [ $"Could not decode '{value}' at path '{section |> path}' as type '{typeof<'parsed>}'." ])

    /// <summary>A <see cref="Decoder" /> for <see cref="System.Boolean" /> values.</summary>
    let bool = ofParser System.Boolean.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.Char" /> values.</summary>
    let char = ofParser System.Char.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.DateTime" /> values.</summary>
    let dateTime = ofParser System.DateTime.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.Double" /> values.</summary>
    let float = ofParser System.Double.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.Int16" /> values.</summary>
    let int16 = ofParser System.Int16.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.Int32" /> values.</summary>
    let int = ofParser System.Int32.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.Int64" /> values.</summary>
    let int64 = ofParser System.Int64.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.UInt16" /> values.</summary>
    let uint16 = ofParser System.UInt16.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.UInt32" /> values.</summary>
    let uint = ofParser System.UInt32.TryParse

    /// <summary>A <see cref="Decoder" /> for <see cref="System.UInt64" /> values.</summary>
    let uint64 = ofParser System.UInt64.TryParse
