namespace Symbolica.Extensions.Configuration.FSharp

[<AutoOpen>]
module Api =
    /// <summary>A computation expression that is used to create a binder which binds the data of config section to a user defined type.</summary>
    /// <returns>An instance of the builder.</returns>
    let bind = Binder.Builder()

    /// <summary>A combinator for taking an existing binder and nesting it under a parent section at the given key.</summary>
    /// <param name="key">The key of the child section to which the <paramref name="sectionBinder" /> should be bound.</param>
    /// <param name="sectionBinder">The binder that binds the child section to some type.</param>
    /// <returns>A binder for the section at the <paramref name="key" />.</returns>
    let section key sectionBinder =
        key |> Binder.section |> Binder.nest sectionBinder

    /// <summary>A combinator for taking an existing binder and nesting it under an optional parent section at the given key.</summary>
    /// <remarks>If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.</remarks>
    /// <param name="key">The key of the child section to which the <paramref name="sectionBinder" /> should be bound.</param>
    /// <param name="sectionBinder">The binder that binds the child section to some type.</param>
    /// <returns>A binder for the section at the <paramref name="key" />.</returns>
    let optSection key sectionBinder =
        key
        |> Binder.optSection
        |> Binder.nestOpt sectionBinder

    /// <summary>Binds the value at the <paramref name="key" /> as a <c>string</c>.</summary>
    /// <param name="key">The key whose value should be bound.</param>
    /// <returns>A binder for the value at the <paramref name="key" />.</returns>
    let value key =
        key
        |> Binder.section
        |> Binder.nest Binder.Section.value

    /// <summary>Binds the value at the <paramref name="key" /> using the <paramref name="decoder" />.</summary>
    /// <param name="key">The key whose value should be bound.</param>
    /// <param name="decoder">The decoder to use when converting the value.</param>
    /// <returns>A binder for the value at the <paramref name="key" />.</returns>
    let valueOf decoder key = key |> value |> Binder.bind decoder

    /// <summary>Binds the optional value at the <paramref name="key" /> as a <c>string</c>.</summary>
    /// <remarks>If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.</remarks>
    /// <param name="key">The key whose value should be bound.</param>
    /// <returns>A binder for the optional value at the <paramref name="key" />.</returns>
    let optValue key =
        key
        |> Binder.optSection
        |> Binder.nestOpt Binder.Section.value

    /// <summary>Binds the optional value at the <paramref name="key" /> using the <paramref name="decoder" />.</summary>
    /// <remarks>
    /// If the <paramref name="key" /> does not exist or has an empty value then the binder will evaluate to <c>None</c>.
    /// If the value exists but the decoding fails then the binder will evaluate to <c>Some(Failure(message)))</c>.
    /// </remarks>
    /// <param name="key">The key whose value should be bound.</param>
    /// <param name="decoder">The decoder to use when converting the value.</param>
    /// <returns>A binder for the optional value at the <paramref name="key" />.</returns>
    let optValueOf decoder key =
        key
        |> optValue
        |> Binder.bind (function
            | Some s -> s |> decoder |> Binder.map Some
            | None -> None |> Success |> Binder.ofBindResult)
