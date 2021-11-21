/// <summary>
/// A collection of <see cref="Binder" /> instances that can be used to retrieve parts
/// of the <see cref="IConfiguration" /> object being bound.
/// </summary>
module Symbolica.Extensions.Configuration.FSharp.Config

open Microsoft.Extensions.Configuration

/// <summary>
/// A <see cref="Binder" /> that retrieves the section specified by the <paramref name="key" />.
/// </summary>
/// <remarks>Will evaluate to <c>Failure</c> if the <pararef name="key" /> cannot be found.</remarks>
/// <param name="key">The key of the child section to retrieve.</param>
let section key : Binder<'config, IConfigurationSection, Error> =
    Binder (fun (parent: #IConfiguration) ->
        let section = parent.GetSection(key)

        if section.Exists() then
            Success section
        else
            Error.KeyNotFound |> Failure)

/// <summary>
/// A <see cref="Binder" /> that retrieves the optional section specified by the <paramref name="key" />.
/// </summary>
/// <remarks>Will evaluate to <c>Success(None)</c> if the <pararef name="key" /> cannot be found.</remarks>
/// <param name="key">The key of the child section to retrieve.</param>
let optSection key =
    Binder (fun (parent: #IConfiguration) ->
        let section = parent.GetSection(key)

        Success(
            if section.Exists() then
                Some section
            else
                None
        ))

/// <summary>
/// A <see cref="Binder" /> that retrieves the key from the current <see cref="IConfiguration" /> section.
/// </summary>
let key: Binder<'config, string, _> =
    Binder(fun (s: #IConfigurationSection) -> s.Key |> BindResult.result)

/// <summary>
/// A <see cref="Binder" /> that retrieves the value from the current <see cref="IConfiguration" /> section.
/// </summary>
/// <remarks>Will evaluate to <c>Failuer</c> if this section is not a value and has children.</remarks>
let value =
    Binder (fun (section: #IConfigurationSection) ->
        if section.GetChildren() |> Seq.isEmpty then
            section.Value |> Success
        else
            Error.NotAValueNode |> Failure)

/// <summary>
/// A <see cref="Binder" /> that retrieves the child sections from the current <see cref="IConfiguration" /> section.
/// </summary>
let children<'c, 'e when 'c :> IConfiguration> : Binder<'c, IConfigurationSection seq, 'e> =
    Binder.ask
    |> Binder.map (fun s -> s.GetChildren())
