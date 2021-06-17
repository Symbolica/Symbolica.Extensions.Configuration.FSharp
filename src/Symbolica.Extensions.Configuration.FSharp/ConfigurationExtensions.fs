[<AutoOpen>]
module private Symbolica.Extensions.Configuration.FSharp.Extensions

open Microsoft.Extensions.Configuration

let path (config: IConfiguration) =
    match config with
    | :? IConfigurationRoot -> "$"
    | :? IConfigurationSection as section -> section.Path
    | _ -> "?"
