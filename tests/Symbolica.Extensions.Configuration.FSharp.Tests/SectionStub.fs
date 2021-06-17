namespace Symbolica.Extensions.Configuration.FSharp

open Microsoft.Extensions.Configuration

type SectionStub =
    { Children: IConfigurationSection seq
      Path: string
      Value: string }
    static member Empty path =
        { Children = Seq.empty
          Path = path
          Value = null }

    interface IConfigurationSection with
        member x.GetChildren() = x.Children
        member _.GetReloadToken() = failwith "Not Implemented"

        member x.GetSection(key) =
            x.Children
            |> Seq.tryFind (fun s -> s.Key = key)
            |> Option.defaultValue (
                ConfigurationPath.Combine(x.Path, key)
                |> SectionStub.Empty
                :> IConfigurationSection
            )

        member _.Item
            with get _ = failwith "Not Implemented"
            and set _ __ = failwith "Not Implemented"

        member x.Key = ConfigurationPath.GetSectionKey(x.Path)
        member x.Path = x.Path
        member x.Value = x.Value

        member _.Value
            with set _ = failwith "Not Implemented"
