module Symbolica.Extensions.Configuration.FSharp.Config

open FsCheck
open FsCheck.Xunit
open Microsoft.Extensions.Configuration
open Swensen.Unquote
open global.Xunit

open Symbolica.Extensions.Configuration.FSharp

module Section =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists should be Success section`` path key =
        let subSection =
            { Children = Seq.empty
              Path = key
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test
            <@ Config.section (key |> ConfigPathSegment.value)
               |> Binder.eval section = Success(subSection :> IConfigurationSection) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Failure`` path key =
        test
            <@ Config.section key
               |> Binder.eval (path |> SectionStub.Empty) = Failure(Error.KeyNotFound) @>

module OptSection =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section exists should be Success Some section`` path key =
        let subSection =
            { Children = Seq.empty
              Path = key
              Value = "Value" }

        let section =
            { Children = seq { subSection }
              Path = path
              Value = null }

        test
            <@ Config.optSection (key |> ConfigPathSegment.value)
               |> Binder.eval section = Success(subSection :> IConfigurationSection |> Some) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section does not exist should be Success None`` path key =
        test
            <@ Config.optSection (key |> ConfigPathSegment.value)
               |> Binder.eval (path |> SectionStub.Empty) = Success(None) @>

module Key =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``should be Success key`` path x =
        let section =
            { Children = Seq.empty
              Path = path
              Value = x }

        test <@ Config.key |> Binder.eval section = Success(path |> ConfigPathSegment.value) @>

module Value =
    [<Property(Arbitrary = [| typeof<Arb.NotNullString>
                              typeof<ConfigurationArb> |])>]
    let ``when value exists and is not null should be Success value`` path x =
        let section =
            { Children = Seq.empty
              Path = path
              Value = x }

        test <@ Config.value |> Binder.eval section = Success(x) @>

    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``when section has children should be Failure`` path key =
        let section =
            { Children =
                [ { Children = Seq.empty
                    Path = key
                    Value = "Value" } ]
              Path = path
              Value = null }

        test <@ Config.value |> Binder.eval section = Failure(Error.NotAValueNode) @>

module Children =
    [<Property(Arbitrary = [| typeof<ConfigurationArb> |])>]
    let ``should be Success children`` childPaths path value =
        let children =
            childPaths
            |> List.map (fun p ->
                { Children = Seq.empty
                  Path = p
                  Value = null }
                :> IConfigurationSection)

        let section =
            { Children = children |> Seq.ofList
              Path = path
              Value = value }

        test
            <@ Config.children
               |> Binder.eval section
               |> BindResult.map List.ofSeq = Success(children) @>
