module Symbolica.Extensions.Configuration.FSharp.Builders

open FsCheck
open Swensen.Unquote
open global.Xunit

type SubOptions =
    { OptionalNumber: float option
      Bool: bool }

type Options =
    { Name: string
      SubOptions: SubOptions
      OptSubOptions: SubOptions option }

let mkOptions config =
    let bindSubOptions =
        bind {
            let! optionalNumber = Bind.optValueAt "MaybeDecimal" Bind.float
            and! bool = Bind.valueAt "bool" Bind.bool

            return
                { OptionalNumber = optionalNumber
                  Bool = bool }
        }

    Bind.section
        "Options"
        (bind {
            let! name = Bind.valueAt "Name" Bind.string
            and! subOptions = Bind.section "Sub" bindSubOptions
            and! optSubOptions = Bind.optSection "OptSub" bindSubOptions

            return
                { Name = name
                  SubOptions = subOptions
                  OptSubOptions = optSubOptions }
        })
    |> Binder.eval config

[<Fact>]
let ``should bind when configuration correct`` () =
    let config =
        { Children =
            [ { Children =
                  [ { Children = Seq.empty
                      Path = ConfigPathSegment "Name"
                      Value = "A name" }
                    { Children =
                        [ { Children = Seq.empty
                            Path = ConfigPathSegment "MaybeDecimal"
                            Value = "1.0" }
                          { Children = Seq.empty
                            Path = ConfigPathSegment "bool"
                            Value = "true" } ]
                      Path = ConfigPathSegment "Sub"
                      Value = null }
                    { Children =
                        [ { Children = Seq.empty
                            Path = ConfigPathSegment "MaybeDecimal"
                            Value = "2.0" }
                          { Children = Seq.empty
                            Path = ConfigPathSegment "bool"
                            Value = "false" } ]
                      Path = ConfigPathSegment "OptSub"
                      Value = null } ]
                Path = ConfigPathSegment "Options"
                Value = null } ]
          Path = ConfigPathSegment.empty
          Value = null }

    test
        <@ config |> mkOptions = Success(
            { Name = "A name"
              SubOptions =
                { OptionalNumber = Some(1.0)
                  Bool = true }
              OptSubOptions =
                { OptionalNumber = Some(2.0)
                  Bool = false }
                |> Some }
        ) @>

[<Fact>]
let ``should bind successfully when optional config missing`` () =
    let config =
        { Children =
            [ { Children =
                  [ { Children = Seq.empty
                      Path = ConfigPathSegment "Name"
                      Value = "A name" }
                    { Children =
                        [ { Children = Seq.empty
                            Path = ConfigPathSegment "MaybeDecimal"
                            Value = null }
                          { Children = Seq.empty
                            Path = ConfigPathSegment "bool"
                            Value = "true" } ]
                      Path = ConfigPathSegment "Sub"
                      Value = null } ]
                Path = ConfigPathSegment "Options"
                Value = null } ]
          Path = ConfigPathSegment.empty
          Value = null }

    test
        <@ config |> mkOptions = Success(
            { Name = "A name"
              SubOptions = { OptionalNumber = None; Bool = true }
              OptSubOptions = None }
        ) @>

[<Fact>]
let ``should fail when non optional config missing`` () =
    let config =
        { Children =
            [ { Children =
                  [ { Children = Seq.empty
                      Path = ConfigPathSegment "NotTheNameKey"
                      Value = "A name" }
                    { Children = Seq.empty
                      Path = ConfigPathSegment "Sub"
                      Value = null } ]
                Path = ConfigPathSegment "Options"
                Value = null } ]
          Path = ConfigPathSegment.empty
          Value = null }

    let expected =
        Failure(
            Error.SectionError(
                "Options",
                Error.Many(
                    Errors.AllOf(
                        Error.keyNotFound "Name"
                        +& Error.keyNotFound "Sub"
                    )
                )
            )
        )

    test <@ config |> mkOptions = expected @>
