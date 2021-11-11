module Symbolica.Extensions.Configuration.FSharp.Builders

open Swensen.Unquote
open Xunit

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
            let! optionalNumber = Bind.optValue "MaybeDecimal" Bind.float
            and! bool = Bind.value "bool" Bind.bool

            return
                { OptionalNumber = optionalNumber
                  Bool = bool }
        }

    Bind.section
        "Options"
        (bind {
            let! name = Bind.value "Name" Bind.string
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
                      Path = "Name"
                      Value = "A name" }
                    { Children =
                        [ { Children = Seq.empty
                            Path = "MaybeDecimal"
                            Value = "1.0" }
                          { Children = Seq.empty
                            Path = "bool"
                            Value = "true" } ]
                      Path = "Sub"
                      Value = null }
                    { Children =
                        [ { Children = Seq.empty
                            Path = "MaybeDecimal"
                            Value = "2.0" }
                          { Children = Seq.empty
                            Path = "bool"
                            Value = "false" } ]
                      Path = "OptSub"
                      Value = null } ]
                Path = "Options"
                Value = null } ]
          Path = System.String.Empty
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
                      Path = "Name"
                      Value = "A name" }
                    { Children =
                        [ { Children = Seq.empty
                            Path = "MaybeDecimal"
                            Value = null }
                          { Children = Seq.empty
                            Path = "bool"
                            Value = "true" } ]
                      Path = "Sub"
                      Value = null } ]
                Path = "Options"
                Value = null } ]
          Path = System.String.Empty
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
                      Path = "NotTheNameKey"
                      Value = "A name" }
                    { Children = Seq.empty
                      Path = "Sub"
                      Value = null } ]
                Path = "Options"
                Value = null } ]
          Path = System.String.Empty
          Value = null }

    test
        <@ config |> mkOptions = Failure(
            [ "The key 'Name' does not exist at 'Options'."
              "The key 'Sub' does not exist at 'Options'." ]
        ) @>
