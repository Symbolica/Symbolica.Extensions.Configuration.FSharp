namespace Symbolica.Extensions.Configuration.FSharp

[<AutoOpen>]
module Api =
    let bind = Binder.Builder()

    let section key sectionBinder =
        key |> Binder.section |> Binder.nest sectionBinder

    let optSection key sectionBinder =
        key
        |> Binder.optSection
        |> Binder.nestOpt sectionBinder

    let value =
        Binder.section >> Binder.nest Binder.Section.value

    let valueOf decoder = value >> Binder.bind decoder

    let optValue =
        Binder.optSection
        >> Binder.nestOpt Binder.Section.value

    let optValueOf decoder =
        optValue
        >> Binder.bind
            (function
            | Some s -> s |> decoder |> Binder.map Some
            | None -> None |> Success |> Binder.ofBind)
