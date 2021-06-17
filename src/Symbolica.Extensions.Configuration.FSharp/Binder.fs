namespace Symbolica.Extensions.Configuration.FSharp

open Microsoft.Extensions.Configuration

type Binder<'a> = Binder of (IConfiguration -> BindResult<'a>)

module Binder =
    let ofBind b = Binder(fun _ -> b)

    let run (Binder b) = b

    let eval config (b: Binder<'a>) = (run b) config

    let apply (f: Binder<'a -> 'b>) (a: Binder<'a>) : Binder<'b> =
        Binder(fun config -> BindResult.apply (f |> eval config) (a |> eval config))

    let bind (f: 'a -> Binder<'b>) (m: Binder<'a>) : Binder<'b> =
        Binder
            (fun config ->
                m
                |> eval config
                |> BindResult.bind (f >> eval config))

    let contramap f (m: Binder<'a>) = Binder(f >> run m)

    let nest (b: Binder<'a>) parentBinder =
        Binder
            (fun parent ->
                parentBinder
                |> eval parent
                |> BindResult.bind (fun subSection -> b |> eval subSection))

    let nestOpt (b: Binder<'a>) (sectionBinder: Binder<IConfigurationSection option>) =
        Binder
            (fun parent ->
                sectionBinder
                |> eval parent
                |> BindResult.bind
                    (function
                    | Some subSection -> b |> eval subSection |> BindResult.map Some
                    | None -> None |> Success))

    let map f (m: Binder<'a>) : Binder<'b> =
        Binder(fun config -> m |> eval config |> BindResult.map f)

    let result x = x |> Success |> ofBind

    let zip x y : Binder<'a * 'b> =
        Binder(fun config -> BindResult.zip (x |> eval config) (y |> eval config))

    let section key =
        Binder
            (fun parent ->
                let section = parent.GetSection(key)

                if section.Exists() then
                    Success section
                else
                    [ $"The key '{key}' does not exist at '{parent |> path}'." ]
                    |> Failure)

    let optSection key =
        Binder
            (fun parent ->
                let section = parent.GetSection(key)

                Success(
                    if section.Exists() then
                        Some section
                    else
                        None
                ))

    module Section =
        let value =
            Binder
                (fun section ->
                    if section.GetChildren() |> Seq.isEmpty then
                        match section with
                        | :? IConfigurationSection as s -> s.Value |> Success
                        | _ ->
                            failwith
                                $"Expected to bind from an IConfigurationSection, but was binding from an {section.GetType()}"
                    else
                        [ $"Expected a simple value at '{section |> path}' but found an object." ]
                        |> Failure)

        let valueOf decoder = value |> bind decoder

[<AutoOpen>]
module BinderExtensions =
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
