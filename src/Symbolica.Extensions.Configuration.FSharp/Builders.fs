namespace Symbolica.Extensions.Configuration.FSharp

type SectionBuilder(key) =

    let nest (b: Binder<'a>) = key |> Binder.section |> Binder.nest b
    member _.Bind(x: Binder<'a>, f) = x |> Binder.bind f
    member _.BindReturn(x: Binder<'a>, f) = x |> Binder.map f |> nest
    member _.MergeSources(x1, x2) = Binder.zip x1 x2
    member _.Return(x: 'a) : Binder<'a> = Binder(fun _ -> x |> Success) |> nest
    member _.ReturnFrom(x: Binder<'a>) = x |> nest

type OptSectionBuilder(key) =
    let nest (b: Binder<'a>) : Binder<'a option> =
        key |> Binder.optSection |> Binder.nestOpt b

    member _.Bind(x: Binder<'a>, f) = x |> Binder.bind f
    member _.BindReturn(x: Binder<'a>, f) = x |> Binder.map f |> nest
    member _.MergeSources(x1, x2) = Binder.zip x1 x2
    member _.Return(x: 'a) : Binder<'a option> = Binder(fun _ -> x |> Success) |> nest
    member _.ReturnFrom(x: Binder<'a>) = x |> nest

[<AutoOpen>]
module Builders =
    let section key = SectionBuilder(key)

    let optSection key = OptSectionBuilder(key)
