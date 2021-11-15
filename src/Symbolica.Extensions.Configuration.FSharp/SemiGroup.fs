namespace Symbolica.Extensions.Configuration.FSharp

type IApSemiGroup<'a> =
    abstract Append: 'a -> 'a

type IToApSemiGroup<'a when 'a :> IApSemiGroup<'a>> =
    abstract ToSemiGroup: unit -> 'a

module ApSemiGroup =
    let from x = (x :> IToApSemiGroup<_>).ToSemiGroup()

    let append y x = (from x).Append(from y)

[<RequireQualifiedAccess>]
type AltSemiGroupItem<'group, 'item> =
    | SemiGroup of 'group
    | Item of 'item

type IAltSemiGroup<'item, 'group> =
    abstract Append: AltSemiGroupItem<'group, 'item> -> 'group
    abstract ToItem: unit -> AltSemiGroupItem<'group, 'item>

module AltSemiGroup =
    let append y x =
        (x :> IAltSemiGroup<_, _>)
            .Append((y :> IAltSemiGroup<_, _>).ToItem())
