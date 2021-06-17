module Symbolica.Extensions.Configuration.FSharp

open Microsoft.Extensions.Configuration

type BindResult<'a> =
    | Success of 'a
    | Failure of string list

module BindResult =

    let apply f a : BindResult<'b> =
        match f, a with
        | Failure e1, Failure e2 -> Failure(List.append e1 e2)
        | Failure e1, Success _ -> e1 |> Failure
        | Success _, Failure e2 -> e2 |> Failure
        | Success f, Success a -> a |> f |> Success

    let bind f : BindResult<'a> -> BindResult<'b> =
        function
        | Success x -> x |> f
        | Failure e -> e |> Failure

    let defaultWith f =
        function
        | Success x -> x
        | Failure e -> e |> f

    let map f : BindResult<'a> -> BindResult<'b> =
        function
        | Success x -> x |> f |> Success
        | Failure e -> e |> Failure

    let zip x y : BindResult<'a * 'b> =
        match x, y with
        | Failure e1, Failure e2 -> Failure(List.append e1 e2)
        | Failure e1, Success _ -> e1 |> Failure
        | Success _, Failure e2 -> e2 |> Failure
        | Success a, Success b -> Success(a, b)

type Binder<'a> = Binder of (IConfiguration -> BindResult<'a>)

module Binder =
    let ofBind b = Binder(fun _ -> b)

    let run (Binder b) = b

    let eval config (b: Binder<'a>) = (run b) config

    let apply (f: Binder<'a -> 'b>) (a: Binder<'a>) : Binder<'b> =
        Binder(fun config -> BindResult.apply (f |> eval config) (a |> eval config))

    let bind (f: 'a -> Binder<'b>) (m: Binder<'a>) : Binder<'b> =
        Binder(fun config -> m |> eval config |> BindResult.bind (f >> eval config))

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

let private path (config: IConfiguration) =
    match config with
    | :? IConfigurationRoot -> "$"
    | :? IConfigurationSection as section -> section.Path
    | _ -> "?"

type Decoder<'a> = string -> Binder<'a>

module Decode =
    let map f (m: Decoder<'a>) : Decoder<'b> = m >> Binder.map f

    let ofParser (parser: string -> bool * 'parsed) value =
        Binder
            (fun section ->
                match parser value with
                | true, x -> Success x
                | false, _ ->
                    Failure [ $"Could not decode '{value}' at path '{section |> path}' as type '{typeof<'parsed>}'." ])

    let bool = ofParser System.Boolean.TryParse
    let char = ofParser System.Char.TryParse
    let dateTime = ofParser System.DateTime.TryParse
    let float = ofParser System.Double.TryParse
    let int16 = ofParser System.Int16.TryParse
    let int = ofParser System.Int32.TryParse
    let int64 = ofParser System.Int64.TryParse
    let uint16 = ofParser System.UInt16.TryParse
    let uint = ofParser System.UInt32.TryParse
    let uint64 = ofParser System.UInt64.TryParse

module Section =
    let value =
        Binder
            (fun section ->
                if section.GetChildren() |> Seq.isEmpty then
                    match section with
                    | :? IConfigurationSection as s -> s.Value |> Success
                    | _ -> failwith $"Expected to bind from an IConfigurationSection, but was binding from an {section.GetType()}"
                else
                    [ $"Expected a simple value at '{section |> path}' but found an object." ]
                    |> Failure)

    let valueOf (decoder: Decoder<'a>) = value |> Binder.bind decoder

let getSection key =
    Binder
        (fun parent ->
            let section = parent.GetSection(key)

            if section.Exists() then
                Success section
            else
                [ $"The key '{key}' does not exist at '{parent |> path}'." ]
                |> Failure)

let getOptSection key =
    Binder
        (fun parent ->
            let section = parent.GetSection(key)

            Success(
                if section.Exists() then
                    Some section
                else
                    None
            ))

let value = getSection >> Binder.nest Section.value

let valueOf decoder = value >> Binder.bind decoder

let optValue =
    getOptSection >> Binder.nestOpt Section.value

let optValueOf (decoder: Decoder<_>) =
    optValue
    >> Binder.bind
        (function
        | Some s -> s |> decoder |> Binder.map Some
        | None -> None |> Success |> Binder.ofBind)

type SectionBuilder(key) =

    let nest (b: Binder<'a>) = key |> getSection |> Binder.nest b

    member _.Bind(x: Binder<'a>, f) = x |> Binder.bind f
    member _.BindReturn(x: Binder<'a>, f) = x |> Binder.map f |> nest

    member _.MergeSources(x1, x2) = Binder.zip x1 x2

    member _.Return(x: 'a) : Binder<'a> = Binder(fun _ -> x |> Success) |> nest

    member _.ReturnFrom(x: Binder<'a>) = x |> nest

type OptSectionBuilder(key) =
    let nest (b: Binder<'a>) : Binder<'a option> =
        key |> getOptSection |> Binder.nestOpt b

    member _.Bind(x: Binder<'a>, f) = x |> Binder.bind f
    member _.BindReturn(x: Binder<'a>, f) = x |> Binder.map f |> nest

    member _.MergeSources(x1, x2) = Binder.zip x1 x2

    member _.Return(x: 'a) : Binder<'a option> = Binder(fun _ -> x |> Success) |> nest

    member _.ReturnFrom(x: Binder<'a>) = x |> nest

let section key = SectionBuilder(key)

let optSection key = OptSectionBuilder(key)
