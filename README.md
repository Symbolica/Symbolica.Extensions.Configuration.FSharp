# Symbolica.Extensions.Configuration.FSharp


Provides a safe API for binding an F# type from the dotnet [`IConfiguration`](https://docs.microsoft.com/en-us/dotnet/api/microsoft.extensions.configuration.iconfiguration?view=dotnet-plat-ext-5.0) interface. It is an F#-friendly alternative to using the reflection-based [`ConfigurationBinder.Bind`](https://docs.microsoft.com/en-us/dotnet/api/microsoft.extensions.configuration.configurationbinder.bind?view=dotnet-plat-ext-5.0).

## Motivation

Out-of-the-box dotnet provides what it calls the ["the Options pattern"](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration/options?view=aspnetcore-5.0) which it describes as:

> The options pattern uses classes to provide strongly typed access to groups of related settings.

Whilst this might be "strongly typed" in the sense that you're interacting with statically typed options objects, the binding mechanism is not strictly safe and so the static types are often a lie. This leads to a few notable problems, especially when working with it from F#.

1. It's a large source of `NullReferenceException`s because the binder will hapily set a value to `null` if it's missing in the underlying config. This means your F# type is probably lying to you about the fact its value cannot be null. F# developers would rather model optional config with an `Option`, but the binder doesn't support this.
1. It uses a reflection based API which means that if you want to use F# records to model your options they have to be annotated with `[<CLIMutable>]`.
1. It can struggle to bind more exotic types beyond the primitives from the CLR and the most common collection types.

This library provides an alternative approach to options binding for F# developers in the form of declarative computation expressions. Rather than relying on reflection magic it gives you control over the whole options binding process in a composable and type safe way. It provides first class support for `Option` values. It models the outcome of the bind operation with a `BindResult` which can be either `Success` or `Failure` and it reports as many issues as it can when a `Failure` occurs so you can fix them all in one go.

## Build Status

[![Build history](https://buildstats.info/github/chart/SymbolicaDev/Symbolica.Extensions.Configuration.FSharp?branch=master)](https://github.com/SymbolicaDev/Symbolica.Extensions.Configuration.FSharp/actions)

## Installation

[![NuGet Badge](https://buildstats.info/nuget/Symbolica.Extensions.Configuration.FSharp)](https://www.nuget.org/packages/Symbolica.Extensions.Configuration.FSharp/)

Using the `dotnet` cli

```sh
dotnet add package Symbolica.Extensions.Configuration.FSharp
```

or with the NuGet package manager

```sh
PM> Install-Package Symbolica.Extensions.Configuration.FSharp
```

## Usage

The primary means of using this library is through a computation expression called `bind` and a handful of combinator functions which make it easy to combine binders for subsections into larger types.
These provide a declarative DSL for safely binding a type from an `IConfiguration` instance.
It's probably best to start with an example, but you can find the full api [here](src/Symbolica.Extensions.Configuration.FSharp/Bind.fs).

### Example

Imagine you have the following records defined to model some options

```fsharp
type LoggingSink =
    { Level: string option
      MaxSize: int option
      Type: string }

type LoggingOptions =
    { DefaultLevel: string
      Sinks: LoggingSink list }
```

We can bind this from an `IConfiguration` like this

```fsharp
let bindOptions config =

    // We can independently define binders for the child sections because the api is composable
    let bindSink =
        bind {
            let! level = Bind.optValueAt "Level" Bind.string
            and! maxSize = Bind.optValueAt "MaxSize" Bind.int
            and! typ = Bind.valueAt "Type" Bind.string

            return
                { Level = level
                  MaxSize = maxSize
                  Type = typ }
        }

    Bind.section
        "Logging"
        (bind {
            let! defaultLevel = Bind.valueAt "DefaultLevel" Bind.string

            // We can use the `section` and `list` combinators with `bindSink` to bind it from a section called "Sinks"
            and! sinks = Bind.section "Sinks" (Bind.list bindSink)

            return
                { DefaultLevel = defaultLevel
                  Sinks = sinks }
         })
    |> Binder.eval config
```

Let's also pretend we're using the JSON configuration provider. We'll consider a few different example configs and see what `bindOptions` will return us.

### Correct config

In this scenario all required fields are populated and correct in our `appsettings.json` file.

```json
{
    "Logging": {
        "DefaultLevel": "Warning",
        "Sinks": [
            {
                "Level": "Info",
                "Type": "Console"
            },
            {
                "MaxSize": 1024,
                "Type": "File"
            }
        ]
    }
}
```

This would bind fine and return the following value

```fsharp
Success
    { DefaultLevel = "Warning"
      Sinks = [
          { Level = Some "Info"
            MaxSize = None
            Type = "Console" }
          { Level = None
            MaxSize = Some 1024
            Type = "File" }
      ] }

```

### Incorrect config

In this scenario some required values are missing and some others are the wrong type.

```json
{
    "Logging": {
        "Sinks": [
            {
                "Level": "Debug"
            },
            {
                "MaxSize": "NotAnInt",
                "Type": "File"
            }
        ]
    }
}
```

This would result in a `Failure` like so, assuming we call `BindResult.mapFailure (fun e -> e.ToString())` to pretty print the error.

```fsharp
Failure
    """
@'Logging':
  all of these:
    @'DefaultLevel':
      The key was not found.
    @'Sinks':
      all of these:
        @'0':
          all of these:
            all of these:
              @'Type':
                The key was not found.
        @'1':
          all of these:
            all of these:
              @'MaxSize':
                Value: 'NotAnInt'
                Error:
                  Could not parse value as type 'Int32'."""

```

Notice how in this case it returns as many errors as it can.

If you want to see a more sophisticated example then check out the [IntegrationTests](tests/Symbolica.Extensions.Configuration.FSharp.Tests/IntegrationTests.fs)

## Creating New Binders

At the heart of this library is the `Binder<'config, 'value', 'error>` type.
It is nothing more than a simple wrapper around a function that takes some config as input it and returns a `BindResult`.
In order to bind your custom type from configuration you need to create an instance of a `Binder<IConfiguration, YourOptionsType, Error>`.

You can create a binder by just writing `Binder(fun config -> // parse config and return a BindResult)`, however, in practice you don't usually create `Binder` instances directly.
Instead you use the `bind` computation expression and the combinators in the `Bind` module to create a new `Binder` from a collection of child `Binder`s, as can be seen in the example above.
One common exception to this rule is when binding a DU, see below.
If there are binders for common types missing from the `Bind` module please feel free to open a PR to add them.

## Binding DUs

There are a few ways to bind a DU depending on how you want to handle errors and the complexity of the types in the cases.

#### Simple DUs

Let's consider a very simple `LogLevel` DU.

```fsharp
type LogLevel =
    | Info
    | Warning
    | Error
```

The most straightforward way to create a `Binder` for this is to just write it directly like so:

```fsharp
let bind =
    Binder(function
        | "Info" -> Success Info
        | "Warning" -> Success Warning
        | "Error" -> Success Error
        | _ -> Failure ValueError.invalidType<LogLevel>)
```

The downside of this approach is that the error reporting is rather generic and it won't tell you what the allowed format is for the case labels.
This can be remedied by using the `<|>` (alternative) operator and the `Bind.oneValueOf` function.
The `<|>` operator first tries the `Binder` on the left hand side and if that fails it then tries the `Binder` on the right hand side, if both fail it returns all the errors.
The `Bind.oneValueOf` function is just there to lift the errors up to the common `Error` type.

```fsharp
let bind =
    let bindCase value case =
        Binder (function
            | s when s = value -> Success case
            | _ -> Failure(ValueError.Message $"Could not parse as '{case}'."))

    Bind.oneValueOf (
        bindCase "Info" Info
        <|> bindCase "Warning" Warning
        <|> bindCase "Error" Error
    )
```

Now if it fails we'll get an error message like this:

> @'DefaultLevel':
>   Value: 'NotALevel'
>   Error:
>     one of these:
>       Could not parse as 'Info'.
>       Could not parse as 'Warning'.
>       Could not parse as 'Error'.

### Complex DUs

Let's imagine a more complex DU this time, one where the cases have additional data associated with them.

```fsharp
type ComplexType = { Foo: bool; Bar: System.DateTime }

type ComplexDu =
    | SimpleCase
    | IntCase of int
    | ComplexCase of ComplexType
```

Fortunately we can use the `<|>` operator along with other combinators from the `Bind` module to tackle this type like so:

```fsharp
let bindSimpleCase =
    Binder (fun value ->
        if value = "SimpleCase" then
            Success SimpleCase
        else
            Failure(ValueError.Message "Could not parse as SimpleCase"))

let bindComplexType =
    bind {
        let! foo = Bind.valueAt "Foo" Bind.bool
        and! bar = Bind.valueAt "Bar" Bind.dateTime
        return { Foo = foo; Bar = bar }
    }

let bindComplexDu =
    Bind.oneOf (
        Bind.value bindSimpleCase
        <|> (Bind.value Bind.int |> Binder.map IntCase)
        <|> (bindComplexType |> Binder.map ComplexCase)
    )
```

## Usage with DI

It's a common practice when configuring options in dotnet applications to do it using the `Configure` extension method of the `IServiceCollection` like this

```fsharp
let configureServices (ctx: WebHostBuilderContext) (services: IServiceCollection) =
    services
        .Configure<Options>(ctx.Configuration.GetSection("Options").Bind)
    |> ignore
```

Because these `Configure` extension methods take an `Action<TOptions>` it requires that the type `TOptions` has a parameterless public constructor that it can invoke via reflection. This goes against the design of this library. At present if you want to use this library with DI then you will need to do something like this instead.

```fsharp
let bindOptions config =
    Bind.section
        "Options"
        (bind {
            // rest of binding code
        })
    |> Binder.eval config
    |> BindResult.getOrFail // Chosen to throw an exception containing all of the errors in the case of failure

let configureServices (ctx: WebHostBuilderContext) (services: IServiceCollection) =
    // Add the options type as a transient service if you want it to be rebound on each request,
    // i.e. to pick up config changes
    services
        .AddTransient<MyOptions>(fun _ -> ctx.Configuration |> bindOptions)
    |> ignore
```

The drawbacks to this are that we don't have support for named options or reactive options monitoring in this form.

If enough people want to use this with DI, because named and monitored options are required, then it would be possible to provide an alternative instance of the `IOptionsFactory` which worked well with this library. Please open an issue if this is something you'd like.
