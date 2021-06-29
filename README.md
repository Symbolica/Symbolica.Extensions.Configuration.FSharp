# Symbolica.Extensions.Configuration.FSharp


Provides a safe API for binding an FSharp type from the dotnet [`IConfiguration`](https://docs.microsoft.com/en-us/dotnet/api/microsoft.extensions.configuration.iconfiguration?view=dotnet-plat-ext-5.0) interface. It is an FSharp-friendly alternative to using the reflection-based [`ConfigurationBinder.Bind`](https://docs.microsoft.com/en-us/dotnet/api/microsoft.extensions.configuration.configurationbinder.bind?view=dotnet-plat-ext-5.0).

## Motivation

Out-of-the-box dotnet provides what it calls the ["the Options pattern"](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration/options?view=aspnetcore-5.0) which it describes as:

> The options pattern uses classes to provide strongly typed access to groups of related settings.

Whilst this might be strongly typed in the sense that you're interacting with statically typed options objects, the binding mechanism is not strictly safe and so the static types are often a lie. This leads to a few notable problems, especially when working with it from FSharp.

1. It's a large source of `NullReferenceException`s because the binder will hapily set a value to `null` if it's missing in the underlying config. This means your FSharp type is probably lying to you about the fact its value cannot be null. FSharp developers would rather model optional config with an `Option`, but the binder doesn't support this.
1. It uses a reflection based API which means that if you want to use FSharp records to model your options they have to be annotated with `[<CLIMutable>]`.
1. It can struggle to bind more exotic types beyond the primitives from the CLR and the most common collection types.

This library provides an alternative approach to options binding for FSharp developers in the form of declarative computation expressions. Rather than relying on reflection magic it gives you control over the whole options binding process in a composable and type safe way. It provides first class support for `Option` values. It models the outcome of the bind operation with a `BindResult` which can be either `Success` or `Failure` and it reports as many issues as it can when a `Failure` occurs so you can fix them all in one go.

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

The primary means of using this library is through a computation expression called `bind` and a handful of combinator functions which make it easy to combine binders for sub sections into larger types. These provide a declarative DSL for safely binding a type from an `IConfiguration` instance. It's probably best to see an example, but you can find the full api [here](src/Symbolica.Extensions.Configuration.FSharp/Api.fs).

## Example

Imagine you have the following records defined to model some options

```fsharp
type SubOptions =
    { OptionalNumber: float option
      Bool: bool }

type Options =
    { Name: string
      Count: int
      SubOptions: SubOptions
      OptSubOptions: SubOptions option }
```

We can bind this from an `IConfiguration` like this

```fsharp
let bindOptions config =

    // We can easily define reusable binders for the child sections because the api is composable
    let bindSubOptions =
        bind {
            let! optionalNumber = optValueOf Decode.float "MaybeDecimal"
            and! bool = valueOf Decode.bool "bool"

            return
                { OptionalNumber = optionalNumber
                  Bool = bool }
        }

    section
        "Options"
        (bind {
            let! name = value "Name"

            // We can use the `section` combinator with our bindSubOptions function to bind it from a section called "Sub"
            and! subOptions = section "Sub" bindSubOptions

            // We can also use the `optSection` combinator with our bindSubOptions funtions if that whole section of config is optional
            and! optSubOptions = optSection "OptSub" bindSubOptions

            return
                { Name = name
                  SubOptions = subOptions
                  OptSubOptions = optSubOptions }
         })
    |> Binder.eval config
```

Let's also pretend we're using the JSON configuration provider. We'll consider a few different example configs and see what `bindOptions` will return us.

### Correct config

In this scenario all fields are populated and correct in our `appsettings.json` file.

```json
{
    "Options": {
        "Name": "A name",
        "Count": 10,
        "Sub": {
            "MaybeDecimal": 1.0,
            "bool": true
        },
        "OptSub": {
            "MaybeDecimal": 2.0,
            "bool": false
        }
    }
}
```

This would bind fine and return the following value

```fsharp
Success(
    { Name = "A name"
      Count = 10
      SubOptions =
        { OptionalNumber = Some(1.0)
          Bool = true }
      OptSubOptions = Some(
        { OptionalNumber = Some(2.0)
          Bool = false }
        ) }
)
```

### Correct config with missing optional values

In this scenario some optional fields are missing and all others are present and correct in our `appsettings.json` file.

```json
{
    "Options": {
        "Name": "A name",
        "Count": 10,
        "Sub": {
            "bool": true
        }
    }
}
```

This would bind fine and return the following value

```fsharp
Success(
    { Name = "A name"
      Count = 10
      SubOptions = { OptionalNumber = None; Bool = true }
      OptSubOptions = None }
)
```

### Incorrect config

In this scenario some required values are missing and some others are the wrong type.

```json
{
    "Options": {
        "Count": "string"
    }
}
```

This would result in a `Failure` like so

```fsharp
Failure(
    [ "The key 'Name' does not exist at 'Options'."
      "Could not decode 'string' at path 'Options:Count' as type 'System.Int32'.",
      "The key 'Sub' does not exist at 'Options'." ]
)
```

Notice how in this case it returns as many errors as it can.

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
    section "Options" {
        // rest of binding code
    }
    |> Binder.eval config
    |> BindResult.defaultWith (
        String.concat System.Environment.NewLine
        >> failwith
    ) // Choosen to throw an exception containing all of the errors in the case of failure

let configureServices (ctx: WebHostBuilderContext) (services: IServiceCollection) =
    // Add the options type as a transient service if you want it to be rebound on each request,
    // i.e. to pick up config changes
    services
        .AddTransient<Symbolica.Infrastructure.GitHub.Options>(fun _ -> ctx.Configuration |> bindOptions)
    |> ignore
```

The drawbacks to this are that we don't have support for named options or reactive options monitoring in this form.

If enough people want to use this with DI, because named and monitored options are required, then it would be possible to provide an alternative instance of the `IOptionsFactory` which worked well with this library. Please open an issue if this is something you'd like.
