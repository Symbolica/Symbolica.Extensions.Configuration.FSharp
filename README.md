# Symbolica.Extensions.Configuration.FSharp


Provides a safe API for binding an FSharp type from the dotnet [`IConfiguration`](https://docs.microsoft.com/en-us/dotnet/api/microsoft.extensions.configuration.iconfiguration?view=dotnet-plat-ext-5.0) interface. It is an FSharp friendly alternative to using the reflection based [`ConfigurationBinder.Bind`](https://docs.microsoft.com/en-us/dotnet/api/microsoft.extensions.configuration.configurationbinder.bind?view=dotnet-plat-ext-5.0).

## Motivation

Out-of-the-box dotnet provides what it calls the ["the Options pattern"](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/configuration/options?view=aspnetcore-5.0) which it describes as:

> The options pattern uses classes to provide strongly typed access to groups of related settings.

Whilst this might be strongly typed in the sense that you're interacting with statically typed objects, the binding mechanism is not strictly type safe. It has a few notable problems, especially when working with it from FSharp.

1. It's a large source of `NullReferenceException`s because the binder will hapily set a value to `null` if it's missing in the underlying config. This means your FSharp type is a lie because it most likely purports to not allow `null` and so it's not so strongly typed after all. FSharp developers would rather model optional config with an `Option`.
1. It uses a reflection based API which means that if you want to use FSharp records to model your options they have to be annotated with `[<CLIMutable>]`.
1. It can struggle to bind more exotic types beyond primitives from the CLR and the most common collection types.

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

The primary means of using this library is through two computation expressions called `section` and `optSection`. These provide a declarative DSL for safely binding a type from an `IConfiguration` instance.

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
let bindOptions (config: IConfiguration) =
    section "Options" {
        let! name = value "Name"
        and! count = value "Count"

        and! subOptions =
            section "Sub" {
                let! optionalNumber = optValueOf Decode.float "MaybeDecimal"
                and! bool = valueOf Decode.bool "bool"

                return
                    { OptionalNumber = optionalNumber
                      Bool = bool }
            }

        and! optSubOptions =
            optSection "OptSub" {
                let! optionalNumber = optValueOf Decode.float "MaybeDecimal"
                and! bool = valueOf Decode.bool "bool"

                return
                    { OptionalNumber = optionalNumber
                      Bool = bool }
            }

        return
            { Name = name
              Count = count
              SubOptions = subOptions
              OptSubOptions = optSubOptions }
    }
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
