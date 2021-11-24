module Symbolica.Extensions.Configuration.FSharp.IntegrationTests

open Microsoft.Extensions.Configuration
open Swensen.Unquote
open Xunit

[<RequireQualifiedAccess>]
type LogLevel =
    | Info
    | Debug
    | Warning
    | Error

module LogLevel =
    let bind =
        Binder(
            fun (s: string) -> s.ToLowerInvariant()
            >> (function
            | "info" -> Success LogLevel.Info
            | "debug" -> Success LogLevel.Debug
            | "warning" -> Success LogLevel.Warning
            | "error" -> Success LogLevel.Error
            | _ -> Failure ValueError.invalidType<LogLevel>)
        )

type ILogSink =
    abstract Level: LogLevel option

type ConsoleSink =
    { Level: LogLevel option }
    interface ILogSink with
        member x.Level = x.Level

module ConsoleSink =
    let bind =
        bind {
            let! level = Bind.optValueAt "Level" LogLevel.bind
            return { Level = level }
        }

module Bytes =
    let bind<[<Measure>] 'u> (units: string) =
        Binder (fun (s: string) ->
            if s.EndsWith(units) then
                s.Substring(0, s.Length - 1) |> Success
            else
                Failure(ValueError.Message $"Expected bytes value to end with '{units}'."))
        |> Binder.extend Bind.int
        |> Binder.map LanguagePrimitives.Int32WithMeasure<'u>

[<Measure>]
type B

module B =
    let bind = Bytes.bind<B> "B"

[<Measure>]
type KB

module KB =
    let bind = Bytes.bind<KB> "KB"
    let toBytes (x: int<KB>) = x * 1000<B/KB>

[<Measure>]
type MB

module MB =
    let bind = Bytes.bind<MB> "MB"
    let toKiloBytes (x: int<MB>) = x * 1000<KB/MB>
    let toBytes = toKiloBytes >> KB.toBytes

type FileSink =
    { Level: LogLevel option
      MaxFileSize: int<B> }
    interface ILogSink with
        member x.Level = x.Level

module FileSink =
    let bind =
        bind {
            let! level = Bind.optValueAt "Level" LogLevel.bind

            and! maxFileSize =
                Bind.valueAt
                    "MaxFileSize"
                    (Bind.oneValueOf (
                        MB.bind |> Binder.map MB.toBytes
                        <|> (KB.bind |> Binder.map KB.toBytes)
                        <|> B.bind
                    ))

            return
                { Level = level
                  MaxFileSize = maxFileSize }
        }

type AppInsightsSink =
    { Level: LogLevel option
      InstrumentationKey: string }
    interface ILogSink with
        member x.Level = x.Level

module AppInsightsSink =
    let bind =
        bind {
            let! level = Bind.optValueAt "Level" LogLevel.bind
            and! instrumentationKey = Bind.valueAt "InstrumentationKey" Bind.string

            return
                { Level = level
                  InstrumentationKey = instrumentationKey }
        }

type LoggingOptions =
    { DefaultLevel: LogLevel
      Sinks: ILogSink list }

module LoggingOptions =

    let toILogSinkBinder b =
        b |> Binder.map (fun s -> s :> ILogSink)

    let bind =
        bind {
            let! defaultLevel = Bind.valueAt "DefaultLevel" LogLevel.bind

            and! sinks =
                Bind.optSection
                    "Sinks"
                    ([ Bind.optSection "Console" (ConsoleSink.bind |> toILogSinkBinder)
                       Bind.optSection "File" (FileSink.bind |> toILogSinkBinder)
                       Bind.optSection "AppInsights" (AppInsightsSink.bind |> toILogSinkBinder) ]
                     |> Bind.allOf
                     |> Binder.map (List.choose id))

            return
                { DefaultLevel = defaultLevel
                  Sinks = sinks |> Option.defaultValue [] }
        }

let mkConfig config =
    Bind.section "Logging" LoggingOptions.bind
    |> Binder.eval (
        ConfigurationBuilder()
            .AddInMemoryCollection(config |> Map.ofList)
            .Build()
    )
    |> BindResult.mapFailure (fun e -> e.ToString())

[<Fact>]
let ``should bind successfully without any sinks`` () =
    test
        <@ [ "Logging:DefaultLevel", "Warning" ] |> mkConfig = Success(
            { DefaultLevel = LogLevel.Warning
              Sinks = [] }
        ) @>

[<Fact>]
let ``should bind successfully with some sinks`` () =
    test
        <@ [ "Logging:DefaultLevel", "Warning"
             "Logging:Sinks:File:MaxFileSize", "1024B"
             "Logging:Sinks:AppInsights:Level", "Error"
             "Logging:Sinks:AppInsights:InstrumentationKey", "super-secret-key" ]
           |> mkConfig = Success(
            { DefaultLevel = LogLevel.Warning
              Sinks =
                [ { Level = None; MaxFileSize = 1024<B> }
                  { Level = Some LogLevel.Error
                    InstrumentationKey = "super-secret-key" } ] }
        ) @>

[<Fact>]
let ``should fail with pretty message if config empty`` () =
    test
        <@ [] |> mkConfig = Failure(
            """@'Logging':
  The key was not found."""
        ) @>

[<Fact>]
let ``should fail with pretty message if level and a sink is invalid`` () =
    test
        <@ [ "Logging:DefaultLevel", "NotALevel"
             "Logging:Sinks:File:MaxFileSize", "NotBytes" ]
           |> mkConfig = Failure(
            """@'Logging':
  all of these:
    @'DefaultLevel':
      Value: 'NotALevel'
      Error:
        Could not parse value as type 'LogLevel'.
    @'Sinks':
      all of these:
        @'File':
          all of these:
            @'MaxFileSize':
              Value: 'NotBytes'
              Error:
                one of these:
                  Expected bytes value to end with 'MB'.
                  Expected bytes value to end with 'KB'.
                  Expected bytes value to end with 'B'."""
        ) @>
