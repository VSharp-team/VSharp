namespace VSharp

open System.Collections.Generic
open System.Text

module Logger =
    open System

    // Tag for state transitions info logs
    let stateTraceTag = "StateTrace"
    let deserializationTraceTag = "Deserialization"
    let fuzzingInteractionTraceTag = "FuzzingInteraction"
    let defaultTag = ""

    let Quiet = 0
    let Critical = 1
    let Error = 2
    let Warning = 3
    let Info = 4
    let Trace = 5


    let mutable currentTextWriter = Console.Out
    let mutable writeTimestamps = true

    let private enabledTags = Dictionary([
        KeyValuePair(defaultTag, Error)
    ])

    let public configureWriter writer = currentTextWriter <- writer
    let public enableTimestamps value = writeTimestamps <- value
    let public enableTag tag level = enabledTags[tag] <- level
    let public changeVerbosity tag level = enabledTags[tag] <- level
    let public changeVerbosityTuple (tag, level) = enabledTags[tag] <- level
    let public disableTag tag = enabledTags.Remove tag |> ignore
    let public currentLogLevel tag =
        if enabledTags.ContainsKey tag then
            enabledTags[tag]
        else Quiet

    let LevelToString = function
        | 0 -> "Quiet"
        | 1 -> "Critical"
        | 2 -> "Error"
        | 3 -> "Warning"
        | 4 -> "Info"
        | 5 -> "Trace"
        | _ -> "Unknown"

    let private writeLineString vLevel tag (message : string) =
        let builder = StringBuilder $"[{LevelToString vLevel}] "
        let builder = if writeTimestamps then builder.Append $"[%A{DateTime.Now}] " else builder
        let builder = if tag <> defaultTag then builder.Append $"[{tag}] " else builder
        let builder = builder.Append message
        currentTextWriter.WriteLine(builder.ToString())
        currentTextWriter.Flush()

    let public writeLine (message : string) =
        currentTextWriter.WriteLine(message)
        currentTextWriter.Flush()

    let public printLogString vLevel (message : string) =
        if currentLogLevel defaultTag >= vLevel then
            writeLineString vLevel defaultTag message

    let public printLogLazyString vLevel (computeMessage : Func<string>) =
        if currentLogLevel defaultTag >= vLevel then
            computeMessage.Invoke() |> writeLineString vLevel ""

    let public printLogWithTag tag vLevel format =
        Printf.ksprintf (fun message -> if currentLogLevel tag >= vLevel then writeLineString vLevel tag message) format

    let public printLog vLevel format = printLogWithTag defaultTag vLevel format

    let public printLogLazyWithTag tag vLevel format (s : Lazy<_>) =
        if currentLogLevel tag >= vLevel then
            Printf.ksprintf (writeLineString vLevel tag) format (s.Force())

    let public printLogLazy vLevel format s = printLogLazyWithTag defaultTag vLevel format s

    let public error format = printLog Error format
    let public warning format = printLog Warning format
    let public info format = printLog Info format
    let public trace format = printLog Trace format

    let public errorWithTag tag format = printLogWithTag tag Error format
    let public warningWithTag tag format = printLogWithTag tag Warning format
    let public infoWithTag tag format = printLogWithTag tag Info format
    let public traceWithTag tag format = printLogWithTag tag Trace format
