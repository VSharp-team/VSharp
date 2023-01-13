namespace VSharp

open System.Text

module Logger =
    open System
    
    // Tag for state transitions info logs
    let stateTraceTag = "StateTrace"

    let Quiet = 0
    let Error = 1
    let Warning = 2
    let Info = 3
    let Trace = 4

    let mutable currentLogLevel = Error
    let mutable currentTextWriter = Console.Out
    let mutable writeTimestamps = true
    let mutable tagFilter : string -> bool = fun s -> s <> stateTraceTag
    
    let public configureWriter writer = currentTextWriter <- writer
    let public enableTimestamps value = writeTimestamps <- value
    let public setTagFilter filter = tagFilter <- filter

    let LevelToString = function
        | 1 -> "Error"
        | 2 -> "Warning"
        | 3 -> "Info"
        | 4 -> "Trace"
        | _ -> "Unknown"

    let writeLineString vLevel tag (message : string) =
        let builder = StringBuilder $"[{LevelToString vLevel}] "
        let builder = if writeTimestamps then builder.Append $"[%A{DateTime.Now}] " else builder
        let builder = if tag <> "" then builder.Append $"[{tag}] " else builder
        let builder = builder.Append message
        currentTextWriter.WriteLine(builder.ToString())
        currentTextWriter.Flush()
        
    let public printLogString vLevel (message : string) =
        writeLineString vLevel "" message
        
    let public printLogWithTag tag vLevel format =
        Printf.ksprintf (fun message -> if currentLogLevel >= vLevel && tagFilter tag then writeLineString vLevel tag message) format

    let public printLog vLevel format = printLogWithTag "" vLevel format
    
    let public printLogLazyWithTag tag vLevel format (s : Lazy<_>) =
        if currentLogLevel >= vLevel && tagFilter tag then
            Printf.ksprintf (writeLineString vLevel tag) format (s.Force())

    let public printLogLazy vLevel format (s : Lazy<_>) = printLogLazyWithTag "" vLevel format

    let public error format = printLog Error format
    let public warning format = printLog Warning format
    let public info format = printLog Info format
    let public trace format = printLog Trace format
    
    let public errorWithTag tag format = printLogWithTag tag Error format
    let public warningWithTag tag format = printLogWithTag tag Warning format
    let public infoWithTag tag format = printLogWithTag tag Info format
    let public traceWithTag tag format = printLogWithTag tag Trace format
